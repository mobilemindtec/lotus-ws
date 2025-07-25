-module(lotus_ws_handler).

-behaviour(cowboy_handler).

-include("../include/lotus_ws.hrl").

-export([
	init/2
	]).

%% standalone routes
-export([
	handle/1
	]).

-spec init(Req, any()) -> Req when Req :: cowboy_req:req().

init(Req, State) ->
	Ctx = create_ctx(Req),
	Newctx = handle(Ctx),
	{ok, lotus_ws_http_utils:cowboy_resp(Newctx, Req), State}.


create_ctx(#{path := Path, method := Method, headers := Headers} = Req) ->	
	M1 = lotus_ws_utils:binary_to_string(Method),
	M2 = list_to_atom(string:lowercase(M1)),	
	#ctx {
		req = #req{ 
			path = lotus_ws_utils:binary_to_string(Path), 
			body = lotus_ws_http_utils:get_cowboy_req_body(Req), 
			req = Req, 
			method = M2,
			queries = cowboy_req:parse_qs(Req),			
			headers = Headers}}.



%%
%% Handle request
%%
-spec handle(Req) -> Req when Req :: req() |  Ctx :: req().
handle(Req=#req{}) ->	
	handle(#ctx { req = Req });

handle(Ctx = #ctx{ req = Req }) ->	
	
	io:fwrite("~s ~s~n", [Req#req.method, Req#req.path]),
	
	{ok, Debug} = lotus_ws_router:get_debug(),
	
	case lotus_ws_router:match(Req#req.path) of
		Route=#route{} ->
			
			{ok, #router{
					interceptors = Interceptors
					, recover = Recover					
					}} = lotus_ws_router:get_router(),
			
			Middlewares = Route#route.middlewares,
			
			RouteCtx = Ctx#ctx{ 				
					req = Req#req{ 
						params = Route#route.params
						, route = Route } },
			
			NewCtx = try
				EnterCtx = dispatch_middwares(Middlewares, RouteCtx, enter),
				case EnterCtx of
					#ctx{ resp = undefined } ->
						Response = dispatch_request(Req#req.method, Route, EnterCtx),
						lotus_ws_http_utils:handle_resp(EnterCtx, Response);
					_ -> EnterCtx
				end
			catch
				error:function_clause ->
					if 
						Debug ->
							logger:error("Route handler error: function_clause");
						true -> ok
					end,
					lotus_ws_http_utils:handle_resp(Ctx, lotus_ws_http_utils:not_found(Req#req.headers));
				
				Error ->
					case dispatch_recover(Recover, Req, Error) of
						#req{} ->
							lotus_ws_http_utils:handle_resp(Ctx, lotus_ws_http_utils:server_error(Req#req.headers, Error));
						Recovered ->
							lotus_ws_http_utils:handle_resp(Ctx, Recovered)
					end			
			end,
			NewCtx0 = dispatch_middwares(Middlewares, NewCtx, leave),
			dispatch_interceptors(Interceptors, NewCtx0);					
		NoRoute ->
			if 
				Debug ->
					logger:debug("404 ~s ~s State: ~p~n", [Req#req.method, Req#req.path, NoRoute]);
				true ->
					io:fwrite("404 ~s ~s~n", [Req#req.method, Req#req.path])
			end,
			lotus_ws_http_utils:handle_resp(Ctx, lotus_ws_http_utils:not_found(Req#req.headers))
	end.

dispatch_interceptors([], Ctx) -> Ctx;
dispatch_interceptors([H|T], Ctx=#ctx{ req = Req, resp = Resp }) -> 
	StatusCode = Resp#resp.status,
	NewResp = case H of
		{Status, Mod, Fun} when Status =:= StatusCode -> 
			case lotus_ws_utils:find_module_fn(Mod, Fun) of
				{_, 2} -> Mod:Fun([Req, Resp]);
				_ -> 
					logger:warning("Wrong intercept arity. Expected arity 2: ~p:~p", [Mod, Fun]),
					Resp
			end;
		Mod when is_atom(Mod) -> 
			case lotus_ws_utils:find_module_fn(Mod, intercept) of
				{_, 3} -> 
					try 
						Mod:intercept([StatusCode, Req, Resp])
					catch 
						error:function_clause -> Resp
					end;
				_ -> 
					logger:warning("Wrong intercept arity. Expected arity 2: ~p:~p", [Mod, intercept]),
					Resp
			end;
		Fun when is_function(Fun, 3) ->
			try 
				Fun(StatusCode, Req, Resp)
			catch 
				error:function_clause -> Resp
			end;
		Other ->
			logger:warning("Wrong interceptor: ~p", [Other]),
			Resp
	end,
	NewCtx = lotus_ws_http_utils:handle_resp(Ctx, NewResp),
	dispatch_interceptors(T, NewCtx).


dispatch_recover(undefined, Req=#req{}, _) -> Req;
dispatch_recover({Mod, Fun}, Req=#req{}, Error) when is_atom(Mod), is_atom(Fun) -> 
	case lotus_ws_utils:find_module_fn(Mod, Fun) of
		{_, 2} -> Mod:Fun(Req, Error);
		_ -> 
			logger:warning("Wrong recover arity. Expected arity 2: ~p:~p", [Mod, Fun]),
			Req
	end;
dispatch_recover(Mod, Req=#req{}, Error) when is_atom(Mod) ->
	case lotus_ws_utils:find_module_fn(Mod, recover) of
		{_, 2} -> Mod:recover([Req, Error]);
		false ->
			logger:warning("Recover func not found on module ~p", [Mod]),
			Req;
		{_, _} -> 
			logger:warning("Wrong recover arity. Expected arity 2: ~p:~p", [Mod, recover]),
			Req
	end;
dispatch_recover(Fun, Req=#req{}, Error) when is_function(Fun, 2) -> 
	Fun(Req, Error);
dispatch_recover(Other, Req=#req{}, _) -> 
	logger:warning("Invalid recover handler: ~p", [Other]),
	Req.

find_http_handler(Verb, #route { handler  = Handler }) ->
	
	case Handler of
		{Module, Func} ->  % Module:Func
			
			%% func(verb, path, req) | func(path, req) | func(req)
			case lotus_ws_utils:find_module_fn(Module, Func) of
				{_, Arity} when Arity >= 1, Arity =< 3 -> 					
					{mod, Arity, {Module, Func}};
				_ -> 
					logger:warning("Wrong http handler arity. Expected arity 1, 2 or 3. Module: ~p:~p", [Module, Func]),
					undefined
			end;
		
		Module when is_atom(Module) -> % module:verb
			
			case lotus_ws_utils:find_module_fn(Module, Verb) of
				{_, Arity} when Arity >= 1, Arity =< 2 -> 
					{mod, Arity, {Module, Verb}};
				_ -> 
					logger:warning("Wrong http handler arity. Expected arity 1 or 2. Module: ~p:~p", [Module, Verb]),
					undefined
			end;
		
		Func when is_function(Func) -> 
			
			if
				%% func(verb, req) | func(req)
				is_function(Func, 1) -> {func, 1, Func};
				is_function(Func, 2) -> {func, 2, Func};
				true -> 
					logger:warning("Wrong http handler arity. Expected arity 1 or 2. Func: ~p", [Func]),
					undefined
			end;
		
		What -> 
			{error, io:format("Wrong handler ~p", [What])}
	end.	

dispatch_middwares([], Ctx=#ctx{}, _) -> Ctx;
dispatch_middwares([#middleware{} = Middleware|Middlewares], #ctx{req = Req, resp = Resp} = Ctx, Step) ->	
	
	ExpectedArity = case Step of enter -> 1; leave -> 2 end,
	
	MD = case Middleware#middleware.handler of
		
		undefined -> 
			
			% check middleware enter and leave function
			case Step of
				enter ->
					case Middleware#middleware.enter of
						undefined -> undefined;
						Func0 when is_function(Func0, 1) -> {Step, func, Func0};
						_ -> 
							logger:warning("Wrong middleware arity. Expected arity 1: ~p, ~p", [enter, Middleware#middleware.enter]),
							undefined
					
					end;
				leave ->
					case Middleware#middleware.leave of
						undefined -> undefined;
						Func0 when is_function(Func0, 2) -> {Step, func, Func0};
						_ -> 
							logger:warning("Wrong middleware arity. Expected arity 2: ~p, ~p", [leave, Middleware#middleware.leave]),
							undefined
					end
			end;
		
		{Mod, Func} -> 
			
			%% fun(req) -> req | resp
			%% fun(req, resp) -> resp
			
			case lotus_ws_utils:find_module_fn(Mod, Func) of
				{_, FnArity} when FnArity =:= ExpectedArity -> {Step, mod, {Mod, Func}};
				_ -> 
					logger:warning("Wrong middleware arity. Expected arity ~p: ~p:~p", [ExpectedArity , Mod, Func]),
					undefined
			end;
		
		Mod when is_atom(Mod) -> 
			%% Mod:(enter|leave)
			
			case lotus_ws_utils:find_module_fn(Mod, Step) of
				{_, FnArity} when FnArity =:= ExpectedArity -> {Step, mod, {Mod, Step}};
				false -> undefined;
				_ -> 
					logger:warning("Wrong middleware arity. Expected arity ~p: ~p:~p", [ExpectedArity, Mod, Step]),
					undefined
			end;
		
		Func when Step =:= enter andalso is_function(Func, 1) -> {enter, func, Func};
		
		Func when Step =:= leave andalso is_function(Func, 2) -> {leave, func, Func};
		
		Other ->
			logger:warning("Wrong middleware: ~p", [Other]),
			undefined
	end,
	
	Result = try
		if
			Step =:= enter ->
				
				case MD of
					undefined -> undefined; % no middleware
					{enter, mod, {Mod1, Func1}} -> Mod1:Func1(Req);
					{enter, func, Func2} -> Func2(Req)
				end;
			
			Step =:= leave ->
				
				case MD of
					undefined -> undefined; % no middleware
					{leave, mod, {Mod3, Func3}} -> Mod3:Func3(Req, Resp);
					{leave, func, Func4} -> Func4(Req, Resp)
				end;
			
			true ->
				logger:warning("Wrong middleware step: ~p", [Step]),
				undefined
		end
	catch 
		error:function_clause -> 
			logger:debug("Error function_clause"),
			undefined;
		Error -> throw(Error) 
	end,
	
	NewCtx = case {Step, Result} of
		{_, undefined} -> Ctx;
		{enter, NewReq=#req{}} ->
			Ctx#ctx { req = NewReq };
		{enter, NewResp=#resp{}} ->
			Ctx#ctx { resp = NewResp };
		{leave, NewResp=#resp{}} ->
			Ctx#ctx { resp = NewResp };
		OtherResult -> 
			logger:debug("OtherResult = ~p", [OtherResult]),
			lotus_ws_http_utils:handle_resp(Ctx, OtherResult)
	end,
	
	dispatch_middwares(Middlewares, NewCtx, Step).

dispatch_request(Verb, Route = #route{}, #ctx {req = #req{ headers = Headers } = Req}) when is_atom(Verb) -> 
	Handler = find_http_handler(Verb, Route),	
	RoutePath = Route#route.compiled_path,
	Result = case Handler of
		{error, Reason} ->
			lotus_ws_http_utils:server_error(Headers, Reason);
		undefined ->
			lotus_ws_http_utils:not_found(Headers);
		{mod, 1, {Mod, Func}} -> Mod:Func(Req);
		{mod, 2, {Mod, Func}} -> Mod:Func(RoutePath, Req);
		{mod, 3, {Mod, Func}} -> Mod:Func(Verb, RoutePath, Req);
		{func, 1, Func} -> Func(Req);
		{func, 2, Func} -> Func(Verb, Req)
	end,
	%logger:debug("handle Result = ~p", [Result]),
	Result.