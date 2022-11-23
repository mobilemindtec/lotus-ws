-module(lotus_ws_handler).

-behaviour(cowboy_handler).

-include("include/lotus_ws.hrl").

-export([
	init/2
]).

%% standalone routes
-export([
	run_ctx/1
]).

-spec init(Req, any()) -> Req when Req :: cowboy_req:req().

init(Req, State) ->
	ctx = create_ctx(Req),
	Newctx = run_ctx(ctx),
	{ok, lotus_ws_http_utils:cowboy_response(Newctx, Req), State}.


create_ctx(#{path := Path, method := Method, headers := Headers} = Req) ->	
	Authenticator = lotus_ws_router:get_authenticator(),
	#ctx {
		authenticator = Authenticator,
		req = #req{ 
			path = lotus_ws_utils:binary_to_string(Path), 
			body = lotus_ws_http_utils:get_cowboy_req_body(Req), 
			req = Req, 
			method = Method,
			queries = cowboy_req:parse_qs(Req),
			headers = Headers}}.

-spec run_ctx(Ctx) -> Ctx when Ctx :: ctx().

run_ctx(#ctx{ req = Req } = Ctx) ->	
	%lager:info("Body = ~p", [Body]),

	case lotus_ws_router:search(Req#req.path) of
		Route=#route{} ->

			lager:info("route found: ~p", [Route#route.path]),
			
			Middlewares = get_route_middlewares(Route),
			RouteCtx = Ctx#ctx{ route = Route
												, req = Req#req{ params = Route#route.params } },	
			EnterCtx = dispatch_middwares(Middlewares, RouteCtx, enter),

			case EnterCtx of
				#ctx{ error = true } -> EnterCtx;
				_ ->
					try dispatch(Req#req.method, Route, EnterCtx) of
						Response  -> 
							RespCtx = lotus_ws_http_utils:handle_resp(EnterCtx, Response),
							NextAction = case RespCtx of
								#ctx{ error = true } -> error;
								_ -> leave					
							end,
							dispatch_middwares(Middlewares, RespCtx, NextAction)
						catch
							Throw -> 
								lager:info("Throw = ~p", [Throw]),
								lotus_ws_http_utils:new_ctx_error(EnterCtx, fmt:sprint("$reason", [{reason, Throw}]))								
					end
			end;

		NoRoute ->
			lager:info("no route found = ~p", NoRoute),
			lotus_ws_http_utils:handle_resp(ctx, lotus_ws_http_utils:not_found(Req#req.headers))
	end.

dispatch(<<"GET">>, #route{} = Route, #ctx { req = #req{ headers = Headers } = Req } = Ctx) ->
	Module = Route#route.handler,
	case find_fn(Module, Route#route.fn, get) of
		{Fn, ArityCount } ->
			dispatch_apply_without_body(Module, Fn, ArityCount, [Route#route.compiled_path, Ctx, Req], Headers);
		false ->
			lotus_ws_http_utils:server_error(Headers, "action get not found")
	end;
dispatch(<<"POST">>, #route{} = Route, #ctx {} = Ctx) -> dispatch(post, Route, Ctx);
dispatch(<<"DELETE">>, #route{} = Route, #ctx {} = Ctx) -> dispatch(delete, Route, Ctx);
dispatch(<<"PUT">>, #route{} = Route, #ctx {} = Ctx) -> dispatch(put, Route, Ctx);
dispatch(<<"PATCH">>, #route{} = Route, #ctx {} = Ctx) -> dispatch(patch, Route, Ctx);
dispatch(Method, #route{} = Route, #ctx { req = #req{ body = Body, headers = Headers } = Req } = Ctx) ->
	Module = Route#route.handler,
	case find_fn(Module, Route#route.fn, Method) of
		{Fn, ArityCount } ->
			dispatch_apply_with_body(Module, Fn, ArityCount, [Route#route.compiled_path, Ctx, Req, Body], Headers);
		false ->
			lotus_ws_http_utils:server_error(Headers, fmt:sprint("action to $method not found", [{method, Method}]))
	end.	


dispatch_middware(undefined, _, undefined, Ctx) -> Ctx;
dispatch_middware(Module, enter, undefined, Ctx) ->
	dispatch_middware_apply(lotus_ws_utils:find_module_fn(Module, enter), Module, enter, Ctx);
dispatch_middware(Module, leave, undefined, Ctx) ->
	dispatch_middware_apply(lotus_ws_utils:find_module_fn(Module, leave), Module, leave, Ctx);
dispatch_middware(Module, error, undefined, Ctx) ->
	dispatch_middware_apply(lotus_ws_utils:find_module_fn(Module, error), Module, error, Ctx);
dispatch_middware(undefined, _, Fn, Ctx) -> apply(Fn, [Ctx]).

dispatch_middware_apply(false, _, _, Ctx) -> Ctx;
dispatch_middware_apply(_, Module, Fn, Ctx) -> apply(Module, Fn, [Ctx]).

dispatch_middwares([], Ctx, _) -> Ctx;
dispatch_middwares([#middleware{} = Middleware|Middlewares], #ctx{} = Ctx, Step) ->	
	Module = Middleware#middleware.handler,
	Result = case Step of
		enter ->
			dispatch_middware(Module, enter, Middleware#middleware.enter, Ctx);
		leave ->
			dispatch_middware(Module, leave, Middleware#middleware.leave, Ctx);
		error ->
			dispatch_middware(Module, error, Middleware#middleware.error, Ctx)
	end,

	NewCtx = lotus_ws_http_utils:handle_resp(Ctx, Result),

	case NewCtx of
		#ctx{ error = true } ->
			dispatch_middwares(Middlewares, NewCtx, error);
		_ ->
			dispatch_middwares(Middlewares, NewCtx, Step)
	end.


get_route_middlewares(#route { middlewares = Handlers }  = Route) when is_list(Handlers)->
	get_route_middlewares(Route#route{ middlewares = #middlewares { handlers = Handlers } });

get_route_middlewares(#route { middlewares = #middlewares{ values = Values, handlers = Handlers } }) ->
	Middlewares = lotus_ws_router:get_middlewares(),
	CustomHandlers = lists:map(fun(H) -> #middleware{ handler = H } end, Handlers),
	lotus_ws_utils:list_in_list(fun(X, Y) -> X#middleware.name =:= Y end, Middlewares, Values) ++ CustomHandlers.

dispatch_apply_without_body(undefined, Fn, 1, [Arg1|_], _) -> apply(Fn, [Arg1]);
dispatch_apply_without_body(Module, Fn, 1, [Arg1|_], _) -> apply(Module, Fn, [Arg1]);
dispatch_apply_without_body(undefined, Fn, 2, [Arg1, Arg2|_], _) -> apply(Fn, [Arg1, Arg2]);
dispatch_apply_without_body(Module, Fn, 2, [Arg1, Arg2|_], _) -> apply(Module, Fn, [Arg1, Arg2]);
dispatch_apply_without_body(undefined, Fn, 3, [Arg1, Arg2, Arg3|_], _) -> apply(Fn, [Arg1, Arg2, Arg3]);
dispatch_apply_without_body(Module, Fn, 3, [Arg1, Arg2, Arg3|_], _) -> apply(Module, Fn, [Arg1, Arg2, Arg3]);
dispatch_apply_without_body(_, _, Arity, _, Headers) ->
	lotus_ws_http_utils:server_error(Headers, lotus_ws_utils:sprint("action get expects min 1 max 3 args, but found $count args", [{count, Arity}])).

dispatch_apply_with_body(undefined, Fn, 1, [Arg1|_], _) -> apply(Fn, [Arg1]);
dispatch_apply_with_body(Module, Fn, 1, [Arg1|_], _) -> apply(Module, Fn, [Arg1]);
dispatch_apply_with_body(undefined, Fn, 2, [Arg1, Arg2|_], _) -> apply(Fn, [Arg1, Arg2]);
dispatch_apply_with_body(Module, Fn, 2, [Arg1, Arg2|_], _) -> apply(Module, Fn, [Arg1, Arg2]);
dispatch_apply_with_body(undefined, Fn, 3, [Arg1, Arg2, Arg3|_], _) -> apply(Fn, [Arg1, Arg2, Arg3]);
dispatch_apply_with_body(Module, Fn, 3, [Arg1, Arg2, Arg3|_], _) -> apply(Module, Fn, [Arg1, Arg2, Arg3]);
dispatch_apply_with_body(undefined, Fn, 4, [Arg1, Arg2, Arg3, Arg4|_], _) -> apply(Fn, [Arg1, Arg2, Arg3, Arg4]);
dispatch_apply_with_body(Module, Fn, 4, [Arg1, Arg2, Arg3, Arg4|_], _) -> apply(Module, Fn, [Arg1, Arg2, Arg3, Arg4]);
dispatch_apply_with_body(_, _, Arity, _, Headers) ->
	lotus_ws_http_utils:server_error(Headers, lotus_ws_utils:sprint("action post expects min 1 max 4 args, but found $count args", [{count, Arity}])).

find_fn(undefined, undefined, _) -> false;
find_fn(undefined, Fun, _) -> find_fn_arity(erlang:fun_info(Fun, arity), Fun);
find_fn(Module, undefined, FunName) ->  find_fn_arity(lotus_ws_utils:find_module_fn(Module, FunName), FunName).

find_fn_arity({_, ArityCount}, Fn) -> 	{Fn, ArityCount};
find_fn_arity(false, _) -> false.

	