-module(lotus_ws_router).

-behaviour(gen_server).
-include("../include/lotus_ws.hrl").

-export([
	start/1,
	stop/1,
	start_link/1,
	init/1,
	handle_call/3,
	handle_cast/2, 
	handle_info/2, 
	terminate/2, 
	code_change/3
	]).

-export([ 
	find_route/2
	, compile_router/1
	, match/1
	, get_interceptors/0
	, get_recover/0
	, get_debug/0
	, get_router/0]).

-record(state, { router :: router() }).


start(Router) ->
	application:ensure_all_started(jwt),
	gen_server:start({local, ?MODULE}, ?MODULE, Router, []).

stop(Pid) ->
	gen_server:stop(Pid).

start_link(Router) ->
	application:ensure_all_started(jwt),
	gen_server:start_link({local, ?MODULE}, ?MODULE, Router, []).

map_route_to_record(R) ->
	Handler = maps:get(handler, R, undefined),
	Middlewares0 = maps:get(middlewares, R, []),
	Middlewares1 = prepare_middlewares(Middlewares0),
	#route{
		name = maps:get(name, R, Handler),
		path = maps:get(path, R, undefined),
		handler = Handler,
		middlewares = Middlewares1,
		roles = maps:get(roles, R, []),
		routes = map_routes_to_record(maps:get(routes, R, []))		
		}.

map_routes_to_record(Routes) -> map_routes_to_record(Routes, []).
map_routes_to_record([], Results) -> Results;
map_routes_to_record([H| T], Results) ->
	map_routes_to_record(T, Results++[map_route_to_record(H)]).

map_router_to_record(Router) when is_map(Router) ->
	Routes = maps:get(routes, Router, []),
	Debug = maps:get(debug, Router, false),
	Interceptors = maps:get(interceptors, Router, []),
	Recover = maps:get(recover, Router, undefined),
	#router{
		routes = map_routes_to_record(Routes)
		,debug = Debug
		, recover = Recover
		, interceptors = Interceptors
		}.

init(Router) when is_map(Router) ->
	logger:debug("init lotus_ws_router"),
	RouterRecord = map_router_to_record(Router),
	init(RouterRecord);

init(Router=#router{}) ->
	case compile_router(Router) of 
		NewRouter=#router{} -> 
			{ok, #state{ router = NewRouter } };
		Fail -> Fail		
	end.

%% api
call(Args) ->
	case whereis(?MODULE) of
		Pid when is_pid(Pid) ->
			gen_server:call(Pid, Args);
		_ ->
			logger:debug("[lotus_ws_router] pid not found"),
			{error, "pid not found"}
	end.

match(Path) ->
	call({match, Path}).

get_recover() ->
	call(get_recover).

get_interceptors() ->
	call(get_interceptors).

get_debug() ->
	call(get_debug).

get_router() ->
	call(get_router).

%% events

handle_call({match, Path}, _From, State) ->
	Router = State#state.router,
	Result = find_route(Path, Router#router.routes),
	{reply, Result, State};

handle_call(get_recover, _From, State) ->
	Router = State#state.router,	
	{reply, {ok, Router#router.recover}, State};

handle_call(get_interceptors, _From, State) ->
	Router = State#state.router,	
	{reply, {ok, Router#router.interceptors}, State};

handle_call(get_debug, _From, State) ->
	Router = State#state.router,	
	{reply, {ok, Router#router.debug}, State};

handle_call(get_router, _From, State) ->
	Router = State#state.router,	
	{reply, {ok, Router}, State};

handle_call(Event, _From, State) ->
	logger:debug("event ~p not handled", [Event]),
	{reply, {error, "event not found"}, State}.

%% private

prepare_middlewares(Middlewares) ->
	lists:map(
		fun(MD=#middleware{}) -> MD;			
			({Type, Module, Func}) -> #middleware { handler = {Type, Module, Func} };
			(Handler) when is_atom(Handler) -> #middleware { handler = Handler};
			(Handler) -> throw(lotus_ws_utils:format("Invalid middleware handler: ~p", [Handler])) 
		end, 
		Middlewares).

create_full_route(#route{} = Route) -> 	
	create_full_route([Route], "", [], []).

create_full_route([], _, FullRoutes, _) -> FullRoutes;

create_full_route(Routes, "/", FullRoutes, Middlewares) ->
	create_full_route(Routes, "", FullRoutes, Middlewares);

create_full_route([Route|NextRoutes], Path, FullRoutes, Middlewares) ->
	
	RouteMiddlewares = Route#route.middlewares,
	EmptyHandler = Route#route.handler =:= undefined,
	EmptyRouteChildren = length(Route#route.routes) =:= 0,
	RoutePath = Path++Route#route.path,
	CombinedMiddlewares = Middlewares ++ RouteMiddlewares,
	
	%?debugFmt("create_full_route ~p, subroutes = ~p", [RoutePath, length(Route#route.routes)]),
	
	RouteFullPath = if 
		EmptyHandler andalso  EmptyRouteChildren ->
			{badroute, {Route#route.name, io:format("Invalid route handler. Route: ~p", [RoutePath])}};
		not EmptyHandler ->
			%% ?debugFmt("new route ~p, middlewares = ~p, middlewares parent = ~p", [RoutePath, RouteMiddlewares, Middlewares]),
			Route#route { path = RoutePath, middlewares = CombinedMiddlewares };
		true ->
			noroute
	end,
	
	%% ?debugFmt("pass 1 ~p", [RouteFullPath]),
	
	NewRoutes = case RouteFullPath of
		{badroute, Reason} -> 
			{badroute, Reason};
		{noroute, Reason1} -> 
			{noroute, Reason1};
		noroute ->
			create_full_route(Route#route.routes, RoutePath, [], CombinedMiddlewares);
		RouteCompiled ->
			SubRoutes = create_full_route(Route#route.routes, RoutePath, [], CombinedMiddlewares),
			[RouteCompiled] ++ SubRoutes
	end,
	
	%?debugFmt("pass 2 ~p", [NewRoutes]),
	
	case NewRoutes of
		{badroute, Reason2} -> 
			{badroute, Reason2};
		{noroute, Reason3} -> 
			{noroute, Reason3};
		CRoute ->
			%?debugFmt("next route to ~p, routes = ~p", [Path, length(NextRoutes)]),
			% no route, go to next route
			create_full_route(NextRoutes, Path, FullRoutes++CRoute, Middlewares)
	end.


compile_router(Router) when is_map(Router) ->
	RouterRecord = map_router_to_record(Router),
	compile_router(RouterRecord);
compile_router(#router{} = Router) -> compile_router(Router, []).

compile_router(#router{routes=[]} = Router, FullRoutes) -> 
	Router#router{ routes = FullRoutes };

% [] | {badroute, ""} | {noroute, ""}
compile_router(#router{routes=[H|T]} = Router, FullRoutes) ->
	NewFullRoutes = create_full_route(H),
	compile_router(Router#router{routes=T}, FullRoutes ++ NewFullRoutes).


find_route(_, []) -> nomatch;

find_route(Path, [Route|T]) -> 
	
	%logger:info("check ~p =:= ~p", [Route#route.path, Path]),
	
	case extract_path_params(Route#route.path, Path) of
		nomatch ->
			%logger:debug("route ~p not math with ~p", [Path, Route#route.path]),
			%?debugFmt("route ~p not math with ~p", [Path, Route#route.path]),
			find_route(Path, T);
		{CompiledPath, Params} ->
			Route#route { params = Params, compiled_path = CompiledPath }
	end.

extract_path_params(Path, ReqPath) ->
	[_|PathParts] = lists:map(fun(S) -> "/"++S end, string:split(Path, "/", all)),
	[_|ReqPathParts] = lists:map(fun(S) -> "/"++S end, string:split(ReqPath, "/", all)),
	%logger:debug("PathParts = ~p, ReqPathParts = ~p", [PathParts, ReqPathParts]),
	%?debugFmt("PathParts = ~p, ReqPathParts = ~p", [PathParts, ReqPathParts]),
	extract_path_params(PathParts, ReqPathParts, "", #{}).

extract_path_params([], [], CompiledPath, Params) -> {CompiledPath, Params};
extract_path_params([], [_|_], _, _) -> nomatch;

% same path parts
extract_path_params([SamePathPart|PathParts], [SamePathPart|ReqPathParts], CompiledPath, Params) ->
	extract_path_params(PathParts, ReqPathParts, CompiledPath ++ SamePathPart, Params);

extract_path_params([PathPart|PathParts], [ReqPathPart|ReqPathParts], CompiledPath, Params) ->
	
	% check path has regexp = /:id([0-9])
	case re:run(PathPart, "[/][:][0-1a-zA-Z_]{1,}[(].{0,}[)]") of
		nomatch -> nomatch;
		_ ->
			
			% extract path param regexp
			{match, [{StartRegexIdx, EndRegexIdx}]} = re:run(PathPart, "[(].{0,}[)]"),
			%?debugFmt("Regexp = ~p, ", [{StartRegexIdx, EndRegexIdx}]),
			Regexp = string:substr(PathPart, StartRegexIdx+2, EndRegexIdx-2),
			
			
			%?debugFmt("Regexp = ~p, ", [Regexp]),
			
			% extract path param name
			{match, [{StartParamNameIdx, EndParamNameIdx}]} = re:run(PathPart, "[/][:][0-1a-zA-Z_]{1,}[(]"),
			ParamName = string:substr(PathPart, StartParamNameIdx+3, EndParamNameIdx-3),
			
			% create route action/fn name. eg.: /api/customer/:id([1-9]) -> /api/customer/:id
			NewPathPart = string:join([CompiledPath, "/:",  ParamName], ""),
			
			% extract path param value
			ReqPathParm = string:substr(ReqPathPart, 2, length(ReqPathPart)),
			
			%?debugFmt("Param ~p=~p, -> ~p ", [ParamName, ReqPathParm, NewPathPart]),
			
			PathParam = validate_path_param(Regexp, ReqPathParm),
			
			case PathParam of
				nomatch -> nomatch;
				_ ->
					extract_path_params(PathParts, ReqPathParts, NewPathPart, maps:put(ParamName, PathParam, Params))
			end
	end.

validate_path_param(Regexp, Param) ->
	case Regexp of
		"" -> Param;
		_ ->
			% validate path param
			case re:run(Param, Regexp) of 
				nomatch -> nomatch;
				_ ->
					% check regexp is numeric, so parse
					case re:run(Regexp, "^[\[][0-9][-][0-9][\]]$") of
						nomatch -> list_to_binary(Param);
						_ -> list_to_integer(Param)
					end
			end
	end.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.