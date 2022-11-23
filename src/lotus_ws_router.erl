-module(lotus_ws_router).

-behaviour(gen_server).
-include("include/lotus_ws.hrl").

-export([
	start/1,
	start_link/1,
	init/1,
	handle_call/3
]).

-export([ find_route/2
				, compile_router/1
				, search/1
				, get_authenticator/0
				, get_middlewares/0]).

-record(state, { router :: router() }).


start(Router) ->
	application:ensure_all_started(jwt),
	gen_server:start({global, ?MODULE}, ?MODULE, Router, []).

start_link(Router) ->
	application:ensure_all_started(jwt),
	gen_server:start_link({global, ?MODULE}, ?MODULE, Router, []).

init(Router=#router{}) ->
	case compile_router(Router) of 
		NewRouter=#router{} -> {ok, #state{ router = NewRouter } };
		Fail -> Fail		
	end.

%% api

search(Path) ->
	gen_server:call({global, ?MODULE}, {search, Path}).

get_authenticator() ->
	gen_server:call({global, ?MODULE}, get_authenticator).

get_middlewares() ->
	gen_server:call({global, ?MODULE}, get_middlewares).

%% events

handle_call({search, Path}, _From, State) ->
	Router = State#state.router,
	Result = find_route(Path, Router#router.routes),
	{reply, Result, State};

handle_call(get_authenticator, _From, State) ->
	Router = State#state.router,
	{reply, Router#router.authenticator, State};

handle_call(get_middlewares, _From, State) ->
	Router = State#state.router,
	{reply, Router#router.middlewares, State};

handle_call(Event, _From, State) ->
	lager:info("event ~p not handled", [Event]),
	{reply, {error, "event not found"}, State}.

%% private

default_routes() -> [
	#route {
			name = bearer_token
		, path = "/login"
		, handler = dummy_handler
		, middlewares = [
			lotus_ws_body_json_parse_mw
		 	, lotus_ws_login_mw
		 	, lotus_ws_autenticator_mw
		 	, lotus_ws_bearer_token_mw
		]}
].

combine_middewares(#middlewares{} = LastMiddlewares, CurrMiddlewares) when is_list(CurrMiddlewares) ->
	combine_middewares(LastMiddlewares, #middlewares{ handlers = CurrMiddlewares });

combine_middewares(#middlewares{} = LastMiddlewares, #middlewares{} = CurrMiddlewares) ->
	Bypass = LastMiddlewares#middlewares.bypass++CurrMiddlewares#middlewares.bypass,
	Values = LastMiddlewares#middlewares.values++CurrMiddlewares#middlewares.values,
	Handlers = LastMiddlewares#middlewares.handlers++CurrMiddlewares#middlewares.handlers,
	#middlewares{ 
		bypass = Bypass,
		values = lotus_ws_utils:list_in_list_not(fun(X, Y) -> X =:= Y end, Values, Bypass),
		handlers = lotus_ws_utils:list_in_list_not(fun(X, Y) -> X =:= Y end, Handlers, Bypass)
	}.

create_full_route(#route{} = Route) -> 	
	create_full_route([Route], "", [], #middlewares{}).

create_full_route([], _, FullRoutes, _) -> FullRoutes;

create_full_route(Routes, "/", FullRoutes, #middlewares{} = Middlewares) ->
	create_full_route(Routes, "", FullRoutes, Middlewares);

create_full_route([R|NextRoutes], Path, FullRoutes, #middlewares{} = Middlewares) ->

	Route = add_defaults_routes(R),	

	%?debugFmt("Route with defauls = ~p", [Route]),

	RouteMiddlewares = Route#route.middlewares,
	InvalidHandler = Route#route.fn =/= undefined andalso Route#route.handler =/= undefined,
	EmptyHandler = Route#route.fn =:= undefined andalso Route#route.handler =:= undefined,
	EmptyRouteChildren = length(Route#route.routes) =:= 0,
	RoutePath = Path++Route#route.path,

	%?debugFmt("create_full_route ~p, subroutes = ~p", [RoutePath, length(Route#route.routes)]),

	RouteFullPath = if 
		EmptyHandler andalso  EmptyRouteChildren ->
			{badroute, {Route#route.name, "invalid handler, use fn or handler"}};
		InvalidHandler ->
			{badroute, {Route#route.name, "invalid handler, use fn or handler"}};
		not EmptyHandler ->
			%?debugFmt("new route ~p, middlewares = ~p, middlewares parent = ~p", [RoutePath, RouteMiddlewares, Middlewares]),
			Route#route { path = RoutePath, middlewares = combine_middewares(Middlewares, RouteMiddlewares) };
		true ->
			noroute
	end,

	%?debugFmt("pass 1 ~p", [RouteFullPath]),

	NewRoutes = case RouteFullPath of
		{badroute, Reason} -> 
			{badroute, Reason};
		{noroute, Reason1} -> 
			{noroute, Reason1};
		noroute ->
			create_full_route(Route#route.routes, RoutePath, [], combine_middewares(Middlewares, RouteMiddlewares));
		RouteCompiled ->
			SubRoutes = create_full_route(Route#route.routes, RoutePath, [], combine_middewares(Middlewares, RouteMiddlewares)),
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
			create_full_route(NextRoutes, Path, FullRoutes++CRoute, Middlewares)
	end.


find_default_route([#route{ name = Name}=H|_], Name) -> H;
find_default_route([_|T], Name) -> find_default_route(T, Name);
find_default_route([], Name) -> {noroute, "no default route found: " ++ Name}.

find_defaults_routes([], _, [{noroute, Msg}|_]) -> {noroute, Msg};
find_defaults_routes([], _, Results) -> Results;
find_defaults_routes([H|T], DefsRoutes, Results) ->
	find_defaults_routes(T, DefsRoutes, [find_default_route(DefsRoutes, H) | Results]).

add_defaults_routes(#route{ defaults = []} = Route) -> Route;
add_defaults_routes(#route{ defaults = Defs, routes = Routes} = Route) -> 
	DefRoutes = find_defaults_routes(Defs, default_routes(), []),
	Route#route { routes = DefRoutes ++ Routes }.

compile_router(#router{} = Router) -> compile_router(Router, []).

compile_router(#router{routes=[]} = Router, FullRoutes) -> Router#router{ routes = FullRoutes };

% [] | {badroute, ""} | {noroute, ""}
compile_router(#router{routes=[H|T]} = Router, FullRoutes) ->
	NewFullRoutes = create_full_route(H),
	compile_router(Router#router{routes=T}, FullRoutes ++ NewFullRoutes).


find_route(_, []) -> nomatch;

find_route(Path, [Route|T]) -> 
	case extract_path_params(Route#route.path, Path) of
		nomatch ->
			%lager:info("route ~p not math with ~p", [Path, Route#route.path]),
			%?debugFmt("route ~p not math with ~p", [Path, Route#route.path]),
			find_route(Path, T);
		{CompiledPath, Params} ->
			Route#route { params = Params, compiled_path = CompiledPath }
	end.

extract_path_params(Path, ReqPath) ->
	[_|PathParts] = lists:map(fun(S) -> "/"++S end, string:split(Path, "/", all)),
	[_|ReqPathParts] = lists:map(fun(S) -> "/"++S end, string:split(ReqPath, "/", all)),
	%lager:info("PathParts = ~p, ReqPathParts = ~p", [PathParts, ReqPathParts]),
	%?debugFmt("PathParts = ~p, ReqPathParts = ~p", [PathParts, ReqPathParts]),
	extract_path_params(PathParts, ReqPathParts, "", #{}).

extract_path_params([], _, CompiledPath, Params) -> {CompiledPath, Params};

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