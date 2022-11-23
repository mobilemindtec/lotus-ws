-module(lotus_ws_router_test).

-include_lib("eunit/include/eunit.hrl").

-include("include/lotus_ws.hrl").

-compile(export_all).

% rebar3 eunit --module=lotus_ws_router_test --sys_config=test/test.config 

route_sub1_test() ->
	Router = #router {		
		routes = [#route{path = "/", 
								 	 	 handler = api_handler}]
	},
	#router{ routes = [Route] } = lotus_ws_router:compile_router(Router),
	?assert(Route#route.path =:= "/").

route_sub2_test() ->	

	Router = #router {
		routes = [#route{path = "/", 
						 routes = [#route { path = "/api", handler = test }],
					 	 handler = test}]
	},
	#router{ routes = Results } = lotus_ws_router:compile_router(Router),
	%?debugFmt("route paths = ~p", [lists:map(fun(#route{path = P}) -> P end, Results)]),
	[Route1, Route2] = Results,
	?assert(Route1#route.path =:= "/"),
	?assert(Route2#route.path =:= "/api").

route_sub3_test() ->	

	Router = #router {
		routes = [#route{path = "/", 
										 routes = [#route { path = "/api", handler = test, routes = [#route { path = "/customer", handler = test }] }],
									 	 handler = test}]
	},	
	#router{ routes = Results } = lotus_ws_router:compile_router(Router),
	%?debugFmt("route paths = ~p", [lists:map(fun(#route{path = P}) -> P end, Results)]),
	[Route1, Route2, Route3] = Results,
	?assert(Route1#route.path =:= "/"),
	?assert(Route2#route.path =:= "/api"),
	?assert(Route3#route.path =:= "/api/customer").	

route_sub4_test() ->	

	Router = #router {
		routes = [#route{path = "/", 
										 routes = [
										 		#route { path = "/api", handler = test, routes = [#route { path = "/customer", handler = test }] },
										 		#route { path = "/site", handler = test, routes = [#route { path = "/test", handler = test }] }
										 ],
									 	 handler = test}]
	},
	#router{ routes = Results } = lotus_ws_router:compile_router(Router),
	%?debugFmt("route paths ~p", [lists:map(fun(#route{path = P}) -> P end, Results)]),
	[Route1, Route2, Route3, Route4, Route5] = Results,
	?assert(Route1#route.path =:= "/"),
	?assert(Route2#route.path =:= "/api"),
	?assert(Route3#route.path =:= "/api/customer"),
	?assert(Route4#route.path =:= "/site"),
	?assert(Route5#route.path =:= "/site/test").		


route2_sub4_test() ->	

	Router = #router {
		routes = [#route{path = "/", 
										 routes = [
										 		#route { path = "/api", handler = test, routes = [#route { path = "/customer", handler = test }] },
										 		#route { path = "/site", handler = test, routes = [#route { path = "/test", handler = test }] }
										 ]}]
	},
	#router{ routes = Results } = lotus_ws_router:compile_router(Router),
	%?debugFmt("route paths ~p", [lists:map(fun(#route{path = P}) -> P end, Results)]),
	[Route1, Route2, Route3, Route4] = Results,
	?assert(Route1#route.path =:= "/api"),
	?assert(Route2#route.path =:= "/api/customer"),
	?assert(Route3#route.path =:= "/site"),
	?assert(Route4#route.path =:= "/site/test").		


route_handlers_test() ->	

	Router = #router {
		routes = [#route{path = "/", 
										 middlewares = #middlewares{ values = [test] },
										 routes = [
										 		#route { path = "/api", handler = test, routes = [#route { path = "/customer", handler = test }] },
										 		#route { path = "/site", handler = test, routes = [#route { path = "/test", handler = test, middlewares = #middlewares{ bypass = [test] } }] }
										 ]}],
		middlewares = [
			#middleware {
				name = test,
				handler = m_handler
			}
		]
	},
	#router{ routes = Results } = lotus_ws_router:compile_router(Router),
	?debugFmt("route paths ~p", [lists:map(fun(#route{path = P}) -> P end, Results)]),

	%?debugFmt("route middlewares ~p", [lists:map(fun(#route{middlewares = M}) -> M#middlewares.values end, Results)]),

	MCount = fun (#route{ middlewares = M}) -> length(M#middlewares.values) end,

	[Route1, Route2, Route3, Route4] = Results,
	?assert(Route1#route.path =:= "/api"),
	?assert(MCount(Route1) =:= 1),
	?assert(Route2#route.path =:= "/api/customer"),
	?assert(MCount(Route2) =:= 1),
	?assert(Route3#route.path =:= "/site"),
	?assert(MCount(Route3) =:= 1),
	?assert(Route4#route.path =:= "/site/test"),
	?assert(MCount(Route4) =:= 0).		


route_regexp_id_test() ->

	Router = #router {
		routes = [#route{path = "/api/:id()", handler = test}]
	},
	#router{ routes = CompiledRouter } = lotus_ws_router:compile_router(Router),

	%?debugFmt("CompiledRouter = ~p", [CompiledRouter]),

	Route = lotus_ws_router:find_route("/api/1", CompiledRouter),
	?assert(Route =/= nomatch),
	%?debugFmt("route_regexp_test = route = ~p, ~p", [Route#route.params, Route#route.compiled_path]),
	?assert(Route#route.compiled_path =:= "/api/:id"),
	?assert(maps:size(Route#route.params) =:= 1).	

route_regexp_id_limit_offset_test() ->

	Router = #router {
		routes = [#route{path = "/api/:id([0-9])/:limit([0-9])/:offset([0-9])", handler = test}]
	},
	#router{ routes = CompiledRouter } = lotus_ws_router:compile_router(Router),

	%?debugFmt("CompiledRouter = ~p", [CompiledRouter]),

	Route = lotus_ws_router:find_route("/api/10/11/111", CompiledRouter),
	?assert(Route =/= nomatch),
	%?debugFmt("route_regexp_test = route = ~p, ~p", [Route#route.params, Route#route.compiled_path]),
	?assert(Route#route.compiled_path =:= "/api/:id/:limit/:offset"),
	?assert(maps:size(Route#route.params) =:= 3).		

route_with_defaults_test() ->

	Router = #router {				
		routes = [#route{path = "/", 
										 defaults = [bearer_token],
								 	 	 routes = [
								 	 	 	#route {
								 	 	 		path = "/api/user",
								 	 	 		handler = api_handler
								 	 	 	}
								 	 	 ]}]
	},
	Result = lotus_ws_router:compile_router(Router),
	?debugFmt("Result = ~p", [Result]),
	#router{ routes = [Route1, Route2] } = Result,
	?assert(Route1#route.path =:= "/login"),
	?assert(Route2#route.path =:= "/api/user").	