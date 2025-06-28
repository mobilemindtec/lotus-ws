-module(lotus_ws_handler_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/lotus_ws.hrl").

-compile(export_all).

% rebar3 eunit --module=lotus_ws_handler_test --sys_config=test/test.config 

myroute("/test", #req{ body = <<"ping">> }) ->
	{ok, {text, <<"pong">>}}.

myroute_any(#req{ body = <<"ping">> }) ->
	{ok, {text, <<"pong">>}}.

post("/test", #req{ body = <<"ping">> }) ->
	{ok, {text, <<"pong">>}}.

get(#req{ body = <<"ping">> }) ->
	{ok, {text, <<"pong">>}}.

setup() -> 
	ok.

cleanup(_) ->
	ok.

%  rebar3 eunit --module=lotus_ws_handler_test  --sys_config=test/test.config
handler_test_() ->
	
	{setup, 
		fun setup/0,
		fun cleanup/1,
		[
			?_test(handler_route_not_found())
			, ?_test(handler_route_server_error())
			, ?_test(handler_route_func())
			, ?_test(handler_route_mod_func())
			, ?_test(handler_route_mod_func_any())
			, ?_test(handler_route_mod())
			, ?_test(handler_route_mod_any())
			, ?_test(handler_route_func_not_found_with_invalid_guard())
			, ?_test(handler_route_func_with_enter_middleware())
			, ?_test(handler_route_func_with_leave_middleware())
			, ?_test(handler_route_func_with_enter_and_leave_middleware())
			]}.

handler_route_not_found() ->
	Router = #router{debug = true},
	{ok, Pid} = lotus_ws_router:start(Router),
	Req = #req { method = post, path = "/test", body = <<"ping">> }, 
	#ctx{ resp = Resp } = lotus_ws_handler:handle(Req),
	lotus_ws_router:stop(Pid),
	?assertEqual(Resp#resp.status, 404),
	?assertEqual(Resp#resp.body, "Not Found").

handler_route_server_error() ->
	Router = #router{
			debug = true
			, routes = [
				#route {
					path = "/test"
					, handler = fun(_Req=#req{ body = <<"ping">>}) ->
							throw(unknown_error)
					end
					}
				]
			},	
	{ok, Pid} = lotus_ws_router:start(Router),
	Req = #req { method = post, path = "/test", body = <<"ping">> }, 
	#ctx{ resp = Resp } = lotus_ws_handler:handle(Req),
	lotus_ws_router:stop(Pid),
	?assertEqual(Resp#resp.status, 500),
	?assertEqual(Resp#resp.body, "Server Error").

%  rebar3 eunit --test=lotus_ws_handler_test:handler_route_func  --sys_config=test/test.config
handler_route_func() ->
	Router = #router{
			debug = true
			, routes = [
				#route {
					path = "/test"
					, handler = fun(_Req=#req{ body = <<"ping">>}) ->
							#resp { status = 200, body = <<"pong">>}
					end
					}
				]
			},	
	{ok, Pid} = lotus_ws_router:start(Router),
	Req = #req { method = post, path = "/test", body = <<"ping">> }, 
	#ctx{ resp = Resp } = lotus_ws_handler:handle(Req),
	lotus_ws_router:stop(Pid),
	?assertEqual(Resp#resp.status, 200),
	?assertEqual(Resp#resp.body, <<"pong">>).

handler_route_mod_func() ->
	Router = #router{
			debug = true
			, routes = [
				#route {
					path = "/test"
					, handler = {lotus_ws_handler_test, myroute}
					}
				]
			},	
	{ok, Pid} = lotus_ws_router:start(Router),
	Req = #req { method = post, path = "/test", body = <<"ping">> }, 
	#ctx{ resp = Resp } = lotus_ws_handler:handle(Req),
	lotus_ws_router:stop(Pid),
	?assertEqual(Resp#resp.status, 200),
	?assertEqual(Resp#resp.body, <<"pong">>).

handler_route_mod_func_any() ->
	Router = #router{
			debug = true
			, routes = [
				#route {
					path = "/test"
					, handler = {lotus_ws_handler_test, myroute_any}
					}
				]
			},	
	{ok, Pid} = lotus_ws_router:start(Router),
	Req = #req { method = get, path = "/test", body = <<"ping">> }, 
	#ctx{ resp = Resp } = lotus_ws_handler:handle(Req),
	lotus_ws_router:stop(Pid),
	?assertEqual(Resp#resp.status, 200),
	?assertEqual(Resp#resp.body, <<"pong">>).

handler_route_mod() ->
	Router = #router{
			debug = true
			, routes = [
				#route {
					path = "/test"
					, handler = lotus_ws_handler_test
					}
				]
			},	
	{ok, Pid} = lotus_ws_router:start(Router),
	Req = #req { method = post, path = "/test", body = <<"ping">> }, 
	#ctx{ resp = Resp } = lotus_ws_handler:handle(Req),
	lotus_ws_router:stop(Pid),
	?assertEqual(Resp#resp.status, 200),
	?assertEqual(Resp#resp.body, <<"pong">>).

handler_route_mod_any() ->
	Router = #router{
			debug = true
			, routes = [
				#route {
					path = "/test"
					, handler = lotus_ws_handler_test
					}
				]
			},	
	{ok, Pid} = lotus_ws_router:start(Router),
	Req = #req { method = get, path = "/test", body = <<"ping">> }, 
	#ctx{ resp = Resp } = lotus_ws_handler:handle(Req),
	lotus_ws_router:stop(Pid),
	?assertEqual(Resp#resp.status, 200),
	?assertEqual(Resp#resp.body, <<"pong">>).

handler_route_func_not_found_with_invalid_guard() ->
	Router = #router{
			debug = true
			, routes = [
				#route {
					path = "/test"
					, handler = fun(_Req=#req{ body = <<"ping">>}) ->
							#resp { status = 200, body = <<"pong">>}
					end
					}
				]
			},	
	{ok, Pid} = lotus_ws_router:start(Router),
	Req = #req { method = post, path = "/test", body = <<"ping-invalid">> }, 
	#ctx{ resp = Resp } = lotus_ws_handler:handle(Req),
	lotus_ws_router:stop(Pid),
	?assertEqual(Resp#resp.status, 404),
	?assertEqual(Resp#resp.body, "Not Found").	


handler_route_func_with_enter_middleware() ->
	Router = #router{
			debug = true
			, routes = [
				#route {
					path = "/test"
					, middlewares = [
						#middleware {
							enter = fun(Req=#req{ body = <<"ping">> }) ->
									Req#req { body = <<"ping changed">> }
							end
							}
						]
					, handler = fun(_Req=#req{ body = <<"ping changed">>}) ->
							#resp { status = 200, body = <<"pong">>}
					end
					}
				]
			},	
	{ok, Pid} = lotus_ws_router:start(Router),
	Req = #req { method = post, path = "/test", body = <<"ping">> }, 
	#ctx{ resp = Resp } = lotus_ws_handler:handle(Req),
	lotus_ws_router:stop(Pid),
	?assertEqual(Resp#resp.status, 200),
	?assertEqual(Resp#resp.body, <<"pong">>).

handler_route_func_with_leave_middleware() ->
	Router = #router{
			debug = true
			, routes = [
				#route {
					path = "/test"
					, middlewares = [
						#middleware {
							leave = fun(#req{}, Resp=#resp{ body = <<"pong">>}) ->
									Resp#resp { body = <<"pong changed">> }
							end
							}
						]
					, handler = fun(_Req=#req{ body = <<"ping">>}) ->
							#resp { status = 200, body = <<"pong">>}
					end
					}
				]
			},	
	{ok, Pid} = lotus_ws_router:start(Router),
	Req = #req { method = post, path = "/test", body = <<"ping">> }, 
	#ctx{ resp = Resp } = lotus_ws_handler:handle(Req),
	lotus_ws_router:stop(Pid),
	?assertEqual(Resp#resp.status, 200),
	?assertEqual(Resp#resp.body, <<"pong changed">>).	

handler_route_func_with_enter_and_leave_middleware() ->
	Router = #router{
			debug = true
			, routes = [
				#route {
					path = "/test"
					, middlewares = [
						#middleware {							
							enter = fun(Req=#req{ body = <<"ping">> }) ->
									Req#req { body = <<"ping changed">> }
							end
							, leave = fun(#req{}, Resp=#resp{ body = <<"pong">>}) ->
									Resp#resp { body = <<"pong changed">> }
							end
							}
						]
					, handler = fun(_Req=#req{ body = <<"ping changed">>}) ->
							#resp { status = 200, body = <<"pong">>}
					end
					}
				]
			},	
	{ok, Pid} = lotus_ws_router:start(Router),
	Req = #req { method = post, path = "/test", body = <<"ping">> }, 
	#ctx{ resp = Resp } = lotus_ws_handler:handle(Req),
	lotus_ws_router:stop(Pid),
	?assertEqual(Resp#resp.status, 200),
	?assertEqual(Resp#resp.body, <<"pong changed">>).		