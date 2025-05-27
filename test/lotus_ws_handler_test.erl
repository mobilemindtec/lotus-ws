-module(lotus_ws_handler_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/lotus_ws.hrl").

-compile(export_all).

% rebar3 eunit --module=lotus_ws_handler_test --sys_config=test/test.config 


setup() -> 
	ok.

cleanup(_) ->
	ok.

handler_test_() ->
	
	{setup, 
		fun setup/0,
		fun cleanup/1,
		[
			?_test(handler_login_success()),
			?_test(handler_login_failure()),
			?_test(handler_auth_and_enter_auth_route())
			]}.


%  rebar3 eunit --test=lotus_ws_handler_test:handler_login_success_test  --sys_config=test/test.config
handler_login_success() ->
	Router = #router {
			authenticator = data_test:default_authenticator(),
			routes = [
				data_test:login_route()
				]},
	{ok, Pid} = lotus_ws_router:start(Router),
	LoginCtx = lotus_ws_handler:prepare_ctx(data_test:login_ctx("test123")),
	#ctx{ resp = Resp } = lotus_ws_handler:run_ctx(LoginCtx),
	lotus_ws_router:stop(Pid),
	?assert(Resp#resp.status =:= 200).

%  rebar3 eunit --test=lotus_ws_handler_test:handler_login_failure_test  --sys_config=test/test.config
handler_login_failure() ->
	
	Router = #router {
			authenticator = data_test:default_authenticator(),
			routes = [
				data_test:login_route()
				]
			},
	{ok, Pid} = lotus_ws_router:start(Router),
	LoginCtx = lotus_ws_handler:prepare_ctx(data_test:login_ctx("test")),
	#ctx{ resp = Resp } = lotus_ws_handler:run_ctx(LoginCtx),
	lotus_ws_router:stop(Pid),
	?assert(Resp#resp.status =:= 401).


handler_auth_and_enter_auth_route() ->
	
	Router = #router {				
			authenticator = data_test:default_authenticator(),
			routes = [#route {
					path = "/",
					routes = [
						data_test:login_route(),					
						#route {
							path = "/api/user/me",
							middlewares = [lotus_ws_bearer_token_auth_mw],
							handler_fn  = fun(_) -> 
									{200, [{json, [{status, "success"}]}]} 
							end
							}
						]
					}
				
				]},
	
	{ok, Pid} = lotus_ws_router:start(Router),
	
	
	LoginCtxFail = lotus_ws_handler:prepare_ctx(data_test:login_ctx("test")),
	#ctx{ resp = RespAuthFail } = lotus_ws_handler:run_ctx(LoginCtxFail),	
	%?debugFmt("RespAuthFail = ~p", [RespAuthFail]),
	?assert(RespAuthFail#resp.status =:= 401),
	
	LoginCtxSuccess = lotus_ws_handler:prepare_ctx(data_test:login_ctx("test123")),
	#ctx{ resp = RespAuthSuccess } = lotus_ws_handler:run_ctx(LoginCtxSuccess),	
	%?debugFmt("RespAuthSuccess = ~p", [RespAuthSuccess]),
	
	UserMeWithInvalidTokenCtx = lotus_ws_handler:prepare_ctx(data_test:user_me_ctx(<<"xxxxxxx">>)),
	#ctx{ resp = RespAuthTokenFail } = lotus_ws_handler:run_ctx(UserMeWithInvalidTokenCtx),
	%?debugFmt("RespAuthTokenFail = ~p", [RespAuthTokenFail]),
	?assert(RespAuthTokenFail#resp.status =:= 401),
	
	#{ <<"access_token">> := AccessToken } = jsx:decode(RespAuthSuccess#resp.body),
	UserMeWithValidTokenCtx = lotus_ws_handler:prepare_ctx(data_test:user_me_ctx(AccessToken)),
	#ctx{ resp = RespAuthTokenSuccess } = lotus_ws_handler:run_ctx(UserMeWithValidTokenCtx),
	%?debugFmt("RespAuthTokenSuccess = ~p", [RespAuthTokenSuccess]),
	lotus_ws_router:stop(Pid),
	?assert(RespAuthTokenSuccess#resp.status =:= 200).	
