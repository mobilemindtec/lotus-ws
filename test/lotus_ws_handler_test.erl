-module(lotus_ws_handler_test).

-include_lib("eunit/include/eunit.hrl").

-include("include/lotus_ws.hrl").

-compile(export_all).

% rebar3 eunit --module=lotus_ws_handler_test --sys_config=test/test.config 

login_ctx(Password) -> #ctx {
		req = #req {
			path = "/login",
			method = <<"POST">>,
			headers = #{<<"content-type">> => <<"application/json">>},
			body = jsx:encode([{username, "test"}, {password, Password}])
		}
	}.

user_me_ctx(Token) -> #ctx {
		req = #req {
			path = "/api/user/me",
			method = <<"GET">>,
			headers = #{
				<<"content-type">> => <<"application/json">>,
				<<"authorization">> => Token
			}
		}
	}.	


default_handler_test() ->

	Router = #router {				
		routes = [#route{path = "/", 
										 defaults = [bearer_token]}]
	},
	CompiledRouter = lotus_ws_router:compile_router(Router),
	#ctx{ resp = Resp } = lotus_ws_handler:process_ctx(login_ctx("test123"), CompiledRouter),
	?assert(Resp#resp.status =:= 200).

default_handler_auth_test() ->

	Router = #router {				
		authenticator = fun(Ctx, #login{username = Username, password = Password}) -> 
			if 
				Username =:= "test" andalso Password =:= "test123" ->
					?debugMsg("OK!"),
					Ctx;
				true -> 
					?debugMsg("401"),
					{401, [{json, [{message, <<"invalid username or password">>}]}]}
			end
		end,
		routes = [#route{path = "/", 
										 defaults = [bearer_token],
										 routes = [
										 	#route {
										 		path = "/api/user/me",
												middlewares = [lotus_ws_bearer_token_auth_mw],
												fn = fun(Ctx) -> {200, [{json, [{status, "success"}]}]} end
										 	}
										 ]}]
	},

	CompiledRouter = lotus_ws_router:compile_router(Router),
	
	LoginCtxFail = lotus_ws_handler:from_ctx(login_ctx("test"), Router),
	#ctx{ resp = RespAuthFail } = lotus_ws_handler:process_ctx(LoginCtxFail, CompiledRouter),	
	?debugFmt("RespAuthFail = ~p", [RespAuthFail]),
	?assert(RespAuthFail#resp.status =:= 401),
	
	LoginCtxSuccess = lotus_ws_handler:from_ctx(login_ctx("test123"), Router),
	#ctx{ resp = RespAuth } = lotus_ws_handler:process_ctx(LoginCtxSuccess, CompiledRouter),	
	?debugFmt("RespAuth = ~p", [RespAuth]),
	
	#ctx{ resp = RespFail } = lotus_ws_handler:process_ctx(user_me_ctx(<<"xxxxxxx">>), CompiledRouter),
	?debugFmt("RespFail = ~p", [RespFail]),
	?assert(RespFail#resp.status =:= 401),

	#{ <<"access_token">> := AccessToken } = jsx:decode(RespAuth#resp.body),
	#ctx{ resp = Resp } = lotus_ws_handler:process_ctx(user_me_ctx(AccessToken), CompiledRouter),
	?debugFmt("Resp = ~p", [Resp]),
	?assert(Resp#resp.status =:= 200).	