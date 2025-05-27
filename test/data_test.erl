-module(data_test).

-include("../include/lotus_ws.hrl").

-compile(export_all).


login_route() ->
	#route {
		path = "/login"
		, middlewares = [
			lotus_ws_body_json_parse_mw
			, lotus_ws_login_mw
			, lotus_ws_authenticator_mw
			, lotus_ws_bearer_token_mw
			]
		, handler = lotus_ws_dummy_handler}.	

default_authenticator() ->
	fun(#ctx{} = Ctx, #login{username = Username, password = Password}) ->
			if 
				Username =:= "test" andalso Password =:= "test123" ->
					Ctx;
				true -> 
					{401, [{json, [{message, <<"invalid username or password">>}]}]}
			end	
	end.

login_ctx(Password) -> 
	#ctx {
		req = #req {
			path = "/login",
			method = <<"POST">>,
			headers = #{<<"content-type">> => <<"application/json">>},
			body = jsx:encode([{username, "test"}, {password, Password}])
			}
		}.

user_me_ctx(Token) -> 
	#ctx {
		req = #req {
			path = "/api/user/me",
			method = <<"GET">>,
			headers = #{
				<<"content-type">> => <<"application/json">>,
				<<"authorization">> => Token
				}
			}
		}.	