-module(middleware_login_parse).
-include("../include/lotus_ws.hrl").

%%
%% Middleware to process login. The middleware resolve username and password 
%% from json request
%%

-export([
	enter/1
	]).

unauthorized() -> {401, [{auto, [{message, "required username and password"}]}]}.

next(_, undefined, undefined) -> unauthorized();
next(_, _, undefined) -> unauthorized();
next(_, undefined, _) -> unauthorized();
next(Req, Username, Password) -> 
	Req#req { body = 
		#login{ username = Username, password = Password } }.

enter(Req = #req{ body = Body }) when is_map(Body) ->
	logger:debug("enter ~p", [Body]),
	Username = maps:get("username", Body, undefined),
	Password = maps:get("password", Body, undefined),
	next(Req, Username, Password);

enter(#req{} = Req) -> Req.