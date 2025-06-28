-module(middleware_login_parse).
-include("include/lotus_ws.hrl").

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
next(#ctx{ req = Req } = Ctx, Username, Password) -> 
	Ctx#ctx {
		req = Req#req { body = 
			#login{ username = Username, password = Password } }}.

enter(#ctx{ req = #req{ body = Body }} = Ctx) when is_map(Body) ->
	%?debugMsg("enter"),
	Username = maps:get(<<"username">>, Body, undefined),
	Password = maps:get(<<"password">>, Body, undefined),
	next(Ctx, Username, Password);

enter(#ctx{} = Ctx) -> Ctx.