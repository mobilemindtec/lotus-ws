-module(lotus_login_mw).

-include("include/lotus_ws.hrl").

-export([
	enter/1
]).

unauthorized() -> {401, [{auto, [{message, "required username and password"}]}]}.

result(_, undefined, undefined) -> unauthorized();
result(_, _, undefined) -> unauthorized();
result(_, undefined, _) -> unauthorized();
result(#ctx{ req = Req } = Ctx, Username, Password) -> Ctx#ctx {
	req = Req#req { body = #login{ username = Username, password = Password } }
}.

enter(#ctx{ req = #req{ body = Body }} = Ctx) when is_map(Body) ->
	%?debugMsg("enter"),
	Username = maps:get(<<"username">>, Body, undefined),
	Password = maps:get(<<"password">>, Body, undefined),
	result(Ctx, Username, Password);

enter(#ctx{} = Ctx) -> Ctx.