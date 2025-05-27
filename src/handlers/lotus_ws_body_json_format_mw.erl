-module(lotus_ws_body_json_format_mw).

-include("include/lotus_ws.hrl").

-export([
	leave/1
	]).

invalid_json() -> {500, [{json, [{error, <<"invalid json data">>}]}]}.

body_format(Ctx, _, undefined) -> Ctx;
body_format(_, false, _) -> invalid_json();
body_format(#ctx{ resp = Resp} = Ctx, true, Body) -> 
	Ctx#ctx{ resp = Resp#resp { body = jsx:encode(Body)} }.


leave(#ctx{ resp = #resp { body = Body }} = Ctx) -> 
	%?debugMsg("leave"),
	body_format(Ctx, jsx:is_term(Body), Body).