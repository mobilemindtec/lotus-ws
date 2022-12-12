-module(lotus_ws_body_json_parse_mw).

-include("include/lotus_ws.hrl").

-export([
	enter/1
]).

invalid_json() -> {500, [{json, [{error, <<"invalid json content">>}]}]}.

body_parse(Ctx, _, undefined) -> Ctx;
body_parse(#ctx{ req = Req } = Ctx, true, Body) -> Ctx#ctx{ req = Req#req { body = jsx:decode(Body)} };
body_parse(_, false, _) -> invalid_json();
body_parse(_, {incomplete, _}, _) -> invalid_json().


enter(#ctx{ req = #req { body = Body }} = Ctx) -> 
	%?debugMsg("enter"),
	body_parse(Ctx, jsx:is_json(Body), Body).