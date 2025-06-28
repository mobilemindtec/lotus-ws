-module(middleware_json_parse).
-include("include/lotus_ws.hrl").

%%
%% Middleware to process json request and response
%%

-export([
	enter/1
	]).

error() -> {500, [{json, [{error, <<"invalid json data">>}]}]}.


parse(Ctx, _, undefined) -> Ctx;
parse(_, false, _) -> error();
parse(_, {incomplete, _}, _) -> error();
parse(#ctx{ req = Req } = Ctx, true, Body) -> 
	Ctx#ctx{ req = Req#req { body = lotus_ws_json:decode(Body)} }.

enter(#ctx{ req = #req { body = Body }} = Ctx) -> 
	%?debugMsg("enter"),
	parse(Ctx, jsx:is_json(Body), Body).	