-module(middleware_json_parse).
-include("../include/lotus_ws.hrl").

%%
%% Middleware to process json request and response
%%

-export([
	enter/1
	]).

error() -> {500, [{json, [{error, <<"invalid json data">>}]}]}.


parse(Req, _, undefined) -> Req;
parse(_, false, _) -> error();
parse(_, {incomplete, _}, _) -> error();
parse(Req, true, Body) -> 
	Req#req { body = lotus_ws_json:decode(Body)}.

enter(Req=#req { body = Body }) -> 
	%?debugMsg("enter"),
	parse(Req, jsx:is_json(Body), Body).	