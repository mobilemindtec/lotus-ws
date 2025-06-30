-module(middleware_json_format).
-include("../include/lotus_ws.hrl").

%%
%% Middleware to process json request and response
%%

-export([
	leave/2
	]).

error() -> {500, [{json, [{error, <<"invalid json data">>}]}]}.

format(Resp, _, undefined) -> Resp;
format(_, false, _) -> error();
format(Resp, true, Body) -> 
	Resp#resp { body = jsx:encode(Body) }.


leave(_, Resp=#resp { body = Body }) -> 
	format(Resp, jsx:is_term(Body), Body).