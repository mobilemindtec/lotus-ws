-module(middleware_json_format).
-include("include/lotus_ws.hrl").

%%
%% Middleware to process json request and response
%%

-export([
	leave/1
	]).

error() -> {500, [{json, [{error, <<"invalid json data">>}]}]}.

format(Ctx, _, undefined) -> Ctx;
format(_, false, _) -> error();
format(#ctx{ resp = Resp} = Ctx, true, Body) -> 
	Ctx#ctx{ resp = Resp#resp { body = jsx:encode(Body)} }.


leave(#ctx{ resp = #resp { body = Body }} = Ctx) -> 
	%?debugMsg("leave"),
	format(Ctx, jsx:is_term(Body), Body).