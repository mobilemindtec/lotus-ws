-module(lotus_ws_dummy_handler).

-include("include/lotus_ws.hrl").

-export([
	get/2,
	post/2,
	put/2,
	delete/2,
	patch/2,
	enter/1,
	leave/1,
	error/1
	]).


enter(Ctx) -> Ctx.
leave(Ctx) -> Ctx.
error(Ctx) -> Ctx.

get(_, Ctx) -> Ctx.
post(_, Ctx) -> Ctx.
put(_, Ctx) -> Ctx.
delete(_, Ctx) -> Ctx.
patch(_, Ctx) -> Ctx.
