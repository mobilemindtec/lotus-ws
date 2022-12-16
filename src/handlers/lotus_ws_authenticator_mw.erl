-module(lotus_ws_authenticator_mw).

-include("include/lotus_ws.hrl").

-export([
	enter/1
]).

unauthorized() -> {401, [{auto, [{message, <<"required username and password">>}]}]}.
server_error() -> {500, [{auto, [{message, <<"wrong auth callback">>}]}]}.

call_authenticator(#ctx{} = Ctx, {arity, 1}, undefined, AuthFn) -> 
	apply(AuthFn, [Ctx]);
call_authenticator(#ctx{ req = #req { body = Login }} = Ctx, {arity, 2}, undefined, AuthFn) ->
	apply(AuthFn, [Ctx, Login]);
call_authenticator(_, _, undefined, _) -> server_error();

call_authenticator(#ctx{} = Ctx, {arity, 1}, Module, AuthFn) ->
	apply(Module, AuthFn, [Ctx]);		
call_authenticator(#ctx{ req = #req { body = Login }} = Ctx, {arity, 2}, Module, AuthFn) ->
 apply(Module, AuthFn, [Ctx, Login]);
call_authenticator(_, _, _, _) -> server_error().

call_authenticator(Ctx, true, Authenticator) -> 
	call_authenticator(Ctx, erlang:fun_info(Authenticator, arity), undefined, Authenticator);
call_authenticator(Ctx, false, Authenticator) -> 
	call_authenticator(Ctx, lotus_ws_utils:find_module_fn(Authenticator, authenticate), Authenticator, authenticate).

enter(#ctx{ authenticator = undefined } = Ctx) -> Ctx;

enter(#ctx{ authenticator = Authenticator, req = #req{ body = #login{} }} = Ctx) ->
	%?debugFmt("enter: ~p", [Authenticator]),
	call_authenticator(Ctx, erlang:is_function(Authenticator), Authenticator);


enter(_) -> unauthorized().