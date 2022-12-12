-module(lotus_ws_bearer_token_auth_mw).

-include("include/lotus_ws.hrl").


-export([
	enter/1
]).

unauthorized() -> {401, [{json, [{message, "unauthorized"}]}]}.

route_roles(#route{ roles = Roles }) -> Roles;
route_roles(_) -> [].

default_configs() ->
	[
	  {key, "12345678"},
	  {exp, 3600},
	  {alg, "HS256"}
	].

get_config(Key, List) ->
	Def = proplists:get_value(Key, default_configs()),
	proplists:get_value(Key, List, Def).

parse_token(undefined) -> undefined;
parse_token(Token) -> binary:replace(Token, <<"Bearer ">>, <<"">>).

decode_token(undefined, _) -> undefined;
decode_token(Token,Key) -> jwt:decode(Token, Key).

auth_result({ok, Claims}) -> 
	#auth { username = maps:get(<<"username">>, Claims),
				 	roles = maps:get(<<"roles">>, Claims) };
auth_result(_) -> undefined.

validate_roles([], undefined) -> false;
validate_roles([], _) -> true;
validate_roles(_, #auth{ roles = [] }) -> false;
validate_roles(RouteRoles, #auth{ roles = AuthRoles }) -> 
	lotus_ws_utils:list_in_list_any(fun(X, Y) -> X =:= Y end, AuthRoles, RouteRoles).	

auth_finish(Ctx, Auth, true) -> Ctx#ctx { auth = Auth };
auth_finish(_, _, false) -> unauthorized().

enter(#ctx{ route = Route, req = #req { headers = Headers }} = Ctx) -> 
	Configs = lotus_ws_utils:get_env(bearer_token, default_configs()),
	Key = get_config(key, Configs),
	Token = parse_token(maps:get(<<"authorization">>, Headers, undefined)),
	Claims = decode_token(Token, Key),
	Auth = auth_result(Claims),
	Validate = validate_roles(route_roles(Route), Auth),
	auth_finish(Ctx, Auth, Validate);

enter(_) -> unauthorized().