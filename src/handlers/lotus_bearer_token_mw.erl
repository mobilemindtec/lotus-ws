-module(lotus_bearer_token_mw).

-include("include/lotus_ws.hrl").

-export([
	enter/1
]).

unauthorized() -> {401, [{json, [{message, "unauthorized"}]}]}.

auth_roles(#auth{ roles = Roles }) -> Roles;
auth_roles(_) -> [].

default_configs() ->
	[
	  {key, "12345678"},
	  {exp, 3600},
	  {alg, "HS256"}
	].

get_config(Key, List) ->
	Def = proplists:get_value(Key, default_configs()),
	proplists:get_value(Key, List, Def).

enter(#ctx{ auth = Auth, req = #req { body = #login{ username = Username } }}) -> 
	%?debugMsg("enter"),
	Configs = lotus_utils:get_env(bearer_token, default_configs()),
	Key = get_config(key, Configs),
	Exp = get_config(exp, Configs),
	Alg = get_config(alg, Configs),
	%?debugFmt("Key ~p, Exp = ~p, Alg ~p", [Key, Exp, Alg]),
	Claims = [
		{username, Username},
		{roles, auth_roles(Auth)}
	],
	{ok, Token} = jwt:encode(list_to_binary(Alg), Claims, Exp, Key),
	Data = [
		{access_token, Token},
		%{refresh_token, }
		{token_type, <<"Bearer">>},
		{expires_in, Exp}		
	],
	{200, [{json, Data}]};

enter(#ctx{}) -> unauthorized().