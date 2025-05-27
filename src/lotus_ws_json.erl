-module(lotus_ws_json).

-export([
	encode/1,
	decode/1
	]).

str_to_bin_map(Map) ->
	str_to_bin_map(maps:keys(Map), Map).
str_to_bin_map([], Map) -> Map;
str_to_bin_map([K|T], Map) ->
	Val = maps:get(K, Map),
	NewVal = case Val of
		V when is_map(V) -> str_to_bin_map(V);
		_ -> Val
	end,
	NewMap = maps:merge(Map, #{string_to_bin(K) => string_to_bin(NewVal)}),
	str_to_bin_map(T, NewMap).

str_to_bin(List) -> str_to_bin(List, []).

str_to_bin(Map, _) when is_map(Map)->
	str_to_bin_map(Map);
str_to_bin(List=[Map|_], _) when is_map(Map)->
	[str_to_bin_map(Z) || Z <- List];
str_to_bin([], Result) -> Result;
str_to_bin([[_|_]=Item|Tail], ListOfList) ->
	NewItem = str_to_bin(Item, []), str_to_bin(Tail, ListOfList++[NewItem]);
str_to_bin([{_, _} = Val | T], ListResult) ->
	NewVal = string_tuple_to_bin(Val), str_to_bin(T, ListResult++[NewVal]).

string_tuple_to_bin({K, V}) ->
	{string_to_bin(K), string_to_bin(V)}.

string_to_bin(undefined) -> null;
string_to_bin(X) when is_binary(X) -> X;
string_to_bin(X) when is_atom(X) -> X;
string_to_bin(X) when is_list(X) -> list_to_binary(atom_to_str(X));
string_to_bin(X) -> X.

atom_to_str(undefined) -> null;
atom_to_str(Val) when is_atom(Val) -> lists:flatten(io_lib:format("~p", [Val]));
atom_to_str(Val) -> Val.

bin_to_str(List) -> bin_to_str(List, []).
bin_to_str([], Result) -> Result;

bin_to_str([[_|_]=Item|Tail], ListOfList) ->
	NewItem = bin_to_str(Item, []), bin_to_str(Tail, ListOfList++[NewItem]);

bin_to_str([{_, _} = Val | T], ListResult) ->
	NewVal = bin_to_string(Val), bin_to_str(T, ListResult++[NewVal]).

bin_to_string({K, V}) ->
	{binary_to_list(K), binary_to_list(V)}.


encode(Message) ->
	MessageBin = str_to_bin(Message),	
	jsx:encode(MessageBin).

decode(Json) ->
	case jsx:is_json(Json) of
		true ->
			Decoded = jsx:decode(Json, [{return_maps, false}]),
			bin_to_str(Decoded);
		_ -> undefined
	end.