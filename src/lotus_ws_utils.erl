-module(lotus_ws_utils).

-export([
	is_content_type/2,
	is_json/1,
	is_html/1,
	is_text/1,
	get_content_type_atom/1,
	get_content_type/1,
	sprint/2,
	find_module_fn/2,
	list_in_list/3,
	list_in_list_not/3,
	list_in_list_any/3,
	binary_to_string/1,
	get_env/2
	]).

is_content_type(Headers, ContentType) ->
	maps:is_key(<<"content-type">>, Headers) andalso maps:get(<<"content-type">>) =:= ContentType.

is_json(Headers) -> is_content_type(Headers, <<"application/json">>).
is_html(Headers) -> is_content_type(Headers, <<"text/html">>).
is_text(Headers) -> is_content_type(Headers, <<"text/plain">>).

select_content_type(<<"application/json">>) -> json;
select_content_type(<<"text/html">>) -> html;
select_content_type(<<"text/plain">>) -> text;
select_content_type(undefined) -> json;
select_content_type(ContentType) -> ContentType.

get_content_type_atom(Headers) ->
	case maps:is_key(<<"content-type">>, Headers) of
		true -> select_content_type(maps:get(<<"content-type">>, Headers));
		_ -> select_content_type(undefined)
	end.

get_content_type(Headers) ->
	case maps:is_key(<<"content-type">>, Headers) of
		true -> maps:get(<<"content-type">>, Headers);
		_ -> undefined
	end.


sprint(Fmt, []) -> lists:flatten(Fmt);

sprint(Fmt, [{ K, V} | T]) ->
	Word = "\\$" ++ atom_to_list(K),		
	if 
		is_number(V) ->
			Valor = lists:flatten(io_lib:format("~p", [V]));
		is_atom(V) ->
			Valor = lists:flatten(io_lib:format("~p", [V]));
		true ->
			Valor = V
	end,
	Fmt1 = re:replace(Fmt, Word, Valor, [{return, list}]),
	sprint(Fmt1, T).

find_module_fn(Module, FnName) ->
	lists:keyfind(FnName, 1, apply(Module, module_info, [exports])).

list_in_list(Fn, ListX, ListY) ->
	lists:filter(fun(X) -> 
				Rs = lists:filter(fun(Y) -> Fn(X, Y) end, ListY),
				length(Rs) > 0
		end, ListX).	

list_in_list_not(Fn, ListX, ListY) ->
	lists:filter(fun(X) -> 
				Rs = lists:filter(fun(Y) -> Fn(X, Y) end, ListY),
				length(Rs) =:= 0
		end, ListX).	

list_in_list_any(Fn, ListX, ListY) ->
	Results = list_in_list(Fn, ListX, ListY),
	length(Results) > 0.

binary_to_string(Bin) ->
	lists:flatten(binary_to_list(Bin)).	


get_env(Var, Def) ->
	case application:get_env(lotus_ws, Var) of
		undefined -> Def;
		{ok, Val} -> Val
	end.
