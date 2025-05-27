-module(lotus_ws_validator).

-include("include/lotus_ws.hrl").

-export([
	validate/2,
	new_rule/2
]).

new_rule(Field, Rules) -> #vrule{ field = Field, rules = Rules }.

validate(Json = #{}, [#vrule{}|_] = Rules) -> validate(Json, Rules, []).

validate(_, [], []) -> ok;
validate(_, [], Validations) -> [{errors, Validations}];
validate(Json, [H|T], Validations) ->
	#vrule{ field = Field, rules = Rules } = H,
	Value = maps:get(Field, Json, undefined),
	validate(Json, T, Validations++validate_required_rule(Field, Value, Rules)).

validate_required_rule(Field, Value, #{ required := true }) ->
	check_required(Field, Value);
validate_required_rule(_, _, _) -> [].

%check_required(Field, 0) -> required_error_entry(Field);
%check_required(Field, 0.0) -> required_error_entry(Field);
check_required(Field, undefined) -> required_error_entry(Field);
check_required(Field, #{}) -> required_error_entry(Field);
check_required(Field, []) -> required_error_entry(Field);
check_required(_, _) -> [].

required_error_entry(Field) -> 
	[[{<<Field/binary>>,  <<"value is required">>}]].