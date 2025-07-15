%% vi:ts=4 sw=4 et

-include_lib("eunit/include/eunit.hrl").

-define(CONTENT_TYPE_JSON, #{<<"content-type">> => <<"application/json">>}).
-define(CONTENT_TYPE_TEXT, #{<<"content-type">> => <<"text/plain">>}).
-define(CONTENT_TYPE_HTML, #{<<"content-type">> => <<"text/html">>}).

-record(auth, {
		username :: string()
		, token :: string()
		, roles = [] :: list(atom())
		, data = #{} :: map()}).

-type auth() :: #auth{}.


-record(req, {
		body :: any()
		, path :: string()
		, method :: atom() % get, post, put, delete, patch			
		, headers = #{} :: map()	
		, params = #{} :: map()
		, queries = #{} :: map()
		, auth = #auth{} :: auth()
		, data = #{} :: map()
		, req :: cowboy_req:req()
		, route :: route()
		}).

-type req() :: #req{}.

-record(resp, {
		body = "" :: any()
		, headers = #{} :: map()
		, status  = 200:: integer()}).

-type resp() :: #resp{}.


-record(login, {
		username :: string()
		, password :: string()}).

-type login() :: #login{}.													 

-record(ctx, {
		req = undefined :: req()
		,	resp = undefined :: resp()
		}).

-type ctx() :: #ctx{}.

-record(middleware, {
		enter = undefined :: fun((req()) -> req() | resp())
						, leave = undefined :: fun((req(), resp()) -> resp())
						, handler = undefined :: tuple() % {enter|leave, module, func}
						}).


-type middleware() :: #middleware{}.


-record(route, {
		name :: atom()
		, path :: string()
		, compiled_path :: string()
		%% fun(req) -> res | tuple
		%% fun(verb, req) -> res | tuple
		%% module:verb(path, req) -> res | tuple
		%% module:verb(req) -> res | tuple 
		%% { module, fun(verb, path, req) -> res | tuple }
		%% { module, fun(path, req) -> res | tuple }
		%% { module, fun(req) -> res | tuple }
		, handler = undefined ::  fun((req()) -> resp() | tuple()) | atom() | tuple() % {module,func}
						%% module:enter(req) -> req | resp
						%% module:leave(req, resp) -> resp
						%% { module, fun(req) -> req | resp }
						%% { module, fun(req, resp) -> resp }
						%% middleware
						, middlewares = [] :: list(atom() | tuple() | middleware())		
						, roles = [] :: atom()
						, params = #{} :: map()
						, routes = [] :: list(route())
						}).

-type route() :: #route{}.

%% The router middleware are global middlewares. It's can add on subroutes by name
-record(router, {
		routes = [] :: list(route())
		, debug = false :: boolean()
		%% fun(req, error) -> resp 
		%% {module, fun(req, error) -> resp}, 
		%% module:recover(req, error) -> resp
		, recover = undefined :: fun((req(), any()) -> resp()) | tuple() | atom() 
						%% fun(status, req, resp) ->  resp
						%% {status, module, func(req, resp) -> resp}, 
						%% module:intercept(status, req, resp) -> resp
						, interceptors = [] :: list(fun((integer(), req()) -> resp()) | tuple() | atom()) 
						
						}).


-type router() :: #router{}.


-record(vrule, {
		field :: string()
		, rules = [] :: list()}).

-type vrule() :: #vrule{}.