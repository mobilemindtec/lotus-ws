# lotus-ws

Erlang  web framework build on top of Cowboy



## Usage


### Define routes and handler


```erl

%% api_routes.erl
get_routes() ->
	JsonHandler = [middleware_json_parse, middleware_json_format],
	Routes = #{
		debug => true
		, interceptors => []
		, recover => undefined
		, routes => [  		
			#{defaults => []
				, path => "/api"
				, routes => [
					
					#{path => "/healthcheck"
						, handler => api_handler}
					
					,#{path => "/auth"
						, middlewares => JsonHandler ++ [middleware_login_parse]
						, handler => api_handler}									
					
					, #{path => "/register"
						, middlewares => JsonHandler ++ [{enter, api_handler, auth_check}]
						, handler => api_handler}
					
					, #{path => "/notify/:channel"
						, middlewares => JsonHandler ++ [{enter, api_handler, auth_check}]
						, handler => api_handler}
					
					, #{path => "/apps/register"
						, middlewares => JsonHandler
						, handler => api_handler}
					]
				}
			]
		}

%% api_handler.erl

-export([post/2, get/2]).

auth_check(Ctx = #ctx{ req = #req{ headers = Headers } }) ->
	case maps:get(<<"auhtorization">>, Headers) of
		undefined ->
			{401, {json, #{"message" => "Unauthorized"}}};
		ApiKey ->			
			% find by ApiKey			
			Ctx
	end.

get("/api/healthcheck", _) ->
	{ok, {text, "alive"}}.

post("/api/auth", #req{ body = #login{ username = UserName, password = Password }}) ->
	if 
		authenticate(UserName, Password) ->
			{ok, {json, #{"message" => "auth success"}}};
		true ->
			{401, {json, #{"message" => "invalid username or password"}}}
	end;

post("/api/notify/:channel", #req{ params = #{ "channel" := Channel }, body = Body}) ->
	{ok, {text, "notify"}};

post("/api/register", Req=#req{ body = Body }) ->
	{ok, {text, "register"}}.

```

### Start cowboy with lotus handler

```erlang
start(_StartType, _StartArgs) ->  
  Dispatch = cowboy_router:compile(
		[{'_', [
		    {"/", cowboy_static, {priv_file, chat, "views/index.html"}},
		    {"/api/[...]", lotus_ws_handler, []},
		    {"/[...]", cowboy_static, {priv_dir, chat, "./"}}
		]}]),
 	{ok, _} = cowboy:start_clear(chat_listener, [{port, 8000}], #{env => #{dispatch => Dispatch}}),  
```

### Start router:

```erlang
init([]) ->
	Routes = api_routes:get_routes(),
  SupFlags = #{ 
  	strategy => one_for_all,
	 	intensity => 20000,
	 	period => 5},
  ChildSpecs = [
      #{id => lotus_ws_router,
				start => {lotus_ws_router, start_link, [Routes]},
				restart => permanent,
				shutdown => brutal_kill,
				type => worker,
				modules => [lotus_ws_router]}  
  ],
  {ok, {SupFlags, ChildSpecs}}.	    
```

## Details

### Router handler


Handle with module and custom function


```erl

%% route

#{path => "/my-path"
, handler => {mymodule, myfunc}}

%% mymodule
-module(mymodule).

myfunc(get, "/my-path", #req{}) -> {ok, {text, "ok"}}.

%% or

myfunc("/my-path", #req{}) -> {ok, {text, "ok"}}.

%% or

myfunc(get, "/my-path", #req{}) -> {ok, {text, "ok"}}.

```

Handle with module with default names (get, post, put, delete, etc..). 


```erl

%% route

#{path => "/my-path/:id"
, handler => mymodule}

%% mymodule

-module(mymodule).

get("/my-path/:id", #req{params = #{"id" := Id}}) -> #resp{}.
post("/my-path/:id", #req{params = #{"id" := Id}, body = Body}) -> #resp{}.

%% or

get(#req{}) -> #resp{}.
post(#req{}) -> #resp{}.

```

Using function


```erl

#{path => "/my-path"
, handler => fun(#req{}) -> #resp{} end }

```

```erl
#{path => "/my-path"
, handler => fun(get, #req{}) -> {ok, {text, "my response"}} end }
```

### Router middleware

The middleware can be the enter or leave function.

Enter function receive a `#req` as argument and can return 
a `#req{}` to continue request or a `#resp{}` to terminate request. 

Leave function receive a `#req{}` and `#resp{}` arguments should responds with a `#resp{}`.


Middleware with module and custom function


```erl
#{path => "/my-path"
, handler => fun(#req{}) -> {ok, {text, "my response"}} end
, middleware => [{mymodule, myenterfunc}, {mymodule, myleavefunc}] }

%% mymodule
module(mymodule).
myenterfunc(#req{}) -> #req{} | #resp{}. %enter
myleavefunc(#req{}, #resp{}) -> #resp{}. %leave

```

Middleware with module with default enter/leave function


```erl
#{path => "/my-path"
, handler => fun(#req{}) -> {ok, {text, "my response"}} end
, middleware => [mymodule] }

%% mymodule
module(mymodule).
enter(#req{}) -> #req{} | #resp{}.
leave(#req{}, #resp{}) -> #resp{}.

```

Middleware with functions


```erl
#{path => "/my-path"
, handler => fun(#req{}) -> {ok, {text, "my response"}} end
, middleware => [
		fun(#req{}) -> #req{} | #resp{} end % enter
	, fun(#req{}, #resp{}) -> #resp{} end % leave
	]}
```

Middleware with record


```erl
#{path => "/my-path"
, handler => fun(#req{}) -> {ok, {text, "my response"}} end
, middleware => #middleware {
		enter = fun(#req{}) #req{} | #resp{} end
	, leave = fun(#req{}, #resp{}) #resp{} end
}}
```

### Response

```erl

%% 200 text/plain
{ok, {text, "text"}}

%% 200 application/json
{ok, {json, [{"id", 1}, {"name", "Ricardo"}]}}

%% 200 application/json
{ok, {json, #{ "id" => 1, "name" => "Ricardo"}}}

%% 200 text/plain
{200, {text, "text"}}

%% 404 text/plain
{404, {text, "not found"}}

%% 404 text/plain
{not_found, {text, "not found"}}

%% 404 text/plain
#resp { 
		status = 404
	, body = "not found"
	, headers = #{ "content-type" => "text/plain"}
}

```

### Request 

```erl

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


```

### Default middlewares

* middleware_bearer_auth
* middleware_bearer_render
* middleware_json_format
* middleware_json_parse
* middleware_login_parse
