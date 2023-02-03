# lotus-ws
ER lang web framework on top cowboy


## Example

### Routes configs 

	get_routes() -> [  


    #{path => "/healthcheck"
    , handler => healthcheck_controller},

    #{defaults => [bearer_token]
    , path => "/api"
    , routes => [
        #{path => "/customer/:id([0-9])"
        , handler => customer_controller}
      , #{path => "/auth",
          handler => login_controller
        , middlewares => [login_controller]}
      , #{path => "/message",
          handler => message_controller
        , middlewares => [lotus_ws_body_json_parse_mw, chat_token_resolver_mw, message_controller]
        , routes => [
            #{path => "/read"
            , handler => message_controller,
              middlewares => #{
                bypass => [message_controller]
              }},
            #{path => "/received"
            , handler => message_controller,
              middlewares => #{
                bypass => [message_controller]
              }}
          ]}]
    }
  ].   

### Start cowboy handler

	start(_StartType, _StartArgs) ->

	    RouterCfg = #{ routes => get_routes(), debug => true },

	    Dispatch = cowboy_router:compile(
	        [{'_', [
	            {"/", cowboy_static, {priv_file, chat, "views/index.html"}},
	            {"/healthcheck", lotus_ws_handler, []},
	            {"/api/[...]", lotus_ws_handler, []},
	            {"/[...]", cowboy_static, {priv_dir, chat, "./"}}
	        ]}]
	    ),
	    {ok, _} = cowboy:start_clear(chat_listener, [{port, 8000}], #{env => #{dispatch => Dispatch}}),  


### Start router on app sup:

	init([DbConfig, RouterConfig]) ->
	    SupFlags = #{strategy => one_for_all,
	                 intensity => 20000,
	                 period => 5},
	    ChildSpecs = [
	      #{id => lotus_ws_router,
	        start => {lotus_ws_router, start_link, [RouterConfig]},
	        restart => permanent,
	        shutdown => brutal_kill,
	        type => worker,
	        modules => [lotus_ws_router]}  
	    ],
	    {ok, {SupFlags, ChildSpecs}}.	    

###  Controllers

Example of controlles with handlers, middlewares and validations

===============================================

#### healthcheck_controller.erl

	-module(healthcheck_controller).
	-export([get/2]).
	get("/healthcheck", _) ->
		{ok, {text, "alive"}}.


#### login_controller.erl

		-module(login_controller).
		-include_lib("lotus_ws/include/lotus_ws.hrl").
		-export([
			post/4,
			enter/1
		]).

	rules() -> 
		[lotus_ws_validator:new_rule(<<"username">>, [required]),
		 lotus_ws_validator:new_rule(<<"password">>, [required])].	

	validate(#ctx{ req = #req{} = Req } = Ctx, JsonBody, ok) -> 
		Ctx#ctx{ req = Req#req { body = JsonBody }  };
	validate(_, _, [{errors, _}] = Errors) -> 
		{400, [{json, Errors}]};
	validate(Ctx, JsonBody, Rules) -> 
		validate(Ctx, JsonBody, lotus_ws_validator:validate(JsonBody, Rules)).

	invalid_json() -> {500, [{json, [{error, <<"invalid body content">>}]}]}.
	unauthorized() -> {401, [{json, [{message, <<"unauthorized">>}]}]}.

	body_parse(Ctx, true, Body, Rules) -> validate(Ctx, jsx:decode(Body), Rules);
	body_parse(_, false, _, _) -> invalid_json();
	body_parse(_, {incomplete, _}, _, _) -> invalid_json().


	login_resp(unauthorized) -> unauthorized();
	login_resp(#muser{token = Token}) -> {200, [{json, [{token, Token}]}]};
	login_resp(norows) -> unauthorized();
	login_resp({error, Reason}) -> {500, [{json, [{message, Reason}]}]}.


	enter(#ctx{ req = #req{ body = Body } } = Ctx) ->	
		body_parse(Ctx, jsx:is_json(Body), Body, rules()).

	post("/api/auth", _, _, Body) ->	
		#{ <<"username">> := Username, <<"password">> := Password } = Body,
		Result = auth_service:login(Username, Password),
		login_resp(Result).

#### messages_controller.erl

	-module(chat_message_controller).

	-include_lib("lotus_ws/include/lotus_ws.hrl").

	-export([
		enter/1,
		leave/1,
		post/2
	]).

	enter(#ctx{ req = #req { body = Body }} = Ctx) ->
		validate(Ctx, Body).

	leave(#ctx{} = Ctx) ->
		Ctx.

	get_user(#auth { data = #{ user := User }}) -> User.

	post("/api/message/read", #ctx{ req = #req { body = Body }}) ->
		Ids = maps:get(<<"ids">>, Body, []),
		 % update messages by ids..
		{ok, #{json => #{ status => ok }}};

	post("/api/message/received", #ctx{ req = #req { body = Body }}) ->
		Ids = maps:get(<<"ids">>, Body, []),
		% update messages by ids..
		{ok, #{json => #{ status => ok }}};

	post("/api/message", #ctx{ req = #req { body = Body }, auth = Auth }) ->

		#{<<"limit">> := Limit
		, <<"offset">> := Offset} = Body,

		User = get_user(Auth),	% #muser{}
		Messages = find_messages_by_user(User),
		{200, [{json, Messages}]}.


	%% validations
	rules() -> [
		, lotus_ws_validator:new_rule(<<"offset">>, #{required => true})
		, lotus_ws_validator:new_rule(<<"limit">>, #{required => true})
		].	

	validate(Ctx, JsonBody) -> validate(Ctx, JsonBody, rules()).
	validate(Ctx, _, ok) -> Ctx;
	validate(_, _, [{errors, _}] = Errors) -> {400, [{json, Errors}]};
	validate(Ctx, JsonBody, Rules) -> validate(Ctx, JsonBody, lotus_ws_validator:validate(JsonBody, Rules)).
