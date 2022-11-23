%% vi:ts=4 sw=4 et

-include_lib("eunit/include/eunit.hrl").

-define(CONTENT_TYPE_JSON, #{<<"content-type">> => <<"application/json">>}).
-define(CONTENT_TYPE_TEXT, #{<<"content-type">> => <<"text/plain">>}).
-define(CONTENT_TYPE_HTML, #{<<"content-type">> => <<"text/html">>}).

-define(HandlerFn, 			fun((ctx()) -> ctx() | atom())).

-define(ActionFn1, 			fun((string()) -> ctx() | resp() | tuple())).
-define(ActionFn2, 			fun((string(), ctx()) -> ctx() | resp() | tuple())).
-define(ActionFn3, 			fun((string(), ctx(), req()) -> ctx() | resp() | tuple())).
-define(ActionFn4, 			fun((string(), ctx(), req(), any()) -> ctx() | resp() | tuple())).


-record(req, {body 																					:: any()
						, path 																					:: string()
						, method 																				:: binary() % get, post, put, delete, patch			
						, headers 																			:: map()	
						, params 						 	= #{}	                		:: map()
						, queries						 	= #{}	                		:: map()
						, req 												    							:: cowboy_req:req()}).

-type req()  																								:: #req{}.

-record(resp, {body 																				:: any()
							, headers					 	= #{}											:: map()
							, status																			:: integer()}).

-type resp() 																								:: #resp{}.


-record(auth, {username 			 															:: string()
							, token   				 														:: string()
							, roles        			= [] 										  :: list(atom())
							, data							= #{}										  :: map()}).

-type auth() 																								:: #auth{}.

-record(login, {username 																		:: string()
							, password 																		:: string()}).

-type login() 																							:: #login{}.													 

-record(ctx, {req  				 				= undefined								:: req()
						,	resp 				 				= undefined              	:: resp()
						,	error 					 		= false										:: boolean()
						,	auth 						 															:: auth()
						,	data 						 		= #{}											:: map()
						,	route 																				:: route()
						,	authenticator    		= undefined              	:: ?HandlerFn | atom()}).

-type ctx() 																								:: #ctx{}.


-record(middleware, {name 																	:: string()
									 , description 														:: string()
									 , enter				 = undefined							:: ?HandlerFn
									 , leave				 = undefined							:: ?HandlerFn
									 , error				 = undefined							:: ?HandlerFn
									 , handler 			 = undefined	  					:: atom()}).


-type middleware() 																					:: #middleware{}.

-record(middlewares, {values 			 = [] 										:: list(atom())
										, bypass 			 = [] 										:: list(atom())
										, handlers 		 = []											:: list(atom())}).

-type middlewares() 																				:: #middlewares{}.

-record(route, {name 																				:: atom()
							,	path 																				:: string()
							,	compiled_path																:: string()
							,	fn 								 = undefined							:: ?ActionFn1 | ?ActionFn2 | ?ActionFn3 | ?ActionFn4
							,	handler            = undefined							:: atom()
							,	middlewares 			 = #middlewares{}					:: middlewares() | list(atom())		
							,	roles              = []                     :: atom()
							,	params 						 = #{}	                  :: map()
							,	routes 						 = []   									:: list(route())
							,	defaults           = []                     :: list(atom())}).

-type route() 																							:: #route{}.

-record(router, {routes 			 		 = [] 										:: list(route())
							 , middlewares 	 		 = [] 										:: list(middleware())
							 , authenticator 		 = undefined              :: ?HandlerFn | atom()
							 , debug             = false                  :: boolean()}).


-type router()																					    :: #router{}.



-record(vrule, {field 																		  :: string()
							, rules 						 = []										  :: list()}).

-type vrule() 																						  :: #vrule{}.