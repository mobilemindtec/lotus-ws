-module(lotus_ws_http_utils).

-include("include/lotus_ws.hrl").

-export([
	not_found/1,
	server_error/1,
	server_error/2,
	bad_req/1,
	bad_req/2,
	handle_resp/2,
	handle_resp/1,
	cowboy_resp/2,
	get_cowboy_req_body/1,
	new_ctx_error/2,
	new_ctx_400/2,
	new_ctx_401/2,
	find_default_content_type/1,
	add_resp_default_content_type/1,
	new_resp/2,
	new_resp/3,
	unauthorized_json/0
	%resp_error/1,
	%ctx_maybe_error/1
]).

unauthorized_json() -> {401, [{json, [{message, "unauthorized"}]}]}.

not_found(Headers) when is_map(Headers) -> not_found(lotus_ws_utils:get_content_type_atom(Headers));
not_found(json) -> {404, [{json, [{message, "Not Found"}]}]};
not_found(text) -> {404, [{text, "Not Found"}]};
not_found(html) ->
	Html = html_tpl:html(
		html_tpl:body(
			html_tpl:h1("Not Found")
		)
	),
	{404, [{html, Html}]};	

not_found(_) -> 404.	


server_error(Headers) -> server_error(Headers, "").
server_error(Headers, ErrorDetail) when is_map(Headers) -> server_error(lotus_ws_utils:get_content_type_atom(Headers), ErrorDetail);
server_error(Headers, ErrorDetail) when is_map(Headers) -> server_error(lotus_ws_utils:get_content_type_atom(Headers), ErrorDetail);
server_error(json, ErrorDetail) -> {500, [{json, [{message, "Server Error"}, {detail, ErrorDetail}]}]};
server_error(text, ErrorDetail) -> {500, [{text, "Server Error. Detail: " ++ ErrorDetail}]};
server_error(html, ErrorDetail) ->
	Html = html_tpl:html(
		html_tpl:body([
			html_tpl:h1("Server Error"),
			html_tpl:h3(ErrorDetail)
		])
	),
	{500, [{html, Html}]};	

server_error(_, _) -> 500.	

bad_req(Headers) -> bad_req(Headers, "").
bad_req(Headers, ErrorDetail) when is_map(Headers) -> bad_req(lotus_ws_utils:get_content_type_atom(Headers), ErrorDetail);
bad_req(Headers, ErrorDetail) when is_map(Headers) -> bad_req(lotus_ws_utils:get_content_type_atom(Headers), ErrorDetail);
bad_req(json, ErrorDetail) -> {400, [{json, [{message, "Bad Request Error"}, {data, ErrorDetail}]}]};
bad_req(text, ErrorDetail) -> {400, [{text, "Bad Request. Detail: " ++ ErrorDetail}]};
bad_req(html, ErrorDetail) ->
	Html = html_tpl:html(
		html_tpl:body([
			html_tpl:h1("Server Error"),
			html_tpl:h3(ErrorDetail)
		])
	),
	{400, [{html, Html}]};	

bad_req(_, _) -> 400.	

handle_resp(#ctx{} = Ctx, {200, [{text, ResponseBody}]}) ->
	Ctx#ctx { resp = #resp { status = 200, body = ResponseBody, headers = ?CONTENT_TYPE_TEXT }};

handle_resp(#ctx{} = Ctx, {200, [{json, ResponseBody}]}) ->
	Ctx#ctx { resp = #resp { status = 200, body = lotus_ws_json:encode(ResponseBody), headers = ?CONTENT_TYPE_JSON }};

handle_resp(#ctx{} = Ctx, {200, [{html, ResponseBody}]}) ->
	Ctx#ctx { resp = #resp { status = 200, body = ResponseBody, headers = ?CONTENT_TYPE_HTML }};

handle_resp(#ctx{} = Ctx, {Status, [{json, ResponseBody}]}) ->
	Ctx#ctx { error = Status =/= 200, resp = #resp { status = Status, body = lotus_ws_json:encode(ResponseBody), headers = ?CONTENT_TYPE_JSON }};

handle_resp(#ctx{} = Ctx, {Status, [{html, ResponseBody}]}) ->
	Ctx#ctx { error = Status =/= 200, resp = #resp { status = Status, body = ResponseBody, headers = ?CONTENT_TYPE_HTML }};

handle_resp(#ctx{} = Ctx, {Status, [{text, ResponseBody}]}) ->
	Ctx#ctx { error = Status =/= 200, resp = #resp { status = Status, body = ResponseBody, headers = ?CONTENT_TYPE_TEXT }};

handle_resp(#ctx{} = Ctx, {Status, [{body, ResponseBody}, {headers, Headers}]}) ->
	Ctx#ctx { error = Status =/= 200, resp = #resp { status = Status, body = ResponseBody, headers = maps:merge(find_default_content_type(Ctx), Headers) }};

handle_resp(#ctx{} = Ctx, {Status, [{auto, ResponseBody}]}) ->
	TypeRender = find_default_content_type(Ctx),	
	handle_resp(Ctx, {Status, [{TypeRender, ResponseBody}]});

handle_resp(#ctx{} = Ctx, {Status, [{auto, ResponseBody}, {headers, Headers}]}) ->
	TypeRender = lotus_ws_utils:get_content_type_atom(maps:merge(find_default_content_type(Ctx), Headers)),
	handle_resp(Ctx, {Status, [{TypeRender, ResponseBody}, {headers, Headers}]});

handle_resp(#ctx{} = Ctx, 500) ->
	Ctx#ctx { error = true, resp = #resp { status = 500, body = <<"Server Error. No content-type in req">>, headers = ?CONTENT_TYPE_TEXT }};

handle_resp(_, #ctx{ resp = undefined } = Ctx) -> Ctx;

handle_resp(_, #ctx{ resp = #resp { headers = Headers }} = Ctx) ->
	NewHeaders = maps:merge(find_default_content_type(Ctx), Headers),
	Response = Ctx#ctx.resp,
	Ctx#ctx { resp = Response#resp { headers = NewHeaders }, error = Response#resp.status =/= 200};

handle_resp(#ctx{} = Ctx, #resp { headers = Headers } = Response) ->
	NewHeaders = maps:merge(find_default_content_type(Ctx), Headers),
	Ctx#ctx { resp = Response#resp { headers = NewHeaders }, error = Response#resp.status =/= 200 }.

handle_resp(#ctx{} = Ctx) ->
	handle_resp(Ctx, Ctx).

cowboy_resp(#ctx{ resp = #resp { status = Status, headers = Headers, body = Body }}, Req) ->
	cowboy_req:reply(Status, Headers, Body, Req);

cowboy_resp(#ctx{ resp = undefined, error = true}, Req) ->
	lager:info("ctx error, resp not  found"),
	cowboy_req:reply(500, ?CONTENT_TYPE_TEXT, "Server Error", Req).

get_cowboy_req_body(Req) ->
	HasBody = cowboy_req:has_body(Req),
	if 
		HasBody ->
			{ok, ReqBody, _} = cowboy_req:read_body(Req),
			ReqBody;
		true ->
			nil
	end.

find_default_content_type(#ctx{ req = #req { headers = Headers }}) ->	
	case lotus_ws_utils:get_content_type(Headers) of
		undefined -> ?CONTENT_TYPE_TEXT;
		ContentType -> #{<<"content-type">> => ContentType }
	end.

add_ctx_default_content_type(#ctx{ resp = #resp{} = Resp, req = #req { headers = Headers } } = Ctx) ->			
	Ctx#ctx { resp = Resp#resp { headers = maps:merge(find_default_content_type(Ctx), Headers) } }.	

add_resp_default_content_type(#ctx{ resp = #resp{} = Resp, req = #req { headers = Headers } } = Ctx) ->			
	Ctx#ctx { resp = Resp#resp { headers = maps:merge(find_default_content_type(Ctx), Headers) } }.	

new_resp(Status, Body) ->
	#resp { status = Status, body = Body }.

new_resp(Status, Body, Headers) ->
	#resp { status = Status, body = Body, headers = Headers }.

new_ctx_error(#ctx{} = Ctx, Body) ->
	NewCtx = Ctx#ctx { 
								error = true, 
								resp = new_resp(500, Body) },
	add_ctx_default_content_type(NewCtx).	

new_ctx_400(#ctx{} = Ctx, Body) ->
	NewCtx = Ctx#ctx { 
								error = true, 
								resp = new_resp(400, Body) },
	add_ctx_default_content_type(NewCtx).		

new_ctx_401(#ctx{} = Ctx, Body) ->
	NewCtx = Ctx#ctx { 
								error = true, 
								resp = new_resp(401, Body) },
	add_ctx_default_content_type(NewCtx).			

%resp_error(#ctx{ resp = undefined }) -> false;
%resp_error(#ctx{ resp = #resp { status = 200} }) -> true;
%resp_error(#ctx{ resp = #resp{} }) -> false.


%ctx_maybe_error(#ctx{ resp = undefined } = Ctx) -> Ctx;
%ctx_maybe_error(#ctx{ resp = #resp { status = 200} } = Ctx) -> Ctx;
%ctx_maybe_error(#ctx{ resp = #resp{} } = Ctx) -> Ctx#ctx{ error = true }.
