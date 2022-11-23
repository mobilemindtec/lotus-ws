-module(lotus_ws_html).

-export([
	tag/2,
	tag/3,
	html/1,
	html/2,
	head/1,
	head/2,
	body/1,
	body/2,
	p/1,
	p/2,
	span/1,
	span/2,
	panel/1,
	panel/2,
	a/1,
	a/2,
	h1/1,
	h1/2,
	h2/1,
	h2/2,
	h3/1,
	h3/2,
	h4/1,
	h4/2,
	h5/1,
	h5/2,
	h6/1,
	h6/2,
	hr/1,
	hr/2,
	br/1,
	ul/1,
	ul/2,
	li/1,
	li/2,
	text/1,
	button/1,
	button/2
]).

-record(attr, {name 	:: string(),
							 value 	:: string()}).


tag(text, Contents, _, _) -> Contents;


tag(Tag, Contents, AttrsStr, []) ->
	lists:join("", ["<", Tag, AttrsStr, ">"] ++ Contents ++ ["</", Tag, ">"]);

tag(Tag, Contents, AttrsStr, [#attr{ value = Val, name = Name }|T]) ->	
	NewAttrsStr = lists:join("", [AttrsStr, Name, "=", "'", Val, "' "]),	
	tag(Tag, Contents, NewAttrsStr, T).

tag(Tag, Contents) ->
	tag(Tag, Contents, "", []).

tag(Tag, Contents, Attrs) ->
	tag(Tag, Contents, "", Attrs).

html(Contents) -> tag("html", Contents).
html(Contents, Attrs) -> tag("html", Contents, Attrs).

head(Contents) -> tag("head", Contents).
head(Contents, Attrs) -> tag("head", Contents, Attrs).

body(Contents) -> tag("body", Contents).
body(Contents, Attrs) -> tag("body", Contents, Attrs).

p(Contents) -> tag("p", Contents).
p(Contents, Attrs) -> tag("p", Contents, Attrs).

span(Contents) -> tag("span", Contents).
span(Contents, Attrs) -> tag("span", Contents, Attrs).

panel(Contents) -> tag("panel", Contents).
panel(Contents, Attrs) -> tag("panel", Contents, Attrs).

a(Contents) -> tag("a", Contents).
a(Contents, Attrs) -> tag("a", Contents, Attrs).

h1(Contents) -> tag("h1", Contents).
h1(Contents, Attrs) -> tag("h1", Contents, Attrs).

h2(Contents) -> tag("h2", Contents).
h2(Contents, Attrs) -> tag("h2", Contents, Attrs).

h3(Contents) -> tag("h3", Contents).
h3(Contents, Attrs) -> tag("h3", Contents, Attrs).

h4(Contents) -> tag("h4", Contents).
h4(Contents, Attrs) -> tag("h4", Contents, Attrs).

h5(Contents) -> tag("h5", Contents).
h5(Contents, Attrs) -> tag("h5", Contents, Attrs).

h6(Contents) -> tag("h6", Contents).
h6(Contents, Attrs) -> tag("h6", Contents, Attrs).

button(Contents) -> tag("button", Contents).
button(Contents, Attrs) -> tag("button", Contents, Attrs).

hr(Contents) -> tag("hr", Contents).
hr(Contents, Attrs) -> tag("hr", Contents, Attrs).

br(Contents) -> tag("br", Contents).

ul(Contents) -> tag("ul", Contents).
ul(Contents, Attrs) -> tag("ul", Contents, Attrs).

li(Contents) -> tag("li", Contents).
li(Contents, Attrs) -> tag("li", Contents, Attrs).

text(Contents) -> tag(text, Contents).
