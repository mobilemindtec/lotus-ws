-module(lotus_ws_service).
-include("../include/lotus_ws.hrl").
-behaviour(gen_server).
-export([
	start_link/0, 
	init/1, 
	handle_call/3, 
	handle_cast/2, 
	handle_info/2, 
	terminate/2, 
	code_change/3]).

-export([
	handle/1
	]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	{ok, #{}}.

-spec handle(Ctx) -> Ctx when Ctx :: ctx().

handle(#ctx{} = Ctx) ->
	gen_server:call(?MODULE, {handle, Ctx}).

handle_call({handle, #ctx{} = Ctx}, _From, State) ->
	NewCtx = lotus_ws_handler:handle(Ctx),
	{reply, {ok, NewCtx}, State}.


handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.