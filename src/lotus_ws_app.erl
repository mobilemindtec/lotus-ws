%%%-------------------------------------------------------------------
%%% @author ricardo
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(lotus_ws_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lotus_ws_sup:start_link().

stop(_State) ->
    ok.
