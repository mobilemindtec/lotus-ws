%%%-------------------------------------------------------------------
%%% @author ricardo
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(lotus_ws_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(RouterConfig) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, RouterConfig).

init(RouterConfig) ->
  %% Starting simple, and we'll be dynamically adding pools
  SupFlags = #{
    strategy=>one_for_one,
    intensity=>2000,
    period=>1
  },
  ChildSpecs = [
    #{id => lotus_ws_router,
      start => {lotus_ws_router, start_link, RouterConfig},
      restart => permanent,
      shutdown => brutal_kill,
      type => worker,
      modules => [lotus_ws_router]}
    ],
  {ok, {SupFlags, ChildSpecs}}.
