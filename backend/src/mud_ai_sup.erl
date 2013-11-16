-module(mud_ai_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

%% API functions
start_link() ->
    supervisor:start_link(?MODULE, []).

%% Supervisor callbacks
init([]) ->
    Strategy = {simple_one_for_one, 5, 3600},
    Processes = [{mud_ai,
                  {mud_ai, start_link, []},
                  temporary,
                  5000,
                  worker,
                  [mud_ai]}],
    {ok, {Strategy, Processes}}.
