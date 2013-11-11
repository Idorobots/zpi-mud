-module(mud_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

%% API functions
start_link() ->
    supervisor:start_link(?MODULE, []).

%% Supervisor callbacks
init([]) ->
    Strategy = {one_for_one, 5, 3600},
    Processes = [{mud_game,
                  {mud_game, start_link, [self()]},
                  temporary,
                  5000,
                  worker,
                  [mud_game]},
                 {mud_serv,
                  {mud_serv, start_link, []},
                  temporary,
                  5000,
                  worker,
                  [mud_serv]}],
    {ok, {Strategy, Processes}}.

