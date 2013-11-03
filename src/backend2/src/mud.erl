-module(mud).

-export([start/0, start/1, start/2, stop/0, stop/1]).
-export([get_env/1]).

start() ->
    start([<<".">>]).

start([ResourcesDir]) ->
    crypto:start(),
    ssl:start(),
    lager:start(),
    ibrowse:start(),
    application:start(ranch),
    application:start(cowboy),
    set_env(resources, list_to_binary(atom_to_list(ResourcesDir))),
    application:start(mud).

start(_StartType, _StartArgs) ->
    mud_sup:start_link().

stop() ->
    stop(ignored).

stop(_Type) ->
    application:start(cowboy),
    application:start(ranch),
    application:stop(mud).

get_env(Key) ->
    {ok, Value} = application:get_env(?MODULE, Key),
    Value.

set_env(Key, Value) ->
    application:set_env(?MODULE, Key, Value).
