%% The main entry point of the application.
%% Starts the backend Game server & the midend Hive session management server.

-module(mud).
-author('kajtek@idorobots.org').

-export([start/0, start/1, start/2, stop/0, stop/1]).
-export([get_env/1]).

%% Application start/stop management functions:
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
    %% NOTE Hive requires the backend to be running before it can be started.
    Ret = application:start(mud),
    %% NOTE Since Hive is used as a dependancy we need to explicitly link it to
    %% NOTE its various resources.
    hive:start_dev(['./deps/hive/plugins', './deps/hive/etc/schema', './config/config.json']),
    Ret.

start(_StartType, _StartArgs) ->
    mud_sup:start_link().

stop() ->
    stop(ignored).

stop(_Type) ->
    application:start(cowboy),
    application:start(ranch),
    application:stop(mud).

%% Application management functions (provide a convenient configuration access):
get_env(Key) ->
    {ok, Value} = application:get_env(?MODULE, Key),
    Value.

set_env(Key, Value) ->
    application:set_env(?MODULE, Key, Value).
