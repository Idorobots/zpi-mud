-module(mud_game).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {locations, players, passwords, items}).

%% This is a tiny wrapper that makes restarts of various Hive HTTP servers predictable and well-behaved.
%% Gen server callbacks:
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    lager:notice("Starting MUD Game..."),
    {ok, load_game_data()}.

terminate(_Reason, _State) ->
    ok.

%% Game API:


%% Gen server handlers:
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Intenal functions:
load_game_data() ->
    todo,
    #state{}.

hash(Password) ->
    base64:encode(crypto:hash(sha, Password)).

publish(Channel, Message) ->
    todo.
