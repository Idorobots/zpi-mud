-module(mud_serv).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0]).

-record(state, {server_listener}).

%% Gen server callbacks:
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    case ?MODULE:start() of
        {ok, Listener} -> {ok, #state{server_listener = Listener}};
        {error, Error} -> {stop, Error}
    end.

terminate(_Reason, State) ->
    cowboy:stop_listener(State#state.server_listener).

%% Gen server handlers:
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Cowboy callbacks:
start() ->
    lager:notice("Starting MUD Server..."),
    Dispatch = cowboy_router:compile([{'_', [{"/:endpoint[/]", mud_handler, []}]}]),
    cowboy:start_http(?MODULE, mud:get_env(acceptors),
                      [{port, mud:get_env(port)},
                       {max_connections, 1000000}], %% Go nuts
                      [{env, [{dispatch, Dispatch}]}]).
