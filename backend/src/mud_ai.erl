-module(mud_ai).
-behaviour(gen_fsm).

-export([start_link/2, init/1, terminate/3]).
-export([code_change/4, handle_event/3, handle_sync_event/4, handle_info/3]).
-export([transient/2, friendly/2, neutral/2, hostile/2]).

-import(mud_utils, [prop/2, mk_event/2, str_cat/2, data/2, data/1]).

-include("mud_ai.hrl").

%% Gen FSM callbacks:
start_link(Nick, NPC) ->
    gen_fsm:start_link(?MODULE, {Nick, NPC}, []).

init({Nick, NPC}) ->
    Password = prop(<<"password">>, NPC),
    Location = prop(<<"location">>, NPC),
    Type = prop(<<"npc">>, NPC),
    Handler = get_handler(Type),
    gen_fsm:send_event(self(), {authorize, Nick, Password, Location}),
    {ok, transient, #state{
                       handler = Handler,
                       nick = Nick,
                       target = <<"">>,
                       state = [{<<"nick">>, Nick},
                                {<<"location">>, Location},
                                {<<"npc">>, Type}]
                      }}.

terminate(_Reason, _StateName, State) ->
    mud_game:cleanup(data(State#state.state)).

%% Gen FSM handlers:
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {next_state, StateName, State}.

handle_info(Info, StateName, State) ->
    Handler = State#state.handler,
    {ok, NewStateName, NewState} = Handler:on_info(Info, StateName, State),
    {next_state, NewStateName, NewState}.

code_change(_Vsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% Gen FSM states:
transient({authorize, Nick, Password, Location}, State) ->
    Auth = mk_event(<<"authorize">>,
                    [[{<<"nick">>, Nick},
                      {<<"password">>, Password}]]),
    %% NOTE We'll spoof the Hive client for a bit...
    ok = hive_pubsub:join([str_cat(mud:get_env(channel_prefix), Location)]),
    {ok, _Reply} = mud_game:authorize(data(Auth, State#state.state)),
    Handler = State#state.handler,
    {ok, StateName, NewState} = Handler:init(State),
    {next_state, StateName, NewState};

transient(Event, State) ->
    lager:warning("Received an unhandled event: ~p", [Event]),
    {next_state, transient, State}.

friendly({dispatch_events, Event}, State) ->
    Handler = State#state.handler,
    JSON = jsonx:decode(preprocess(Event), [{format, proplist}]),
    Name = prop(<<"name">>, JSON),
    Args = prop(<<"args">>, JSON),
    {ok, StateName, NewState} = Handler:on_friendly(Name, Args, State),
    {next_state, StateName, NewState}.

neutral({dispatch_events, Event}, State) ->
    Handler = State#state.handler,
    JSON = jsonx:decode(preprocess(Event), [{format, proplist}]),
    Name = prop(<<"name">>, JSON),
    Args = prop(<<"args">>, JSON),
    {ok, StateName, NewState} = Handler:on_neutral(Name, Args, State),
    {next_state, StateName, NewState}.

hostile({dispatch_events, Event}, State) ->
    Handler = State#state.handler,
    JSON = jsonx:decode(preprocess(Event), [{format, proplist}]),
    Name = prop(<<"name">>, JSON),
    Args = prop(<<"args">>, JSON),
    {ok, StateName, NewState} = Handler:on_hostile(Name, Args, State),
    {next_state, StateName, NewState}.

%% Internal functions:
get_handler(Type) ->
    known_type(list_to_atom(binary_to_list(Type))).

known_type(_Type) ->
    generic_npc.

preprocess(<<"5:::", JSON/binary>>) ->
    JSON;

preprocess(Otherwise) ->
    lager:warning("Unrecognized event: ~p", [Otherwise]),
    <<"{}">>.
