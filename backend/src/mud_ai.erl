%% NOTE Licensed under the MIT license. See included LICENSE file for details.

%% A generic module providing AI controler for the characters.
%% Can be parameterized by a specific, swappable handler module - AI is fully scriptable using Erlang.
%% Currently AI scripts assume to handle various game events while being in a certain state
%% (one of 'friendly', 'neutral' or 'hostile'). AI also supports other asynchronous events that
%% don't depend on "friendliness" of a character.

%% NOTE This module spoofs the Hive client behaviour,
%% NOTE meaning it acts on the same messages & returns specifically prepared values.

-module(mud_ai).
-behaviour(gen_fsm).
-author('kajtek@idorobots.org').

-export([start_link/2, init/1, terminate/3]).
-export([code_change/4, handle_event/3, handle_sync_event/4, handle_info/3]).
-export([transient/2, friendly/2, neutral/2, hostile/2, subscribe/2, unsubscribe/2]).

-import(mud_utils, [prop/2, mk_event/2, str_cat/2, data/2, data/1, update/3]).

-include("mud_ai.hrl").

%% Gen FSM callbacks:
start_link(Nick, NPC) ->
    %% FIXME Creating atoms at run time is bad mojo...
    gen_fsm:start_link({local, list_to_atom(binary_to_list(Nick))}, ?MODULE, {Nick, NPC}, []).

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

%% External functions (AI API):

%% Used to spoof Hive clients Pub-Sub subscriptions.
%% More info available in the Hive docs.
subscribe(Sid, Channel) ->
    gen_fsm:send_all_state_event(Sid, {subscribe, Channel}).

%% Used to spoof Hive clients Pub-Sub unsubscriptions.
%% More info available in the Hive docs.
unsubscribe(Sid, Channel) ->
    gen_fsm:send_all_state_event(Sid, {unsubscribe, Channel}).

%% Gen FSM handlers:
handle_event({subscribe, Location}, StateName, State) ->
    ok = hive_pubsub:join([str_cat(mud:get_env(channel_prefix), Location)]),
    {next_state, StateName, State#state{state = update(<<"location">>, Location, State#state.state)}};

handle_event({unsubscribe, Location}, StateName, State) ->
    ok = hive_pubsub:leave([str_cat(mud:get_env(channel_prefix), Location)]),
    {next_state, StateName, State#state{state = update(<<"location">>, <<"">>, State#state.state)}};

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

%% Temporary, startup state which only performs character authorization.
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

%% When the NPC is friendly towards other characters.
friendly({dispatch_events, Event}, State) ->
    Handler = State#state.handler,
    JSON = jsonx:decode(preprocess(Event), [{format, proplist}]),
    Name = prop(<<"name">>, JSON),
    Args = prop(<<"args">>, JSON),
    {ok, StateName, NewState} = Handler:on_friendly(Name, Args, State),
    {next_state, StateName, NewState}.

%% When the NPC is neutral towards other characters.
neutral({dispatch_events, Event}, State) ->
    Handler = State#state.handler,
    JSON = jsonx:decode(preprocess(Event), [{format, proplist}]),
    Name = prop(<<"name">>, JSON),
    Args = prop(<<"args">>, JSON),
    {ok, StateName, NewState} = Handler:on_neutral(Name, Args, State),
    {next_state, StateName, NewState}.

%% When the NPC is hostile towards other characters.
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

%% NOTE ATM only ony type of NPC behaviour is accepted.
%% FIXME Deal with it. *sunglasses*
known_type(_Type) ->
    generic_npc.

preprocess(<<"5:::", JSON/binary>>) ->
    JSON;

preprocess(Otherwise) ->
    lager:warning("Unrecognized event: ~p", [Otherwise]),
    <<"{}">>.
