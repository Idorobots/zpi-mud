-module(generic_npc).

-export([init/1, on_friendly/3, on_neutral/3, on_hostile/3]).

-include("mud_ai.hrl").

%% NPC callbacks:
init(State) ->
    {ok, neutral, State}.

on_friendly(Name, Args, State) ->
    lager:debug("Received an event: ~s", [Name]),
    {ok, friendly, State}.

on_neutral(Name, Args, State) ->
    lager:debug("Received an event: ~s", [Name]),
    {ok, neutral, State}.

on_hostile(Name, Args, State) ->
    lager:debug("Received an event: ~s", [Name]),
    {ok, hostile, State}.
