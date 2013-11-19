%% NOTE Licensed under the MIT license. See included LICENSE file for details.

%% A generic AI script for an NPC.
%% NPC using this script will respond to some inputs and attack the player if disturbed.

-module(generic_npc).
-author('kajtek@idorobots.org').

-export([init/1, on_friendly/3, on_neutral/3, on_hostile/3, on_info/3]).

-import(mud_utils, [mk_event/2, mk_action/2, prop/2, data/2]).

-include("mud_ai.hrl").

%% NPC callbacks:

%% Initializes the NPC:
init(State) ->
    {ok, neutral, State}.

%% Handles asynchronous events such as timeouts:
on_info({attack, Who, Delay}, StateName, State) ->
    do_attack(Who, State),
    case State#state.target of
        <<"">> -> ok;
        Target -> attack(Delay, Target, State)
    end,
    {ok, StateName, State};

on_info({say, How, What}, StateName, State) ->
    do_say(How, What, State),
    {ok, StateName, State};

on_info(Info, StateName, State) ->
    {ok, StateName, State}.

%% Handles game events while in 'friendly' state:
on_friendly(<<"battle">>, _Args, State) ->
    say(100, <<"Hmph...">>, State),
    {ok, neutral, State};

on_friendly(<<"msg">>, [Speech], State) ->
    Nick = prop(<<"nick">>, Speech),
    case re:run(prop(<<"text">>, Speech), <<".*(quest|mission|job|gold|adventure).*">>) of
        {match, _} ->
            say(100, <<"I hope you'll get what you are looking for, ", Nick/binary, ".">>, State),
            {ok, friendly, State};

        _ ->
            {ok, friendly, State}
    end;

on_friendly(_Name, _Args, State) ->
    {ok, friendly, State}.

%% Handles game events while in 'neutral' state:
on_neutral(<<"battle">>, [Battle], State) ->
    Nick = State#state.nick,
    Attacker = prop(<<"attacker">>, Battle),
    case prop(<<"defender">>, Battle) of
        Nick ->
            yell(100, <<"What the hell, ", Attacker/binary, "!? Stop that!">>, State),
            case random:uniform(100) >= 50 of
                true  -> attack(700, Attacker, State),
                         {ok, hostile, State};
                false -> {ok, neutral, State}
            end;

        _Otherwise ->
            case random:uniform(100) >= 95 of
                true  -> yell(100, <<"You two, stop that at once!">>, State),
                         attack(800, Attacker, State),
                         {ok, hostile, State};
                false -> {ok, neutral, State}
            end
    end;

on_neutral(<<"msg">>, [Speech], State) ->
    Nick = prop(<<"nick">>, Speech),
    case re:run(prop(<<"text">>, Speech), <<".*(quest|mission|job|gold|adventure).*">>) of
        {match, _} ->
            case random:uniform(100) >= 40 of
                true ->
                    yell(100, <<"Ha, ha, ", Nick/binary, ", you looking for trouble?">>, State),
                    yell(1000, <<"'Cause I can, sure as hell, make that happen...">>, State);
                false ->
                    ok
            end,
            {ok, neutral, State};

        _ ->
            MyNick = State#state.nick,
            case re:run(prop(<<"text">>, Speech), <<".*", MyNick/binary, ".*">>) of
                {match, _} ->
                    say(100, <<"'Sup, ", Nick/binary, "?">>, State),
                    {ok, friendly, State};

                _ ->
                    {ok, neutral, State}
            end
    end;

on_neutral(_Name, _Args, State) ->
    {ok, neutral, State}.

%% Handles game events while in 'hostile' state:
on_hostile(<<"battle">>, [Battle], State) ->
    Nick = State#state.nick,
    Target = State#state.target,
    case prop(<<"attacker">>, Battle) of
        Nick ->
            case random:uniform(100) >= 80 of
                true  -> yell(100, <<"Arrgh!">>, State);
                false -> ok
            end,
            case random:uniform(100) >= 80 of
                true  -> yell(100, <<"Ugh!">>, State);
                false -> ok
            end,
            {ok, hostile, State};

        Target ->
            case random:uniform(100) >= 90 of
                true  -> yell(100, <<"Mmmmaargh! Die ", Target/binary, "!">>, State);
                false -> ok
            end,
            {ok, hostile, State};

        Attacker ->
            case prop(<<"defender">>, Battle) of
                Nick when Target == <<"">> ->
                    yell(100, <<"I'll get you, ", Attacker/binary, "!">>, State),
                    attack(1000, Attacker, State),
                    {ok, hostile, State#state{target = Attacker}};

                Nick ->
                    case random:uniform(100) >= 50 of
                        true  -> yell(100, <<"I'll get you, ", Attacker/binary, "!">>, State);
                        false -> ok
                    end,
                    {ok, hostile, State#state{target = Attacker}};

                _Otherwise ->
                    case random:uniform(100) >= 75 of
                        true  -> yell(100, <<"Grrr...">>, State),
                                 {ok, neutral, State};
                        false -> {ok, hostile, State}
                    end
            end
    end;

on_hostile(<<"msg">>, [Speech], State) ->
    Target = State#state.target,
    case prop(<<"nick">>, Speech) of
        Target ->
            case random:uniform(100) >= 70 of
                true  -> yell(100, <<"You don't say, ", Target/binary, "!">>, State);
                false -> ok
            end,
            {ok, hostile, State};

        _ ->
            {ok, hostile, State}
    end;

on_hostile(<<"player_enters">>, [Info], State) ->
    Nick = prop(<<"nick">>, Info),
    case State#state.target of
        Nick ->
            yell(100, <<"Get over here! I'll kill you, ", Nick/binary, "!">>, State),
            {ok, hostile, State};

        _Otherwise ->
            {ok, hostile, State}
    end;

on_hostile(_Name, _Args, State) ->
    {ok, hostile, State}.

%% Internal functions:
say(Time, What, State) ->
    erlang:send_after(Time, self(), {say, <<"says">>, What}).

yell(Time, What, State) ->
    erlang:send_after(Time, self(), {say, <<"yells">>, What}).

attack(Time, Who, State) ->
    erlang:send_after(Time, self(), {attack, Who, Time}).

do_say(How, What, State) ->
    Say = mk_event(<<"say">>,
                   [[{<<"text">>, What},
                     {<<"type">>, How}]]),
    mud_game:say(data(Say, State#state.state)).

do_attack(Who, State) ->
    Attack = mk_event(<<"do">>,
                      [[{<<"action">>, <<"attack">>},
                        {<<"args">>, Who}]]),
    mud_game:do(data(Attack, State#state.state)).
