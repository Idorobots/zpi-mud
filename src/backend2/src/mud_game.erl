-module(mud_game).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([authorize/1, say/1, do/1, cleanup/1]).

-import(mud_utils, [hash/1, publish/2, sid/1, trigger/1, state/1, file_to_json/1, prop/2]).
-import(mud_utils, [mk_error/1, mk_reply/2, mk_store/1, update/3, subscribe/2, unsubscribe/2, remove/2]).

-record(state, {locations, players, passwd, items}).

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
authorize(Data) ->
    Trigger = trigger(Data),
    [Args] = prop(<<"args">>, Trigger),
    case prop(<<"password">>, Args) of
        null     -> gen_server:call(?MODULE, {new_character, prop(<<"nick">>, Args), sid(Data)});
        Password -> gen_server:call(?MODULE, {log_in, prop(<<"nick">>, Args), Password, sid(Data)})
    end.

say(Data) ->
    Trigger = trigger(Data),
    [Args] = prop(<<"args">>, Trigger),
    gen_server:cast(?MODULE, {say, prop(<<"type">>, Args),
                              prop(<<"text">>, Args),
                              state(Data)}).

do(Data) ->
    Trigger = trigger(Data),
    [Args] = prop(<<"args">>, Trigger),
    gen_server:call(?MODULE, {do, prop(<<"action">>, Args),
                              prop(<<"args">>, Args),
                              sid(Data),
                              state(Data)}).

cleanup(Data) ->
    gen_server:cast(?MODULE, {cleanup, sid(Data), state(Data)}).

%% Gen server handlers:
handle_call({new_character, Nick, Sid}, _From, State) ->
    case validate_nick(Nick) of
        true ->
            Password = new_password(Sid),
            LocationID = mud:get_env(starting_location),
            Location = prop(LocationID, State#state.locations),
            Character = new_character(Nick),
            Reply = [mk_store([{<<"nick">>, Nick},
                               {<<"location">>, LocationID}]),
                     mk_reply(<<"authorize">>, [[{<<"permission">>, <<"granted">>},
                                                 {<<"password">>, Password}]]),
                     mk_reply(<<"location_info">>, [Location]),
                     mk_reply(<<"character_info">>, [Character])],
            {reply, {ok, Reply}, join(Nick,
                                      Sid,
                                      LocationID,
                                      add_character(Character, Nick, Password, LocationID, State))};

        false ->
            Reply = [mk_reply(<<"authorize">>, [[{<<"permission">>, null}]]),
                     mk_error(<<"Selected nickname is invalid!">>)],
            {reply, {ok, Reply}, State}
    end;

handle_call({log_in, Nick, Password, Sid}, _From, State) ->
    Hashed = hash(Password),
    case prop(Nick, State#state.passwd) of
        null ->
            {reply, {ok, mk_reply(<<"authorize">>, [[{<<"permission">>, null}]])}, State};

        Player ->
            case prop(<<"password">>, Player) of
                Hashed ->
                    LocationID = prop(<<"location">>, Player),
                    Location = prop(LocationID, State#state.locations),
                    Character = prop(Nick, State#state.players),
                    Reply = [mk_store([{<<"nick">>, Nick},
                                       {<<"location">>, LocationID}]),
                             mk_reply(<<"authorize">>, [[{<<"permission">>, <<"granted">>}]]),
                             mk_reply(<<"location_info">>, [Location]),
                             mk_reply(<<"character_info">>, [Character])],
                    {reply, {ok, Reply}, join(Nick, Sid, LocationID, State)};

                _Otherwise ->
                    {reply, {ok, mk_reply(<<"authorize">>, [[{<<"permission">>, null}]])}, State}
            end
    end;

handle_call({do, <<"examine">>, ID, _Sid, PlayerState}, _From, State) ->
    Nick = prop(<<"nick">>, PlayerState),
    LocationID = prop(<<"location">>, PlayerState),
    case object_by_id(Nick, LocationID, ID, State) of
        {player, Player, _Where} ->
            {reply, {ok, mk_reply(<<"character_info">>, [Player])}, State};

        {location, Location, _Where} ->
            {reply, {ok, mk_reply(<<"location_info">>, [Location])}, State};

        {item, Item, _Where} ->
            {reply, {ok, mk_reply(<<"item_info">>, [Item])}, State};

        _Otherwise ->
            {reply, {ok, mk_reply(<<"bad_action">>, [<<"You can't examine that!">>])}, State}
    end;

handle_call({do, <<"move">>, Where, Sid, PlayerState}, _From, State) ->
    %% TODO Call a callback with args and game state.
    Nick = prop(<<"nick">>, PlayerState),
    LocationID = prop(<<"location">>, PlayerState),
    CurrLocation = prop(LocationID, State#state.locations),
    Reachable = prop(<<"locations">>, CurrLocation),
    case prop(Where, Reachable) of
        null ->
            {reply, {ok, mk_reply(<<"bad_action">>, [<<"You can't go there!">>])}, State};

        NewLocationID ->
            Reply = [mk_store([{<<"location">>, NewLocationID}]),
                     mk_reply(<<"location_info">>, [prop(NewLocationID, State#state.locations)])],
            {reply, {ok, Reply}, join(Nick, Sid, NewLocationID, leave(Nick, Sid, LocationID, State))}
    end;

handle_call({do, <<"take">>, ID, _Sid, PlayerState}, _From, State) ->
    Nick = prop(<<"nick">>, PlayerState),
    LocationID = prop(<<"location">>, PlayerState),
    Player = prop(Nick, State#state.players),
    Inventory = prop(<<"inventory">>, Player),
    Location = prop(LocationID, State#state.locations),
    Items = prop(<<"items">>, Location),
    case object_by_id(Nick, LocationID, ID, State) of
        {item, Item, LocationID} ->
            Name = prop(<<"name">>, Item),
            Reply = mk_reply(<<"inventory_update">>,
                             [{<<"type">>, <<"take">>},
                              {<<"id">>, ID},
                              {<<"name">>, Name}]),
            NewState = State#state{
                         locations = update(LocationID,
                                            update(<<"items">>, remove(ID, Items), Location),
                                            State#state.locations),
                         players = update(Nick,
                                          update(<<"inventory">>, update(ID, Name, Inventory), Player),
                                          State#state.players)
                        },
            {reply, {ok, Reply}, NewState};

        _Otherwise ->
            {reply, {ok, mk_reply(<<"bad_action">>, [<<"You can't take that!">>])}, State}
    end;

handle_call({do, <<"drop">>, ID, _Sid, PlayerState}, _From, State) ->
    Nick = prop(<<"nick">>, PlayerState),
    LocationID = prop(<<"location">>, PlayerState),
    Player = prop(Nick, State#state.players),
    Inventory = prop(<<"inventory">>, Player),
    Location = prop(LocationID, State#state.locations),
    Items = prop(<<"items">>, Location),
    case object_by_id(Nick, LocationID, ID, State) of
        {item, Item, Nick} ->
            Name = prop(<<"name">>, Item),
            Reply = mk_reply(<<"inventory_update">>,
                             [{<<"type">>, <<"drop">>},
                              {<<"id">>, ID},
                              {<<"name">>, Name}]),
            NewState = State#state{
                         locations = update(LocationID,
                                            update(<<"items">>, update(ID, Name, Items), Location),
                                            State#state.locations),
                         players = update(Nick,
                                          update(<<"inventory">>, remove(ID, Inventory), Player),
                                          State#state.players)
                        },
            {reply, {ok, Reply}, NewState};

        _Otherwise ->
            {reply, {ok, mk_reply(<<"bad_action">>, [<<"You don't have that!">>])}, State}
    end;

handle_call({do, <<"attack">>, Args, Sid, _State}, _From, State) ->
    %% TODO Call a callback with args and game state.
    {reply, ok, State};

handle_call(status, _From, State) ->
    {reply, State, State};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({say, Type, Text, PlayerState}, State) ->
    Nick = prop(<<"nick">>, PlayerState),
    LocationID = prop(<<"location">>, PlayerState),
    publish(LocationID, [{<<"name">>, <<"msg">>},
                         {<<"args">>, [[{<<"nick">>, Nick},
                                        {<<"type">>, Type},
                                        {<<"text">>, Text}]]}]),
    {noreply, State};

handle_cast({cleanup, Sid, PlayerState}, State) ->
    Nick = prop(<<"nick">>, PlayerState),
    LocationID = prop(<<"location">>, PlayerState),
    {noreply, leave(Nick, Sid, LocationID, State)};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Intenal functions:
load_game_data() ->
    Resources = mud:get_env(resources),
    lager:notice("Loading MUD resources: ~p...", [Resources]),
    #state{
       locations = load_locations(filename:join([Resources, <<"locations.json">>])),
       players = load_players(filename:join([Resources, <<"players.json">>])),
       passwd = load_passwords(filename:join([Resources, <<"passwd.json">>])),
       items = load_items(filename:join([Resources, <<"items.json">>]))
      }.

load_locations(File) ->
    lists:map(extractor(<<"id">>), file_to_json(File)).

load_players(File) ->
    lists:map(extractor(<<"nick">>), file_to_json(File)).

load_passwords(File) ->
    file_to_json(File).

load_items(File) ->
    lists:map(extractor(<<"id">>), file_to_json(File)).

extractor(Field) ->
    fun(JSON) ->
            {proplists:get_value(Field, JSON), JSON}
    end.

join(Nick, Sid, LocationID, State) ->
    %% TODO Subscribe Nick to Location, publish player_enters event.
    Location = prop(LocationID, State#state.locations),
    Players = [Nick | prop(<<"players">>, Location)],
    subscribe(Sid, LocationID),
    publish(LocationID, [{<<"name">>, <<"player_enters">>},
                         {<<"args">>, [[{<<"location">>, prop(<<"name">>, Location)},
                                        {<<"nick">>, Nick}]]}]),
    State#state{
      locations = update(LocationID,
                         update(<<"players">>, Players, Location),
                         State#state.locations)
     }.

leave(Nick, Sid, LocationID, State) ->
    %% TODO unsubscribe Nick from Location, publish player_leaves event.
    Location = prop(LocationID, State#state.locations),
    Players = prop(<<"players">>, Location) -- [Nick],
    unsubscribe(Sid, LocationID),
    publish(LocationID, [{<<"name">>, <<"player_leaves">>},
                         {<<"args">>, [[{<<"location">>, prop(<<"name">>, Location)},
                                        {<<"nick">>, Nick}]]}]),
    State#state{
      locations = update(LocationID,
                         update(<<"players">>, Players, Location),
                         State#state.locations)
     }.

add_character(Character, Nick, Password, Location, State) ->
    State#state{players = update(Nick, Character, State#state.players),
                passwd = update(Nick, [{<<"password">>, hash(Password)},
                                       {<<"location">>, Location}], State#state.passwd)}.

new_character(Nick) ->
    [{<<"nick">>, Nick},
     {<<"stats">>, [{<<"health">>, 100},
                    {<<"strength">>, 100},
                    {<<"toughness">>, 100}]},
     {<<"inventory">>, []}].

new_password(Sid) ->
    binary:part(Sid, {0, 10}).

validate_nick(Nick) ->
    byte_size(Nick) < 30.

object_by_id(_Nick, LocationID, LocationID, State) ->
    {location, prop(LocationID, State#state.locations), LocationID};

object_by_id(Nick, LocationID, ID, State) ->
    CurrLocation = prop(LocationID, State#state.locations),
    Players = prop(<<"players">>, CurrLocation),
    Items = prop(<<"items">>, CurrLocation),
    Player = prop(Nick, State#state.players),
    OwnedItems = prop(<<"inventory">>, Player),
    case lists:member(ID, Players) of
        false ->
            case prop(ID, OwnedItems) of
                null  ->
                    case prop(ID, Items) of
                        null  -> nothing;
                        _Item -> {item, prop(ID, State#state.items), LocationID}
                    end;

                _Item ->
                    {item, prop(ID, State#state.items), Nick}
            end;

        true ->
            {player, prop(ID, State#state.players), LocationID}
    end.
