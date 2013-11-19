-module(mud_game).
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([authorize/1, say/1, do/1, cleanup/1, npcize/1, npcize/2, move/2, status/0]).

-import(mud_utils, [json_to_file/2, publish/2, sid/1, trigger/1, state/1, file_to_json/1, prop/2, prop/3]).
-import(mud_utils, [mk_error/1, mk_reply/2, mk_store/1, update/3, subscribe/2, unsubscribe/2, remove/2]).
-import(mud_utils, [mk_event/2]).

-record(state, {
          ai_supervisor = undefined,
          online_players = [],
          locations = [],
          starting_location = <<"">>,
          players = [],
          passwd = [],
          items = []
         }).

%% This is a tiny wrapper that makes restarts of various Hive HTTP servers predictable and well-behaved.
%% Gen server callbacks:
start_link(Supervisor) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Supervisor, []).

init(Supervisor) ->
    lager:notice("Starting MUD Game..."),
    random:seed(now()),
    erlang:start_timer(mud:get_env(npc_spawn_timeout), ?MODULE, {spawn_ai_sup, Supervisor}),
    erlang:start_timer(mud:get_env(save_timeout), self(), save_state),
    {ok, load_game_data()}.

terminate(Reason, _State) ->
    lager:notice("Stopping MUD Game: ~p", [Reason]),
    ok.

%% Game API:
authorize(Data) ->
    Trigger = prop(<<"trigger">>, Data),
    [Args] = prop(<<"args">>, Trigger),
    gen_server:call(?MODULE, {authorize,
                              prop(<<"nick">>, Args),
                              prop(<<"password">>, Args),
                              sid(Data)}) .

say(Data) ->
    Trigger = trigger(Data),
    [Args] = prop(<<"args">>, Trigger),
    gen_server:cast(?MODULE, {say,
                              prop(<<"type">>, Args),
                              prop(<<"text">>, Args),
                              state(Data)}).

do(Data) ->
    Trigger = trigger(Data),
    [Args] = prop(<<"args">>, Trigger),
    gen_server:call(?MODULE, {do,
                              prop(<<"action">>, Args),
                              prop(<<"args">>, Args),
                              sid(Data),
                              state(Data)}).

cleanup(Data) ->
    gen_server:cast(?MODULE, {cleanup,
                              sid(Data),
                              state(Data)}).

%% External functions:
npcize(Strategy) ->
    npcize(all, Strategy).

npcize(Who, Strategy) when is_binary(Who) ->
    npcize([Who], Strategy);

npcize(Who, Strategy) when Who == all orelse is_list(Who) ->
    gen_server:cast(?MODULE, {npcize, Who, Strategy}).

move(Who, Where) when is_binary(Who) andalso is_binary(Where) ->
    gen_server:cast(?MODULE, {move, Who, Where}).

status() ->
    gen_server:call(?MODULE, status).

%% Gen server handlers:
handle_call({new_character, Nick, Password, Sid}, _From, State) ->
    case validate_nick(Nick, State) of
        true ->
            LocationID = State#state.starting_location,
            Location = prop(LocationID, State#state.locations),
            Character = new_character(Nick),
            Reply = [mk_store([{<<"nick">>, Nick},
                               {<<"location">>, LocationID}]),
                     mk_reply(<<"authorize">>, [[{<<"permission">>, <<"granted">>}]]),
                     mk_reply(<<"location_info">>, [Location]),
                     mk_reply(<<"character_info">>, [Character])],
            {reply, {ok, Reply}, online(Nick,
                                        join(Nick,
                                             Sid,
                                             LocationID,
                                             add_character(Character,
                                                           Nick,
                                                           Password,
                                                           LocationID,
                                                           State)))};

        false ->
            Reply = [mk_reply(<<"authorize">>, [[{<<"permission">>, null}]]),
                     mk_error(<<"Selected nickname is invalid!">>)],
            {reply, {ok, Reply}, State}
    end;

handle_call({authorize, Nick, Password, Sid}, From, State) ->
    case prop(Nick, State#state.passwd) of
        null ->
            handle_call({new_character, Nick, Password, Sid}, From, State);

        Player ->
            case is_char_online(Nick, State) of
                false ->
                    case prop(<<"password">>, Player) of
                        Password ->
                            LocationID = prop(<<"location">>, Player),
                            Location = prop(LocationID, State#state.locations),
                            Character = prop(Nick, State#state.players),
                            Reply = [mk_store([{<<"nick">>, Nick},
                                               {<<"location">>, LocationID}]),
                                     mk_reply(<<"authorize">>, [[{<<"permission">>, <<"granted">>}]]),
                                     mk_reply(<<"location_info">>, [Location]),
                                     mk_reply(<<"character_info">>, [Character])],
                            {reply, {ok, Reply}, online(Nick, join(Nick, Sid, LocationID, State))};

                        _Otherwise ->
                            {reply, {ok, mk_reply(<<"authorize">>, [[{<<"permission">>, null}]])}, State}
                    end;

                true ->
                    Reply = [mk_reply(<<"authorize">>, [[{<<"permission">>, null}]]),
                             mk_error(<<"Player is already online!">>)],
                    {reply, {ok, Reply}, State}
            end
    end;

handle_call({do, Command, Arg, Sid, PlayerState}, From, State) ->
    Nick = prop(<<"nick">>, PlayerState),
    case is_char_alive(Nick, State) of
        true  -> handle_call({exec_command, Command, Arg, Sid, PlayerState}, From, State);
        false -> {reply, {ok, mk_reply(<<"bad_action">>, [<<"You are dead!">>])}, State}
    end;


handle_call({exec_command, <<"examine">>, ID, _Sid, PlayerState}, _From, State) ->
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

handle_call({exec_command, <<"move">>, Where, Sid, PlayerState}, _From, State) ->
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

handle_call({exec_command, <<"take">>, ID, _Sid, PlayerState}, _From, State) ->
    Nick = prop(<<"nick">>, PlayerState),
    LocationID = prop(<<"location">>, PlayerState),
    Player = prop(Nick, State#state.players),
    Inventory = prop(<<"inventory">>, Player),
    Stats = prop(<<"stats">>, Player),
    Location = prop(LocationID, State#state.locations),
    Items = prop(<<"items">>, Location),
    case object_by_id(Nick, LocationID, ID, State) of
        {item, Item, LocationID} ->
            Name = prop(<<"name">>, Item),
            Modifiers = prop(<<"modifiers">>, Item),
            NewStats = apply_item(Modifiers, Stats),
            NewState = State#state{
                         locations = update(LocationID,
                                            update(<<"items">>, remove(ID, Items), Location),
                                            State#state.locations),
                         players = update(Nick,
                                          update(<<"inventory">>,
                                                 update(ID, Name, Inventory),
                                                 update(<<"stats">>, NewStats, Player)),
                                          State#state.players)
                        },
            publish(LocationID, mk_event(<<"inventory_update">>,
                                         [{<<"nick">>, Nick},
                                          {<<"type">>, <<"take">>},
                                          {<<"id">>, ID},
                                          {<<"name">>, Name}])),
            {reply, {ok, mk_store([{<<"nick">>, Nick}])}, NewState};

        _Otherwise ->
            {reply, {ok, mk_reply(<<"bad_action">>, [<<"You can't take that!">>])}, State}
    end;

handle_call({exec_command, <<"drop">>, ID, _Sid, PlayerState}, _From, State) ->
    Nick = prop(<<"nick">>, PlayerState),
    LocationID = prop(<<"location">>, PlayerState),
    Player = prop(Nick, State#state.players),
    Stats = prop(<<"stats">>, Player),
    Inventory = prop(<<"inventory">>, Player),
    Location = prop(LocationID, State#state.locations),
    Items = prop(<<"items">>, Location),
    case object_by_id(Nick, LocationID, ID, State) of
        {item, Item, Nick} ->
            Name = prop(<<"name">>, Item),
            Modifiers = prop(<<"modifiers">>, Item),
            NewStats = unapply_item(Modifiers, Stats),
            NewState = State#state{
                         locations = update(LocationID,
                                            update(<<"items">>, update(ID, Name, Items), Location),
                                            State#state.locations),
                         players = update(Nick,
                                          update(<<"inventory">>,
                                                 remove(ID, Inventory),
                                                 update(<<"stats">>, NewStats, Player)),
                                          State#state.players)
                        },
            publish(LocationID, mk_event(<<"inventory_update">>,
                                         [{<<"nick">>, Nick},
                                          {<<"type">>, <<"drop">>},
                                          {<<"id">>, ID},
                                          {<<"name">>, Name}])),
            {reply, {ok, mk_store([{<<"nick">>, Nick}])}, NewState};

        _Otherwise ->
            {reply, {ok, mk_reply(<<"bad_action">>, [<<"You don't have that!">>])}, State}
    end;

handle_call({exec_command, <<"attack">>, Target, _Sid, PlayerState}, _From, State) ->
    Nick = prop(<<"nick">>, PlayerState),
    LocationID = prop(<<"location">>, PlayerState),
    Player = prop(Nick, State#state.players),
    PlayerStats = prop(<<"stats">>, Player),
    case object_by_id(Nick, LocationID, Target, State) of
        {player, Enemy, LocationID} ->
            EnemyNick = prop(<<"nick">>, Enemy),
            EnemyStats = prop(<<"stats">>, Enemy),
            case battle(PlayerStats, EnemyStats) of
                {kill, Value} ->
                    NewState = kill_character(Enemy,
                                              LocationID,
                                              State#state{
                                                players = remove(EnemyNick, State#state.players),
                                                passwd = remove(EnemyNick, State#state.passwd)
                                               }),
                    publish(LocationID, mk_event(<<"battle">>,
                                                 [[{<<"attacker">>, Nick},
                                                   {<<"defender">>, EnemyNick},
                                                   {<<"type">>, <<"kill">>},
                                                   {<<"value">>, Value}]])),
                    {reply, {ok, mk_store([{<<"nick">>, Nick}])}, NewState};

                {hit, Value, NewEnemyStats} ->
                    publish(LocationID, mk_event(<<"battle">>,
                                                 [[{<<"attacker">>, Nick},
                                                   {<<"defender">>, EnemyNick},
                                                   {<<"type">>, <<"hit">>},
                                                   {<<"value">>, Value}]])),
                    NewState = State#state{
                                 players = update(EnemyNick,
                                                  update(<<"stats">>, NewEnemyStats, Enemy),
                                                  State#state.players)
                                },
                    {reply, {ok, mk_store([{<<"nick">>, Nick}])}, NewState};

                {miss, EnemyStats} ->
                    publish(LocationID, mk_event(<<"battle">>,
                                                 [[{<<"attacker">>, Nick},
                                                   {<<"defender">>, EnemyNick},
                                                   {<<"type">>, <<"miss">>}]])),
                    {reply, {ok, mk_store([{<<"nick">>, Nick}])}, State}
            end;

        _Otherwise ->
            {reply, {ok, mk_reply(<<"bad_action">>, [<<"You can't do that!">>])}, State}
    end;

handle_call(status, _From, State) ->
    {reply, State, State};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({say, Type, Text, PlayerState}, State) ->
    Nick = prop(<<"nick">>, PlayerState),
    case is_char_alive(Nick, State) of
        true ->
            LocationID = prop(<<"location">>, PlayerState),
            publish(LocationID, mk_event(<<"msg">>,
                                         [[{<<"nick">>, Nick},
                                           {<<"type">>, Type},
                                           {<<"text">>, Text}]])),
            {noreply, State};

        false ->
            %% TODO Inform about the death thing, maby?
            {noreply, State}
    end;

handle_cast({cleanup, Sid, PlayerState}, State) ->
    Nick = prop(<<"nick">>, PlayerState),
    case is_char_alive(Nick, State) of
        true ->
            Passwd = prop(Nick, State#state.passwd),
            LocationID = prop(<<"location">>, PlayerState),
            Password = prop(<<"password">>, Passwd),
            NewState = State#state{
                         passwd = update(Nick,
                                         update(<<"password">>, Password, PlayerState),
                                         State#state.passwd)
                        },
            {noreply, offline(Nick, leave(Nick, Sid, LocationID, NewState))};

        false ->
            {noreply, State}
    end;

handle_cast({npcize, all, Strategy}, State) ->
    handle_cast({npcize,
                 lists:map(fun({Nick, _Vals}) ->
                                   Nick
                           end,
                           State#state.players),
                 Strategy},
                State);

handle_cast({npcize, Who, Strategy}, State) ->
    Passwd = lists:map(fun(Nick) ->
                               case is_char_online(Nick, State) of
                                   true ->
                                       {Nick, prop(Nick, State#state.passwd)};

                                   false ->
                                       case prop(Nick, State#state.passwd, null) of
                                           null ->
                                               {Nick, [{<<"npc">>, Strategy},
                                                       {<<"location">>, State#state.starting_location},
                                                       {<<"password">>, mud:get_env(hive_api_key)}]};

                                           Vals ->
                                               {Nick, update(<<"password">>,
                                                             mud:get_env(hive_api_key),
                                                             update(<<"npc">>, Strategy, Vals))}
                                       end
                               end
                       end,
                       Who),
    ok = spawn_npcs(State#state.ai_supervisor, Passwd, State),
    {noreply, State#state{passwd = Passwd ++ State#state.passwd}};

handle_cast({move, Nick, Where}, State) ->
    case prop(Nick, State#state.passwd) of
        null ->
            {noreply, State};

        Passwd ->
            LocationID = prop(<<"location">>, Passwd),
            case prop(Where, State#state.locations) of
                null ->
                    {noreply, State};

                _ValidLocation ->
                    %% FIXME Creating atoms at run time is a bad idea...
                    Sid = list_to_atom(binary_to_list(Nick)),
                    {noreply, join(Nick, Sid, Where, leave(Nick, Sid, LocationID, State))}
            end
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, _TRef, {spawn_ai_sup, Supervisor}}, State) ->
    lager:notice("Starting MUD AI server..."),
    case supervisor:start_child(Supervisor, {mud_ai_sup,
                                             {mud_ai_sup, start_link, []},
                                             permanent,
                                             infinity,
                                             supervisor,
                                             [mud_ai_sup]})
    of
        {ok, Pid} ->
            lager:notice("Loading NPCs..."),
            ok = spawn_npcs(Pid, State#state.passwd, State),
            {noreply, State#state{ai_supervisor = Pid}};

        {error, {already_started, Pid}} ->
            lager:notice("Loading NPCs..."),
            ok = spawn_npcs(Pid, State#state.passwd, State),
            {noreply, State#state{ai_supervisor = Pid}};

        _ ->
            {stop, shutdown, State}
    end;

handle_info({timeout, _TRef, save_state}, State) ->
    save_game_data(State),
    erlang:start_timer(mud:get_env(save_timeout), self(), save_state),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Intenal functions:
load_game_data() ->
    Resources = mud:get_env(resources),
    lager:notice("Loading MUD resources: ~p...", [Resources]),
    State = #state{
               locations = [],
               passwd = load_passwords(filename:join([Resources, mud:get_env(passwords_file)])),
               players = load_players(filename:join([Resources, mud:get_env(players_file)])),
               items = load_items(filename:join([Resources, mud:get_env(items_file)])),
               online_players = []
              },
    load_locations(filename:join([Resources, mud:get_env(locations_file)]), State).

save_game_data(State) ->
    Resources = mud:get_env(resources),
    lager:notice("Saving MUD resources: ~p...", [Resources]),
    save_locations(filename:join([Resources, mud:get_env(locations_file)]), State#state.locations),
    save_players(filename:join([Resources, mud:get_env(players_file)]), State#state.players),
    save_passwords(filename:join([Resources, mud:get_env(passwords_file)]), State#state.passwd),
    save_items(filename:join([Resources, mud:get_env(items_file)]), State#state.items).

load_locations(File, State) ->
    lists:foldl(fun(Location, NewState) ->
                        LocationID = prop(<<"id">>, Location),
                        Players = prop(<<"players">>, Location),
                        NewestState =
                            lists:foldl(fun(Nick, S) ->
                                                case prop(Nick, S#state.passwd) of
                                                    null ->
                                                        S#state{
                                                          passwd = update(Nick,
                                                                          [{<<"location">>, LocationID}],
                                                                          S#state.passwd)
                                                         };

                                                    Passwd ->
                                                        S#state{
                                                          passwd = update(Nick,
                                                                          update(<<"location">>,
                                                                                 LocationID,
                                                                                 Passwd),
                                                                          S#state.passwd)
                                                         }
                                                end
                                        end,
                                        NewState,
                                        Players),
                        NewestState#state{
                          locations = update(LocationID,
                                             update(<<"players">>, [], Location),
                                             NewestState#state.locations),
                          starting_location = case NewestState#state.starting_location of
                                                  <<"">>    -> LocationID;
                                                  Otherwise -> Otherwise
                                              end
                         }
                end,
                State,
                file_to_json(File)).

load_players(File) ->
    lists:map(extractor(<<"nick">>), file_to_json(File)).

load_passwords(File) ->
    case file_to_json(File) of
        {error, _Error} -> [];
        Otherwise       -> Otherwise
    end.

load_items(File) ->
    lists:map(extractor(<<"id">>), file_to_json(File)).

save_locations(File, Data) ->
    json_to_file(File, lists:map(fun({_Key, Value}) -> update(<<"players">>, [], Value) end, Data)).

save_players(File, Data) ->
    json_to_file(File, lists:map(fun({_Key, Value}) -> Value end, Data)).

save_passwords(File, Data) ->
    json_to_file(File, Data).

save_items(File, Data) ->
    json_to_file(File, lists:map(fun({_Key, Value}) -> Value end, Data)).

extractor(Field) ->
    fun(JSON) ->
            {proplists:get_value(Field, JSON), JSON}
    end.

spawn_npcs(_AISup, [], _State) ->
    ok;

spawn_npcs(AISup, [{Nick, Character} | Characters], State) ->
    case {prop(<<"npc">>, Character, null), is_char_online(Nick, State)} of
        {null, _} ->
            spawn_npcs(AISup, Characters, State);

        {_, true} ->
            spawn_npcs(AISup, Characters, State);

        {_, false} ->
            lager:info("Spawning NPC ~s...", [Nick]),
            case supervisor:start_child(AISup, [Nick, Character]) of
                {ok, _Pid} -> spawn_npcs(AISup, Characters, State);
                _          -> error
            end
    end.

join(Nick, Sid, LocationID, State) ->
    Location = prop(LocationID, State#state.locations),
    Players = [Nick | prop(<<"players">>, Location)],
    subscribe(Sid, LocationID),
    publish(LocationID, mk_event(<<"player_enters">>,
                                 [[{<<"location">>, prop(<<"name">>, Location)},
                                   {<<"nick">>, Nick}]] )),
    State#state{
      locations = update(LocationID,
                         update(<<"players">>, Players, Location),
                         State#state.locations)
     }.

leave(Nick, Sid, LocationID, State) ->
    Location = prop(LocationID, State#state.locations),
    Players = prop(<<"players">>, Location) -- [Nick],
    unsubscribe(Sid, LocationID),
    publish(LocationID, mk_event(<<"player_leaves">>,
                                 [[{<<"location">>, prop(<<"name">>, Location)},
                                   {<<"nick">>, Nick}]])),
    State#state{
      locations = update(LocationID,
                         update(<<"players">>, Players, Location),
                         State#state.locations)
     }.

add_character(Character, Nick, Password, Location, State) ->
    State#state{players = update(Nick, Character, State#state.players),
                passwd = update(Nick, [{<<"password">>, Password},
                                       {<<"nick">>, Nick},
                                       {<<"location">>, Location}], State#state.passwd)}.

apply_item([], Stats) ->
    Stats;

apply_item([{Modifier, Value} | Rest], Stats) ->
    apply_item(Rest, update(Modifier, Value + prop(Modifier, Stats, 0), Stats)).

unapply_item([], Stats) ->
    Stats;

unapply_item([{Modifier, Value} | Rest], Stats) when Modifier == <<"health">> ->
    unapply_item(Rest, update(Modifier, max(1, prop(Modifier, Stats, 0) - Value), Stats));

unapply_item([{Modifier, Value} | Rest], Stats) ->
    unapply_item(Rest, update(Modifier, prop(Modifier, Stats, 0) - Value, Stats)).

battle(Attacker, Defender) ->
    case roll_attack(Attacker) - roll_defense(Defender) of
        Value when Value =< 0 ->
            {miss, Defender};

        Value ->
            CurrHealth = prop(<<"health">>, Defender, 0),
            case CurrHealth - Value of
                Negative when Negative =< 0 ->
                    {kill, Value};

                _Positive ->
                    {hit, Value, update(<<"health">>, CurrHealth - Value, Defender)}
            end
    end.

roll_attack(Stats) ->
    random:uniform(max(1, prop(<<"strength">>, Stats, 1))).

roll_defense(Stats) ->
    T = max(2, prop(<<"toughness">>, Stats, 2)),
    (T div 2) + random:uniform(T div 2).

new_character(Nick) ->
    [{<<"nick">>, Nick},
     {<<"stats">>, [{<<"health">>, 100},
                    {<<"strength">>, 100},
                    {<<"toughness">>, 100}]},
     {<<"inventory">>, []}].

kill_character(Player, LocationID, State) ->
    Nick = prop(<<"nick">>, Player),
    Location = prop(LocationID, State#state.locations),
    Inventory = prop(<<"inventory">>, Player),
    Items = Inventory ++ prop(<<"items">>, Location),
    Players = prop(<<"players">>, Location) -- [Nick],
    NewLocation = update(<<"players">>, Players, Location),
    NewestLocation = update(<<"items">>, Items, NewLocation),
    lists:map(fun({ID, Name}) ->
                      publish(LocationID, mk_event(<<"inventory_update">>,
                                                   [{<<"nick">>, Nick},
                                                    {<<"type">>, <<"drop">>},
                                                    {<<"id">>, ID},
                                                    {<<"name">>, Name}]))
              end,
              Inventory),
    State#state{
      locations = update(LocationID, NewestLocation, State#state.locations)
     }.

is_char_alive(Nick, State) ->
    case prop(Nick, State#state.players) of
        null ->
            false;

        Player ->
            0 < prop(<<"health">>, prop(<<"stats">>, Player, []), 0)
    end.

is_char_online(Nick, State) ->
    lists:member(Nick, State#state.online_players).

online(Nick, State) ->
    case is_char_online(Nick, State) of
        true  -> State;
        false -> State#state{online_players = [Nick | State#state.online_players]}
    end.

offline(Nick, State) ->
    case is_char_online(Nick, State) of
        true  -> State#state{online_players = State#state.online_players -- [Nick]};
        false -> State
    end.

validate_nick(Nick, State) ->
    byte_size(Nick) < mud:get_env(max_allowed_nick_len)
        andalso byte_size(Nick) > mud:get_env(min_allowed_nick_len)
        andalso valid_chars(Nick)
        andalso prop(Nick, State#state.passwd, false) == false
        andalso prop(Nick, State#state.locations, false) == false
        andalso prop(Nick, State#state.items, false) == false.

valid_chars(Nick) ->
    case binary:match(Nick, [<<"<">>, <<">">>], []) of
        nomatch -> true;
        _       -> false
    end.

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
                null ->
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
