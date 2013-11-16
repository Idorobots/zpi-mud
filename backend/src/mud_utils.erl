-module(mud_utils).
-export([publish/2, sid/1, trigger/1, state/1, file_to_json/1, prop/2, prop/3]).
-export([mk_error/1, mk_reply/2, mk_store/1, update/3, subscribe/2, unsubscribe/2, remove/2]).
-export([json_to_file/2, mk_event/2, str_join/2, str_cat/2, data/1, data/2]).

%% Util functions:
publish(Channel, Message) ->
    api_call(post, [<<"pubsub/publish/">>, mud:get_env(channel_prefix) , Channel], Message).

subscribe(<<"">>, _Channel) ->
    ok;

subscribe(Sid, Channel) ->
    api_call(post, [<<"pubsub/subscribe/">>, Sid], [str_cat(mud:get_env(channel_prefix), Channel)]).

unsubscribe(<<"">>, _Channel) ->
    ok;

unsubscribe(Sid, Channel) ->
    api_call(delete, [<<"pubsub/subscribe/">>, Sid], [str_cat(mud:get_env(channel_prefix), Channel)]).

sid(Data) ->
    proplists:get_value(<<"sid">>, Data).

trigger(Data) ->
    proplists:get_value(<<"trigger">>, Data).

state(Data) ->
    proplists:get_value(<<"state">>, Data).

data(State) ->
    data(null, State).

data(Trigger, State) ->
    [{<<"sid">>, <<"">>},
     {<<"trigger">>, Trigger},
     {<<"state">>, State}].

file_to_json(File) ->
    case file:read_file(File) of
        {ok, Contents} -> jsonx:decode(Contents, [{format, proplist}]);
        Otherwise -> Otherwise
    end.

json_to_file(File, Data) ->
    file:write_file(File, jsonx:encode(Data)).

prop(Name, List) ->
    prop(Name, List, null).

prop(Name, List, Default) ->
    proplists:get_value(Name, List, Default).

update(Name, Value, []) ->
    [{Name, Value}];

update(Name, Value, [{Name, _OldValue} | Rest]) ->
    [{Name, Value} | Rest];

update(Name, NewValue, [Value | Rest]) ->
    [Value | update(Name, NewValue, Rest)].

remove(_Name, []) ->
    [];

remove(Name, [{Name, _OldValue} | Rest]) ->
    remove(Name, Rest);

remove(Name, [Value | Rest]) ->
    [Value | remove(Name, Rest)].

mk_error(Error) ->
    [{<<"action">>, <<"error">>},
     {<<"args">>, Error}].

mk_reply(Event, Args) ->
    [{<<"action">>, <<"reply">>},
     {<<"args">>, mk_event(Event, Args)}].

mk_store(State) ->
    [{<<"action">>, <<"store">>},
     {<<"args">>, State}].

mk_event(Event, Args) ->
    [{<<"name">>, Event},
     {<<"args">>, Args}].

str_join(_Separatr, []) ->
    <<>>;

str_join(_Separator, [String]) ->
    String;

str_join(Separator, [String | Strings]) ->
    str_cat(String, str_cat(Separator, str_join(Separator, Strings))).

str_cat(A, B) ->
    <<A/binary, B/binary>>.

%% Internal functions:
api_call(Method, Path, Data) ->
    ApiUrl = mud:get_env(hive_url),
    ApiPort = mud:get_env(hive_api_port),
    ApiKey = mud:get_env(hive_api_key),
    URL = str_join(<<"/">>, [str_join(<<":">>, [ApiUrl, integer_to_binary(ApiPort)]),
                             <<"api">>,
                             ApiKey,
                             str_join(<<"">>, Path)]),
    Body = jsonx:encode(Data),
    lager:info("Sending API request ~p to ~p, body: ~p", [Method, URL, Body]),
    ibrowse:send_req(binary_to_list(URL), [], Method, Body).
