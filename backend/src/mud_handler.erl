-module(mud_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

%% Cowboy loop handler callbacks
init({tcp, http}, Request, _Options) ->
    case cowboy_req:binding(endpoint, Request) of
        {undefined, Req} ->
            lager:error("Bad request!"),
            {shutdown, Req, bad_request};

        {Endpoint, Req} ->
            {ok, Body, Req2} = cowboy_req:body(Req),
            {ok, Req2, {Endpoint, jsonx:decode(Body, [{format, proplist}])}}
    end.
terminate(_Reason, _Request, _State) ->
    ok.

%% Cowboy loop handler handlers
handle(Request, {<<"authorize">>, Data}) ->
    lager:info("Received an authorization attempt: ~p", [Data]),
    game_call(Request, mud_game:authorize(Data));

handle(Request, {<<"say">>, Data}) ->
    lager:info("Received a dialog: ~p", [Data]),
    game_call(Request, mud_game:say(Data));

handle(Request, {<<"do">>, Data}) ->
    lager:info("Received an action: ~p", [Data]),
    game_call(Request, mud_game:do(Data));

handle(Request, {<<"cleanup">>, Data}) ->
    lager:info("Received a cleanup request: ~p", [Data]),
    game_call(Request, mud_game:cleanup(Data));

handle(Request, {Endpoint, Data}) ->
    lager:warning("Received an unknown request: ~p, ~p", [Endpoint, Data]),
    Req2 = reply(404, "Wut?", Request),
    {ok, Req2, ok}.

%% Internal functions:
reply(Reply, Request) ->
    reply(200, Reply, Request).

reply(Code, Reply, Request) ->
    {ok, Req2} = cowboy_req:reply(Code, [], Reply, Request),
    Req2.

game_call(Request, Result) ->
    case Result of
        ok              -> {ok, Request, ok};
        {ok, Reply}    -> Req2 = reply(jsonx:encode(Reply), Request),
                          {ok, Req2, ok};
        {error, Error} -> Req2 = reply(501, jsonx:encode(Error), Request),
                          {ok, Req2, error}
    end.
