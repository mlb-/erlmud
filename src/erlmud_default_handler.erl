-module(erlmud_default_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
%% This module, initially taken from the cowboy_examples repo, conflates both
%% the http handler and the websocket handler into a single module.

%%% Record
-record(state, {
        player :: pid(),
        ws :: pid()
        }).

%%% Exports
%% Cowboy HTTP API
-export([init/3, handle/2, terminate/2]).

%% Cowboy websocket API
-export([websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

%% erlmud_player API
-export([notify/2]).

%%% Functions
%% Cowboy HTTP callbacks
init({_Any, http}, Req, []) ->
    case cowboy_http_req:header('Upgrade', Req) of
        {Websocket, _Req2} when
                Websocket =:= <<"websocket">>;
                Websocket =:= <<"WebSocket">>
                -> {upgrade, protocol, cowboy_http_websocket};
        {undefined, Req2} -> {ok,
                              cowboy_http_req:compact(Req2),
                              undefined}
    end.

handle(Req, State) ->
    {ok, File} = file:read_file("priv/html.html"),
    {ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"text/html">>}], File, Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

%% Cowboy Websocket callbacks
websocket_init(_Any, Req, []=_State) ->
    {ok, Player} = erlmud_sup:start_player({?MODULE, self()}),
    State = #state{player=Player, ws=self()},
    %self() ! post_init,
    Req2 = cowboy_http_req:compact(Req),
    {ok, Req2, State, hibernate}.

websocket_handle({text, Msg}, Req, State) ->
    Reply = {text, <<"Echoing back: ", Msg/binary>>},
    {reply, Reply, Req, State, hibernate};
websocket_handle(_Msg, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_info({notify, Event}, Req, State) ->
    handle_notify(Event, State),
    {ok, Req, State, hibernate};
websocket_info({send, Msg}, Req, State) ->
    Reply = {text, erlang:iolist_to_binary(Msg)},
    {reply, Reply, Req, State, hibernate};
websocket_info(_Msg, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

%% erlmud_player API
notify(PID, Event) ->
    PID ! {notify, Event}.

%% internal
send(Msg, #state{ws=WS}) ->
    WS ! {send, Msg}.

handle_notify(Event, State) ->
    send(io_lib:format("Dunno how to handle: ~p\r\n", [Event]), State).
