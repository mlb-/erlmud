-module(erlmud_default_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
%% This module, initially taken from the cowboy_examples repo, conflates both
%% the http handler and the websocket handler into a single module.

%% Exports
% Cowboy HTTP API
-export([init/3, handle/2, terminate/2]).

% Cowboy websocket API
-export([websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

%% Functions
% Cowboy HTTP callbacks
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

% Cowboy Websocket callbacks
websocket_init(_Any, Req, []=State) ->
    Req2 = cowboy_http_req:compact(Req),
    {ok, Req2, State, hibernate}.

websocket_handle({text, Msg}, Req, State) ->
    Reply = {text, <<"Echoing back: ", Msg/binary>>},
    {reply, Reply, Req, State, hibernate};
websocket_handle(_Msg, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_info(_Msg, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
