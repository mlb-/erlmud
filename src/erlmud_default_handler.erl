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
    {ok, File} = file:read_file("priv/cowboy/html.html"),
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
    Response = case handle_line(Msg, State) of
        ok -> nothing;
        %List when is_list(List) -> list_to_bitstring(List);
        Binary when is_binary(Binary) -> Binary;
        Term -> list_to_bitstring(io_lib:format("~p", [Term]))
    end,
    case Response of
        nothing -> {ok, Req, State, hibernate};
        Response ->
            Reply = {text, <<Response/binary>>},
            {reply, Reply, Req, State, hibernate}
    end;
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

handle_line(Line, State) ->
    [Command|Args] = binary:split(Line, <<" ">>),
    handle_command(Command, Args, State).

handle_command(<<"say">>, Args, #state{player=Player}) ->
    erlmud_player:say(Player, Args);
handle_command(<<"look">>, _Args, #state{player=Player}=State) ->
    [
        erlmud_player:get_room(Player, name),
        erlmud_player:get_room(Player, desc),
            get_exits(State),
        erlmud_player:get_room(Player, players)
        ];
handle_command(Command, _Args, #state{player=Player}=State) ->
    case [erlmud_player:go(Player, Exit) ||
            Exit <- get_exits(State),
            erlang:list_to_bitstring(Exit) =:= Command] of
        [] ->
            <<"Unknown command: ", Command/binary>>;
        [ok] ->
            handle_command(<<"look">>, [], State)
    end.

get_exits(#state{player=Player}) ->
    [Exit || {Exit, _RoomId} <- erlmud_player:get_room(Player, exits)].
