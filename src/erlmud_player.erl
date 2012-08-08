-module(erlmud_player).
-behaviour(gen_server).

%%% Records
-record(state, {
        room=room1 :: atom(),
        % @todo: Remember to expand for web client
        client :: {erlmud_telnet_protocol, pid()}
        }).

%%% Exports
%% OTP API
-export([start_link/1]).

%% API
-export([get_room/2, go/2, notify/2, quit/2]).

%% gen_server API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% Functions
%% OTP API
start_link({_ClientType, _ClientPID}=Client) ->
    gen_server:start_link(?MODULE, Client, []).

%% API
get_room(Player, Attribute) when is_atom(Attribute) ->
    gen_server:call(Player, {room, Attribute}).

go(Player, Direction) ->
    gen_server:cast(Player, {change_room, Direction}).

notify(Player, Event) ->
    gen_server:cast(Player, {notify, Event}).

quit(Player, Reason) ->
    gen_server:cast(Player, {quit, Reason}).

%% gen_server callbacks
init({_ClientType, _ClientPID}=Client) ->
    init(#state{client=Client});
% @todo: Rewrite later to support persistant player characters
init(#state{room=Room}=State) ->
    ok = enter_room(Room),
    {ok, State}.

handle_call({room, Attrib}, _From, State) ->
    Reply = room(Attrib, State),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

% @todo: Consider handling Reason for other reasons (e.g., linkdeath, idle,
% etc.)
handle_cast({quit, _Reason}, State) ->
    {stop, normal, State};
handle_cast({notify, Event}, State) ->
    handle_notify(Event, State),
    {noreply, State};
handle_cast({change_room, Direction}, State) ->
    NewState = change_room(Direction, State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{room=Room}=_State) ->
    erlmud_room:leave(Room, self(), quit),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
room(Attrib, #state{room=Room}) ->
    erlmud_room:get_attr(Room, Attrib).

change_room(Direction, #state{room=Room}=State) ->
    Exits = room(exits, State),
    {Direction, NewRoom} = lists:keyfind(Direction, 1, Exits),
    erlmud_room:leave(Room, self(), {direction, Direction}),
    enter_room(NewRoom),
    State#state{room=NewRoom}.

enter_room(Room) ->
    case start_room(Room) of
        PID when is_pid(PID) ->
            erlmud_room:enter(PID, self());
        {not_found, Room} ->
            {not_found, Room}
    end.

start_room(Room) ->
    case erlmud_sup:start_room(Room) of
        PID when is_pid(PID) ->
            PID;
        {not_found, Room} ->
            {not_found, Room}
    end.

handle_notify(Event, #state{client={ClientType, ClientPID}}) ->
    ClientType:notify(ClientPID, Event).
