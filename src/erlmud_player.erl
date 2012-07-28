-module(erlmud_player).
-behaviour(gen_server).

%%% Records
% @TODO: Put into header
-record(state, {
        room=room1 :: atom()
        }).

%%% Exports
%% OTP API
-export([start_link/0]).

%% API
-export([get_room/1, go/1]).

%% gen_server API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% Functions
%% OTP API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% API
get_room(exits) ->
    gen_server:call(?MODULE, {get_room, exits});
get_room(desc) ->
    gen_server:call(?MODULE, {get_room, desc});
get_room(name) ->
    gen_server:call(?MODULE, {get_room, name}).

go(Direction) ->
    gen_server:cast(?MODULE, {change_room, Direction}).

%% gen_server callbacks
init([]) ->
    init(#state{});
init(#state{room=Room}=State) ->
    case start_room(Room) of
        {not_found, Room} ->
            io:format("Could not find room: ~p~n", [Room]),
            init(State#state{room=room1});
        _ -> {ok, State}
    end.

handle_call({get_room, Attrib}, _From, State) ->
    Reply = get_room(Attrib, State),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({change_room, Direction}, State) ->
    NewState = change_room(Direction, State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
get_room(Attrib, #state{room=Room}) ->
    erlmud_room:get_attr(Room, Attrib).

change_room(Direction, #state{}=State) ->
    Exits = get_room(exits, State),
    {Direction, NewRoom} = lists:keyfind(Direction, 1, Exits),
    ok = start_room(NewRoom),
    State#state{room=NewRoom}.

start_room(Room) ->
    case erlmud_sup:start_room(Room) of
        {ok, _PID} ->
            ok;
        {error, {already_started, _PID}} ->
            ok;
        {not_found, Room}=R ->
            R
    end.
