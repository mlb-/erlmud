-module(erlmud_room).
-behaviour(gen_server).

%%% Record
% @TODO: Put into header
% @TODO: Create user-defined types
-record(state, {
        id = room0 :: atom(),
        name = "room0" :: string(),
        description = "unimaginative description" :: string(),
        exits = [] :: list({nonempty_string(), atom()}),
        players = [] :: list(pid())
        }).

%%% Exports
%% OTP API
-export([start_link/1]).

%% API
-export([get_attr/2, enter/2, leave/3, say/3]).

%% gen_server API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% Functions
%% OTP API
start_link({RoomId, RoomName, RoomDesc, Exits}) when
        is_atom(RoomId) ->
    State = #state{id=RoomId, name=RoomName, description=RoomDesc, exits=Exits},
    gen_server:start_link({local, RoomId}, ?MODULE, State, []);
start_link(Room) when
        is_atom(Room) ->
    gen_server:start_link({local, Room}, ?MODULE, Room, []).

%% API
get_attr(PID, Attrib) ->
    gen_server:call(PID, {attr, Attrib}).

enter(PID, Player) ->
    gen_server:cast(PID, {enter, Player}).

leave(PID, Player, How) ->
    gen_server:cast(PID, {leave, Player, How}).

say(PID, Player, Msg) ->
    gen_server:cast(PID, {say, Player, Msg}).

%% gen_server callbacks
init(Room) when
        is_atom(Room) ->
    RoomName = erlang:atom_to_list(Room),
    init(#state{name=RoomName});
init(#state{}=State) ->
    io:format("Starting room: ~ts~n", [State#state.name]),
    {ok, State}.

handle_call({attr, Attrib}, _From, State) ->
    Reply = attr(Attrib, State),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({say, Player, Msg}, State) ->
    handle_say(Player, Msg, State),
    {noreply, State};
handle_cast({leave, Player, How}, State) ->
    NewState = remove_player(Player, How, State),
    {noreply, NewState};
handle_cast({enter, Player}, State) ->
    NewState = add_player(Player, State),
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
attr(players, #state{players=Players}) ->
    Players;
attr(exits, #state{exits=Exits}) ->
    Exits;
attr(desc, #state{description=Desc}) ->
    Desc;
attr(name, #state{name=Name}) ->
    Name.

remove_player(Player, How, #state{players=Players}=State) ->
    RemainingPlayers = Players -- [Player],
    [ erlmud_player:notify(PID, {exited_room, Player, How})
                                 || PID <- RemainingPlayers],
    State#state{players=RemainingPlayers}.

add_player(Player, #state{players=Players}=State) ->
    [ erlmud_player:notify(PID, {entered_room, Player})
                                 || PID <- Players],
    State#state{players=[Player|Players]}.

handle_say(Player, Msg, #state{players=Players}) ->
    [erlmud_player:notify(PID, {said, Player, Msg})
                         || PID <- Players -- [Player]],
    erlmud_player:notify(Player, {you_said, Msg}).
