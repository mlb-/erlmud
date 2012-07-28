-module(erlmud_room).
-behaviour(gen_server).

%%% Record
% @TODO: Put into header
% @TODO: Create user-defined types
-record(state, {
        id = room0 :: atom(),
        name = "room0" :: string(),
        description = "unimaginative description" :: string(),
        exits = [] :: list({nonempty_string(), atom()})
        }).

%%% Exports
%% OTP API
-export([start_link/1]).

%% API
-export([get_attr/2]).

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

%% gen_server callbacks
init(Room) when
        is_atom(Room) ->
    RoomName = erlang:atom_to_list(Room),
    init(#state{name=RoomName});
init(#state{}=State) ->
    io:format("Starting room with state ~p~n", [State]),
    {ok, State}.

handle_call({attr, Attrib}, _From, State) ->
    Reply = attr(Attrib, State),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
attr(exits, #state{exits=Exits}) ->
    Exits;
attr(desc, #state{description=Desc}) ->
    Desc;
attr(name, #state{name=Name}) ->
    Name.
