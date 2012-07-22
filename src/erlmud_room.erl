-module(erlmud_room).
-behaviour(gen_server).

%%% Record
% @TODO: Put into header
-record(state, {
        name :: string()
        }).

%%% Exports
%% OTP API
-export([start_link/0]).
-export([start_link/1]).

%% API
-export([get_room/1]).

%% gen_server API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% Functions
%% OTP API
start_link() ->
    gen_server:start_link(?MODULE, [], []).

start_link(Room) ->
    gen_server:start_link({local, Room}, ?MODULE, [Room], []).

%% API
get_room(Room) ->
    gen_server:call(Room, get_name).

%% gen_server callbacks
init([]) ->
    {stop, badargs};
init([Room]) ->
    RoomName = erlang:atom_to_list(Room),
    io:format("Starting room ~p~n", [RoomName]),
    {ok, #state{name=RoomName}}.

handle_call(get_name, _From, State) ->
    Reply = get_name(State),
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
get_name(#state{name=Name}) ->
    Name.

% vim: sw=4 ts=4 et
