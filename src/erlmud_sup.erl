-module(erlmud_sup).
-behaviour(esupervisor).
-include_lib("esupervisor/include/esupervisor.hrl").

%%% Exports
%% API
-export([start_link/0]).

%% dev API
-export([start_room/1, start_player/0]).

%% Supervisor callbacks
-export([init/1]).

%%% Functions
%% API functions
start_link() ->
    esupervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Supervisor callback
init([]) ->
    #one_for_one{ % Application supervisor
        children = [
            % Move under a supervisor of rooms (room_sup? area_sup?), under a
            % supervisor of areas, under a single "world" supervisor.
            #one_for_one{ % Room supervisor
                id = erlmud_room_sup,
                registered = erlmud_room_sup,
                children = [
                    room_sofo()
                    ]
                },
            #one_for_one{
                id = erlmud_player_sup,
                registered = erlmud_player_sup,
                children = [
                    player_sofo()
                    ]
                }
            ]
        }.

start_room(Room) when
        is_atom(Room) ->
    Priv = code:priv_dir(erlmud),
    {ok, RoomList} = file:consult(Priv ++ "/rooms.fixture"),
    case lists:keyfind(Room, 1, RoomList) of
        false -> {not_found, Room};
        R -> supervisor:start_child(room_sofo, [R])
    end.

room_sofo() ->
    Mod = erlmud_room,
    #simple_one_for_one{
        registered = room_sofo,
        children = [
            #worker{
                start_func = {Mod, start_link, []}
                }
            ]
        }.

start_player() ->
    supervisor:start_child(player_sofo, []).

player_sofo() ->
    Mod = erlmud_player,
    #simple_one_for_one{
        registered = player_sofo,
        children = [
            #worker{
                start_func = {Mod, start_link, []}
                }
            ]
        }.
