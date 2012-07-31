-module(erlmud_sup).
-behaviour(esupervisor).
-include_lib("esupervisor/include/esupervisor.hrl").

%%% Exports
%% API
-export([start_link/0]).

%% dev API
-export([start_room/1]).

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
                registered = erlmud_room_sup,
                children = [
                    room_sofo()
                    ]
                },
            #worker{
                id = erlmud_player
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
