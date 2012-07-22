-module(erlmud_sup).
-behaviour(esupervisor).
-include_lib("esupervisor/include/esupervisor.hrl").

%%% Exports
%% API
-export([start_link/0]).

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
            #worker{
                id=erlmud_room
                },
            #worker{
                id=erlmud_player
                }
            ]
        }.

%%% Spec description helpers
%room(RoomName) ->
%    Mod = erlmud_room,
%    #worker{
%        id = RoomName,
%        modules = [Mod],
%        start_func = {Mod, start_link, [RoomName]}
%        }.
