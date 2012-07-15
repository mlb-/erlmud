
-module(erlmud_server_sup).
-behaviour(esupervisor).
-include_lib("esupervisor/include/esupervisor.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% API functions
start_link() ->
	esupervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Supervisor callback
init([]) ->
	#one_for_one{ % App supervisor
		children = [
			% Move under a supervisor of rooms (room_sup? area_sup?), under a
			% supervisor of areas, under a single "world" supervisor.
			room(room1),
			room(room2)
		]
	}.

%% Spec description helpers
room(RoomName) ->
	Mod = erlmud_server_room,
	#worker{
		id = RoomName,
		modules = [Mod],
		start_func = {Mod, start_link, [RoomName]}
	}.

