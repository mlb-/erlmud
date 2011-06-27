
%%
%% This module is for managing area supervisors, which in turn supervise all
%% rooms of an area.
%%

-module(erlmud_server_area_sup).

-behaviour(supervisor).
-define(PRINT(Var), error_logger:info_msg("DEBUG: ~p:~p~n~p~n  ~p~n", [?MODULE, ?LINE, ??Var, Var])).

%% OTP API
-export([start_link/0]).

%% area_mgr API
-export([start_area/1]).

%% Supervisor callbacks
-export([init/1]).

-include("supervisor.hrl").

%% OTP API functions
-spec start_link() -> 'ignore' | {'error',_Reason} | {'ok',pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% area_mgr functions
-spec start_area(string()) -> pid().
start_area(Area) ->
	{ok, Sup} = supervisor:start_child(?MODULE, room_sup_spec(Area)),
	[{"root", AreaMgrPID, worker, [erlmud_server_room_mgr]}] = supervisor:which_children(Sup),
	AreaMgrPID.

%% internal functions
-spec room_sup_spec(string()) -> supervisor:child_spec().
room_sup_spec(AreaName) ->
	ID = {room_sup, AreaName},
	Mod = erlmud_server_room_sup,
	StartMFA = {Mod, start_link, [AreaName]},
	{ID, StartMFA, permanent, infinity, supervisor, [Mod]}.

%% Supervisor behaviour functions

init([]) ->
		Children = [
			?CHILD(erlmud_server_area_mgr, worker)
			%?CHILD(erlmud_server_worker, worker)
			%?CHILD(erlmud_server_other_sup, supervisor)
		],
    {ok, { {one_for_one, 5, 10}, Children} }.

% vim: fdm=indent
