
%%
%% This module is for managing area supervisors, which in turn supervise all
%% rooms of an area.
%%

-module(erlmud_server_room_sup).

-behaviour(supervisor).
-define(PRINT(Var), error_logger:info_msg("DEBUG: ~p:~p~n~p~n  ~p~n", [?MODULE, ?LINE, ??Var, Var])).

%% area_sup API
-export([start_link/1, start_room/3]).

%% Supervisor callbacks
-export([init/1]).

-include("supervisor.hrl").

%% API functions
-spec start_link(string()) -> 'ignore' | {'error',_Reason} | {'ok',pid()}.
start_link(Area) ->
	supervisor:start_link(?MODULE, [Area]).

start_room(Sup, Mgr, Room) ->
	{ok, R} = supervisor:start_child(Sup, ?CHILDP(erlmud_server_room, [Mgr, Room])),
	R.

%% internal functions
-spec mgr_spec(string()) -> supervisor:child_spec().
mgr_spec(Area) ->
	%ID = {mgr, Area},
	ID = Area,
	Mod = erlmud_server_room_mgr,
	MFA = {Mod, start_link, [self(), Area]},
	{ID, MFA, permanent, 5000, worker, [Mod]}.

%% Supervisor callbacks

init([Area]) ->
		Children = [
			mgr_spec(Area)
			%?CHILD(erlmud_server_worker, worker)
			%?CHILD(erlmud_server_other_sup, supervisor)
		],
    {ok, { {one_for_one, 5, 10}, Children} }.

