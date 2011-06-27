%%
%% This module keeps a lookup of area -> pid(room_mgr(area))
%%

-module(erlmud_server_area_mgr).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).
-define(PRINT(Var), error_logger:info_msg("DEBUG: ~p:~p~n~p~n  ~p~n", [?MODULE, ?LINE, ??Var, Var])).

-record(state, {
	}).

%% Supervisor API
-export([start_link/0]).

%% API
-export([lookup/2, reg/2]).

%% gen_server API
-export([init/1, handle_call/3, handle_cast/2,
		handle_info/2, terminate/2, code_change/3]).

%% Supervisor Functions
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% API
%% Get room_manager from area_manager, then lookup room in room_manager
lookup(Area, Room) ->
	A = lookup(Area),
	erlmud_server_room_mgr:lookup(A, Room).

reg(Area, PID) ->
	gen_server:cast(?MODULE, {reg, Area, PID}).

%%% Internal Function Definitions

lookup(Area) ->
	gen_server:call(?MODULE, {lookup, Area}).

%%% gen_server Functions

init([]) ->
	_TID = ets:new(?TAB, [named_table, ordered_set, protected]),
	State = #state{
	},
	{ok, State}.


handle_call({lookup, Area}, _From, #state{} = State) ->
	A = case ets:lookup(?TAB, {area, Area}) of
		[] ->
			erlmud_server_area_sup:start_area(Area)
			;
		[{{area, Area}, AreaPID}] -> AreaPID
			;
		Else ->
			?PRINT(Else)
	end,
  {reply, A, State};

handle_call(_Request, _From, #state{} = State) ->
  {noreply, ok, State}.


handle_cast({reg, Area, PID}, #state{} = State) ->
	ets:insert(?TAB, {{area, Area}, PID}),
	{noreply, State};

handle_cast(_Msg, #state{} = State) ->
  {noreply, State}.


handle_info(_Info, #state{} = State) ->
  {noreply, State}.

terminate(_Reason, #state{} = _State) ->
  ok.

code_change(_OldVsn, #state{} = State, _Extra) ->
  {ok, State}.

