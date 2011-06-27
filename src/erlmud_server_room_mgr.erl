%%
%% This module keeps a lookup of room -> pid(room_mgr(room))
%%

-module(erlmud_server_room_mgr).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
%% @debug: Print macro
-define(PRINT(Var), error_logger:info_msg("DEBUG: ~p:~p~n~p~n  ~p~n", [?MODULE, ?LINE, ??Var, Var])).

-record(state, {
		tid :: ets:tab()
		,sup :: pid()
	}).

%% Supervisor API
-export([start_link/2]).

%% API: used by area_mgr, room
-export([lookup/2, reg/3]).

%% gen_server API
-export([init/1, handle_call/3, handle_cast/2,
		handle_info/2, terminate/2, code_change/3]).

%% Supervisor Functions
start_link(Sup, Area) ->
  gen_server:start_link(?MODULE, [Sup, Area], []).

%% Internal Function Definitions

%% @todo: decide on that Room should be. Using integers or pos_integer is
%% tempting.
-spec lookup(pid(), term()) -> pid().
lookup(Mgr, Room) ->
	gen_server:call(Mgr, {lookup, Room}).

-spec reg(pid(), term(), pid()) -> ok.
reg(Mgr, Room, PID) ->
	gen_server:cast(Mgr, {reg, Room, PID}).

%% gen_server Functions

init([Sup, Area]) ->
	TID = ets:new(erlmud_server_room_mgr, [named_table, ordered_set, protected]),
	State = #state{
		tid = TID
		,sup = Sup
	},
	erlmud_server_area_mgr:reg(Area, self()),
	{ok, State}.


handle_call({lookup, Room}, _From, #state{} = State) ->
	TID = State#state.tid,
	A = case ets:lookup(TID, {room, Room}) of
		[] ->
			Sup = State#state.sup,
			erlmud_server_room_sup:start_room(Sup, self(), Room)
			;
		[{{room, Room}, RoomPID}] -> RoomPID
	end,
  {reply, A, State};

handle_call(_Request, _From, #state{} = State) ->
  {noreply, ok, State}.

handle_cast({reg, Room, PID}, #state{} = State) ->
	TID = State#state.tid,
	ets:insert(TID, {{room, Room}, PID}),
	{noreply, State};

handle_cast(_Msg, #state{} = State) ->
  {noreply, State}.

handle_info(_Info, #state{} = State) ->
  {noreply, State}.

terminate(_Reason, #state{} = _State) ->
  ok.

code_change(_OldVsn, #state{} = State, _Extra) ->
  {ok, State}.

