
-module(erlmud_server_room).
-author('Matthew Batema <matthew.batema@gmail.com>').
-define(PRINT(Var), error_logger:info_msg("DEBUG: ~p:~p~n~p~n  ~p~n", [?MODULE, ?LINE, ??Var, Var])).

-behaviour(gen_server).

-record(exit, {
		name :: string()
		, pid :: pid()
		%, door :: 'none' | 'open' | 'closed' | 'locked'
	}).
-record(state, {
		name :: string()
		,description :: string()
		,exits=[] :: list(#exit{})
	}).

%%% Exports

%% supervisor API call
-export([start_link/2]).

%% general API
%-export([]).

%% gen_server API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
		code_change/3]).

%%% Functions

%% API
-spec start_link(pid(), _) -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link(Mgr, Room) ->
	gen_server:start_link(?MODULE, [Mgr, Room], []).

%% internal

%% gen_server

init([Mgr, RoomName]) ->
	erlmud_server_room_mgr:reg(Mgr, RoomName, self()),
	{ok, #state{name=RoomName}}.

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast(Msg, #state{exits = Exits} = State) ->
	case Msg of
		{set_exit, #exit{} = Exit} ->
			NewExits = [Exit]++Exits,
			{noreply, State#state{exits = NewExits}}
	end.
	%{noreply, State}.

handle_info(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_VSN, State, _Extra) ->
	{ok, State}.

% vim: set fdm=indent ts=2 sw=2 :
