
-module(erlmud_server_room).

-behaviour(gen_server).

%%% Records
% @TODO: Put into header
-record(state, {
		name :: string()
		,description :: string()
	}).

%%% Exports {{{

%% API
-export([get_room/1]).

%% OTP API
-export([start_link/1]).

%% gen_server API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
		code_change/3]).

% }}}

%%% Functions {{{

%% OTP API
start_link(Room) ->
	gen_server:start_link({local, Room}, ?MODULE, [Room], []).

%% API {{{
get_room(Room) ->
	gen_server:call(Room, get_name).
% }}}

%% gen_server callbacks {{{
init([Room]) ->
	RoomName = erlang:atom_to_list(Room),
	io:format("Starting room ~p~n", [RoomName]),
	{ok, #state{name=RoomName}}.

handle_call(get_name, _From, #state{name = Name} = State) ->
	{reply, Name, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_VSN, State, _Extra) ->
	{ok, State}.
% }}}

% }}}

% vim: set fdm=marker:
