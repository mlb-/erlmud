-module(erlmud_player).
-behaviour(gen_server).

%%% Records
-record(state, {
        }).

%%% Exports
%% OTP API
-export([start_link/0]).

%%
-export([get_room/1]).

%% gen_server API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% Functions
%% OTP API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_room(desc) ->
    gen_server:call(?MODULE, {get_room, desc});
get_room(name) ->
    gen_server:call(?MODULE, {get_room, name}).

%% gen_server callbacks
init([]) ->
    {ok, #state{}}.

handle_call({get_room, Attrib}, _From, State) ->
    Reply = get_room(Attrib, State),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
get_room(Attrib, #state{}) ->
    erlmud_room:get_attr(Attrib).
