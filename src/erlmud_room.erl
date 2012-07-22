-module(erlmud_room).
-behaviour(gen_server).

%%% Record
% @TODO: Put into header
-record(state, {
        name = "room1" :: string()
        ,description = "unimaginative description" :: string()
        }).

%%% Exports
%% OTP API
-export([start_link/0]).

%% API
-export([get_attr/1]).

%% gen_server API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% Functions
%% OTP API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% API
get_attr(Attrib) ->
    gen_server:call(?MODULE, {get_attr, Attrib}).

%% gen_server callbacks
init([]) ->
    State = #state{},
    io:format("Starting room with state ~p~n", [State]),
    {ok, State}.

handle_call({get_attr, Attrib}, _From, State) ->
    Reply = get_attr(Attrib, State),
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
get_attr(desc, #state{description=Desc}) ->
    Desc;
get_attr(name, #state{name=Name}) ->
    Name.
