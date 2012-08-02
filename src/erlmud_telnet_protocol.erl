-module(erlmud_telnet_protocol).
-behaviour(gen_server).
-include("log.hrl").

%%% Records
-record(state, {
        socket :: inet:socket(),
        transport :: module(),
        opts :: list(term()),
        buffer = <<>> :: binary()
        }).

%%% Exports
%% OTP API
-export([start_link/4]).

%% gen_server API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% Functions
%% OTP API
start_link(ListenerPid, Socket, Transport, Opts) ->
    State = #state{socket=Socket, transport=Transport, opts=Opts},
    gen_server:start_link(?MODULE, {State, ListenerPid}, []).

%% gen_server callbacks
init({#state{}=State, ListenerPid}) ->
    % Synchronous start_link/init, hence ranch accept delayed
    gen_server:cast(self(), {post_init, ListenerPid}),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({post_init, ListenerPid}, State) ->
    post_init(ListenerPid, State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp_closed, Socket}, #state{socket=Socket}=State) ->
    {stop, normal, State};
handle_info({tcp, Socket, Msg}, #state{socket=Socket}=State) ->
    NewState = build_lines(Msg, State),
    once_active(State),
    {noreply, NewState};
handle_info(Info, State) ->
    ?PRINT(Info),
    once_active(State),
    {noreply, State}.

terminate(_Reason, #state{socket=Socket, transport=Transport}=_State) ->
    Transport:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal
post_init(ListenerPid, State) ->
    ok = ranch:accept_ack(ListenerPid),
    send(<<"Hello world.\r\n">>, State),
    send_prompt(State),
    once_active(State).

send_prompt(State) ->
    send("> ", State).

send(Msg, #state{socket=Socket, transport=Transport}) ->
    Transport:send(Socket, Msg).

once_active(#state{socket=Socket, transport=Transport}) ->
    Transport:setopts(Socket, [{active, once}]).

build_lines(Msg, #state{buffer=Buffer} = State) ->
    % Prepend previous buffer, split, and take last "line" as new buffer
    [NewBuffer|ReverseLines] = lists:reverse(binary:split(
                <<Buffer/binary, Msg/binary>>,
                <<"\r\n">>,
                [global])),
    Lines = lists:reverse(ReverseLines),
    [handle_line(Line, State)
     || Line <- Lines],
    State#state{buffer = NewBuffer}.

handle_line(Msg, State) ->
    ?PRINT(binary:split(Msg, <<" ">>)),
    send_prompt(State).
