-module(erlmud_telnet_protocol).
-behaviour(gen_server).

%%% Records
-record(state, {
        socket :: inet:socket(),
        transport :: module(),
        opts :: list(term()),
        player :: pid(),
        buffer = <<>> :: binary()
        }).

%%%
%%% Exports
%% OTP API
-export([start_link/4]).

%% API
-export([notify/2]).

%% gen_server API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% Functions
%% OTP API
start_link(ListenerPid, Socket, Transport, Opts) ->
    State = #state{socket=Socket, transport=Transport, opts=Opts},
    gen_server:start_link(?MODULE, {State, ListenerPid}, []).

%% OTP API
notify(PID, Event) ->
    gen_server:cast(PID, {notify, Event}).

%% gen_server callbacks
init({#state{}=State, ListenerPid}) ->
    {ok, Player} = erlmud_sup:start_player({?MODULE, self()}),
    % Synchronous start_link/init, hence ranch accept delayed
    gen_server:cast(self(), {post_init, ListenerPid}),
    {ok, State#state{player=Player}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({notify, Event}, State) ->
    handle_notify(Event, State),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({post_init, ListenerPid}, State) ->
    post_init(ListenerPid, State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp_closed, Socket}, #state{socket=Socket}=State) ->
    {stop, linkdead, State};
handle_info({tcp, Socket, Msg}, #state{socket=Socket}=State) ->
    NewState = build_lines(Msg, State),
    once_active(State),
    {noreply, NewState};
handle_info(_Info, State) ->
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
    send(<<"\r\n\r\nWelcome to erlmud.\r\n\r\n">>, State),
    handle_command(<<"look">>, [], State),
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
    [Command|Args] = binary:split(Msg, <<" ">>),
    handle_command(Command, Args, State),
    send_prompt(State).

% @TODO: Really should consider migrating handle_command "Commands" into a FSM
% or something
handle_command(<<"">>, _Args, _State) ->
    ok;
handle_command(<<"say">>, Args, #state{player=Player}) ->
    erlmud_player:say(Player, Args);
handle_command(<<"go">>, _Args, State) ->
    send("There is no \"go\" command. Just type the direction you want to go.\r\n", State);
handle_command(<<"quit">>, _Args, #state{}=State) ->
    send("Goodbye\r\n", State),
    gen_server:cast(self(), stop);
handle_command(<<"look">>, _Args, #state{player=Player}=State) ->
    Exits = case get_exits(State) of
        [] -> "none";
        ExitList ->
            [
                "[ ",
                string:join(ExitList, ", "),
                " ]"
                ]
    end,
    Players = case erlmud_player:get_room(Player, players) of
        PlayerList ->
            [io_lib:format("~p is here\r\n", [PID])
             || PID <- PlayerList -- [Player]]
    end,
    Out = [
            erlmud_player:get_room(Player, name),
            <<"\r\n">>,
            <<"  ">>,
            erlmud_player:get_room(Player, desc),
            <<"\r\n">>,
            <<"\r\n">>,
            <<"Exits: ">>,
            Exits,
            <<"\r\n">>,
            <<"\r\n">>,
            Players,
            <<"\r\n">>
            ],
    send(Out,
         State);
handle_command(Command, _Args, #state{player=Player}=State) ->
    case [erlmud_player:go(Player, Exit)
          || Exit <- get_exits(State),
             erlang:list_to_bitstring(Exit) == Command
            ] of
        [] ->
            send(["Unknown command: ", Command, "\r\n"], State);
        [ok] -> handle_command(<<"look">>, [], State)
    end.

get_exits(#state{player=Player}) ->
    [Exit || {Exit, _RoomId} <- erlmud_player:get_room(Player, exits)].

handle_notify({said, Player, Msg}, State) ->
    send(io_lib:format("\r\n~p says \"~ts\"\r\n", [Player, Msg]), State),
    send_prompt(State);
handle_notify({you_said, Msg}, State) ->
    send(io_lib:format("\r\nYou say \"~ts\"\r\n", [Msg]), State),
    send_prompt(State);
handle_notify({exited_room, Player, quit}, State) ->
    send(io_lib:format("\r\n~p has quit.\r\n", [Player]), State),
    send_prompt(State);
handle_notify({exited_room, Player, {direction, Direction}}, State) ->
    send(io_lib:format("\r\n~p leaves to the ~ts\r\n", [Player, Direction]), State),
    send_prompt(State);
handle_notify({entered_room, Player}, State) ->
    send(io_lib:format("\r\n~p enters the room\r\n", [Player]), State),
    send_prompt(State);
handle_notify(Event, State) ->
    send(io_lib:format("\r\nDunno how to handle: ~p\r\n", [Event]), State),
    send_prompt(State).
