-module(erlmud_telnet_protocol).
-export([start_link/4]).
-export([init/4]).
-include("log.hrl").

start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
    {ok, Pid}.

init(ListenerPid, Socket, Transport, _Opts=[]) ->
    ok = ranch:accept_ack(ListenerPid),
    Transport:send(Socket, <<"Hello world.\r\n">>),
    Transport:close(Socket).
