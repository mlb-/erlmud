-module(erlmud_app).
-behaviour(application).

%% OTP API
-export([start/2, stop/1]).
-export([start_phase/3]).

%% application callbacks
start(_StartType, _StartArgs) ->
    erlmud_sup:start_link().

start_phase(cowboy, _StartType, _PhaseStartArgs) ->
    Priv = code:priv_dir(erlmud),
    {ok, Out} = file:consult(Priv ++ "/cowboy.dispatch"),
    [Acceptors, Port, Dispatch] = [proplists:get_value(Key, Out)
                                   || Key <- [acceptors, port, dispatch]],
    {ok, _PID} = cowboy:start_listener(websocket_listener, Acceptors,
                                      cowboy_tcp_transport, [{port, Port}],
                                      cowboy_http_protocol, [{dispatch, Dispatch}]),
    ok;
start_phase(telnet, _StartType, _PhaseStartArgs) ->
    Priv = code:priv_dir(erlmud),
    {ok, Out} = file:consult(Priv ++ "/telnet.ranch"),
    [Acceptors, Port,
     Protocol, ProtocolOpts] = [proplists:get_value(Key, Out)
                                || Key <- [acceptors, port,
                                           protocol, protocol_opts]],
    {ok, _PID} = ranch:start_listener(telnet_listener, Acceptors,
                                      ranch_tcp, [{port, Port}],
                                      Protocol, ProtocolOpts),
    ok;
start_phase(Phase, _StartType, _PhaseStartArgs) ->
    {error, {unknown_start_phase, Phase}}.

stop(_State) ->
    ok.
