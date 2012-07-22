-module(erlmud_app).
-behaviour(application).

%% OTP API
-export([start/2, stop/1]).

%% application callbacks
start(_StartType, _StartArgs) ->
    erlmud_sup:start_link().

stop(_State) ->
    ok.
