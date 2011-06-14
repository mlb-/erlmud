-module(erlmud_server_app).

-behaviour(application).

%% application API
-export([start/2, stop/1]).

%% application functions

start(_StartType, _StartArgs) ->
    erlmud_server_sup:start_link().

stop(_State) ->
    ok.
