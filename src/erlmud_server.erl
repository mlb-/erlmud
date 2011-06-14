-module(erlmud_server).

% Exposed API
-export([start/0]).

% functions
-spec start() -> 'ok' | {'error',_Reason}.
start() -> application:start(erlmud_server).

