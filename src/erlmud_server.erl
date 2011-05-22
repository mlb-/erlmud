-module(erlmud_server).
-author('Matthew Batema <matthew.batema@gmail.com>').

% Exposed API
-export([start/0]).

start() -> application:start(erlmud_server).

