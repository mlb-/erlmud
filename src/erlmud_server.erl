
-module(erlmud_server).

% OTP API
-export([start/0]).

% dev API
-export([restart/0]).

% functions
start() -> application:start(erlmud_server).

restart() ->
	application:stop(erlmud_server),
	?MODULE:start().

