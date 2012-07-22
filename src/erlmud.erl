-module(erlmud).

%%% API
-export([start/0]).

%%% Functions
start() ->
    application:start(?MODULE).
