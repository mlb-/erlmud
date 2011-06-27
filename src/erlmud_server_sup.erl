
-module(erlmud_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("supervisor.hrl").

%% API functions
-spec start_link() -> 'ignore' | {'error',_Reason} | {'ok',pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Supervisor callbacks

init([]) ->
		Children = [
			%?CHILD(erlmud_server_worker, worker)
			%?CHILD(erlmud_server_other_sup, supervisor)
			?CHILD(erlmud_server_area_sup, supervisor)
		],
    {ok, { {one_for_one, 5, 10}, Children} }.

