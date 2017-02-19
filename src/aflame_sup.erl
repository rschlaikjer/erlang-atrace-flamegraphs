-module(aflame_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Children = [
    {
        aflame_parser_sup,
        {aflame_parser_sup, start_link, []},
        permanent,
        3000,
        supervisor,
        [aflame_parser_sup]
    },
        poolboy:child_spec(
          grapher,
          [{name, {local, grapher}},
           {worker_module, aflame_grapher_worker},
           {size, 10}],
          []
        )
	],
    {ok, { {one_for_one, 10, 10}, Children} }.

%%====================================================================
%% Internal functions
%%====================================================================
