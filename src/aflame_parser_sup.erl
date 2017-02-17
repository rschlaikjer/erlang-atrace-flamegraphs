-module(aflame_parser_sup).

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

child_spec() -> {
  aflame_trace_parser,
  {aflame_trace_parser, start_link, []},
  transient,
  3000,
  worker,
  [aflame_trace_parser]
 }.

init([]) ->
    {ok, { {simple_one_for_one, 10, 10}, [child_spec()]} }.
