-module(aflame_grapher_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-compile([{parse_transform, lager_transform}]).

-export([
         start_link/1,
         process_trace/1
        ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {}).

% Public API

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

process_trace(Md5) ->
    spawn(
      poolboy,
      transaction,
      [
       grapher,
       fun(Worker) ->
               gen_server:call(Worker, {process_trace, Md5}, infinity)
       end
      ]).

% Callbacks

init([]) ->
    {ok, #state{}}.

handle_call({process_trace, Md5}, _From, State=#state{}) ->
    do_process_trace(Md5),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{}) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Internal

do_process_trace(Md5) ->
    lager:info("Worker asked to process trace: ~p~n", [Md5]),
    ok.
