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
    InFile = aflame_fs:get_trace_file(Md5),
    OutPath = list_to_binary(aflame_fs:output_dir(Md5)),
    {ok, Parser} = aflame_trace_parser:start_link({file, InFile}),
    ThreadNames = aflame_trace_parser:get_thread_names(Parser),
    lists:map(
      fun(ThreadName) -> flatten_thread(Parser, OutPath, ThreadName) end,
      ThreadNames
     ),
    aflame_trace_parser:close(Parser).

flatten_thread(Parser, OutPath, ThreadName) ->
    lager:info("Flattening thread: ~p~n", [ThreadName]),
    Profile = aflame_trace_parser:get_flat_profile(Parser, ThreadName),
    case maps:size(Profile) of
        0 -> ok;
        _ -> write_thread(Profile, OutPath, ThreadName)
    end.

write_thread(Profile, OutPath, ThreadName) ->
    OutBinary = maps:fold(
                  fun(K, V, Acc) ->
                          VBin = integer_to_binary(V),
                          <<K/binary, " ", VBin/binary, "\n", Acc/binary>>
                  end,
                  <<"">>,
                  Profile
                 ),
    OutName = binary:replace(ThreadName, <<"/">>, <<".">>, [global]),
    OutFile = <<OutPath/binary, "/", OutName/binary, ".flat">>,
    GraphFile = <<OutPath/binary, "/", OutName/binary, ".svg">>,
    lager:info("Start writing output file ~s~n", [OutFile]),
    {ok, File} = file:open(OutFile, [write]),
    file:truncate(File),
    file:write(File, OutBinary),
    lager:info("Finished writing ~s~n", [OutFile]),
    GraphCmd = lists:join(
                 " ",
                 [
                  "flamegraph",
                  "--title", quote_string(ThreadName),
                  "--hash",
                  "--countname", "nanoseconds",
                  quote_string(OutFile), ">", quote_string(GraphFile)
                 ]
                ),
    _Ret = os:cmd(GraphCmd),
    ok.

quote_string(String) ->
    io_lib:format("'~s'", [String]).
