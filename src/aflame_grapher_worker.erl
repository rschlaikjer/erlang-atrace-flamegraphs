-module(aflame_grapher_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-include("include/records.hrl").
-compile([{parse_transform, lager_transform}]).

-export([
         start_link/1,
         process_trace/1
        ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {md5}).

% Public API

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

process_trace(Md5) -> spawn(
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
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({process_trace, Md5}, _From, _State) ->
    do_process_trace(Md5),
    {reply, ok, #state{md5=Md5}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _Pid, normal}, #state{md5=Md5}) ->
    {noreply, #state{}};
handle_info({'EXIT', _Pid, Err}, #state{md5=Md5}) ->
    lager:info("Exception in child: ~p~n", [Err]),
    write_error_page(Md5, Err),
    {noreply, #state{}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{}) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Internal

do_process_trace(Md5) ->
    case filelib:file_size(aflame_fs:get_trace_file(Md5)) of
        0 ->
            write_error_page(Md5, "File has zero length");
        _ ->
            try parse_and_write(Md5)
            catch _:_ ->
                lager:info("Caught error processing trace: ~p~n", [erlang:get_stacktrace()])
            end
    end.

write_error_page(Md5, Stacktrace) ->
    {ok, OutFile} = file:open(aflame_fs:get_trace_index(Md5), [write]),
    ok = file:write(OutFile, [
        "<center><h1>Error processing ", Md5, "</h1><br/>",
        "Please <a target='_blank' href='https://github.com/rschlaikjer/erlang-atrace-flamegraphs/issues'>open an issue</a> if this is not what you had hoped for.",
        "</center>",
        "Guru meditation:\n<pre>", io_lib:format("~p~n", [Stacktrace]), "</pre>"
    ]),
    file:close(OutFile),
    ok.

parse_and_write(Md5) ->
    lager:info("Worker asked to process trace: ~p~n", [Md5]),
    InFile = aflame_fs:get_trace_file(Md5),
    OutPath = list_to_binary(aflame_fs:output_dir(Md5)),
    {ok, Parser} = aflame_trace_parser:start_link({file, InFile}),
    ThreadIds = lists:sort(aflame_trace_parser:get_thread_ids(Parser)),
    OutNames = lists:filter(
                 fun(Name) -> Name /= undefined end,
                 lists:map(
                   fun(ThreadId) -> flatten_thread(Parser, OutPath, ThreadId) end,
                   ThreadIds
                  )
                ),
    write_index_file(Md5, OutNames),
    % Unlink the intermediate flat files
    lists:map(
      fun(ThreadIdBin) ->
              FileName = <<ThreadIdBin/binary, ".flat">>,
              QualName = filename:join([OutPath, FileName]),
              file:delete(QualName)
      end,
      OutNames
     ),
    aflame_trace_parser:close(Parser).

write_index_file(Md5, SvgNames) ->
    {ok, OutFile} = file:open(aflame_fs:get_trace_index(Md5), [write]),
    ok = file:write(OutFile, ["<center><h1>", Md5, "</h1><br/>"]),
    lists:map(
      fun (SvgName) ->
          ok = write_svg_entry(OutFile, Md5, SvgName)
      end,
      SvgNames
     ),
    file:close(OutFile),
    ok.

write_svg_entry(OutFile, Md5, SvgName) when is_binary(SvgName) ->
    write_svg_entry(OutFile, Md5, binary_to_list(SvgName));
write_svg_entry(OutFile, Md5, SvgName) ->
    file:write(
      OutFile,
      ["<object data='/trace/", Md5, "/", SvgName, ".svg' type='image/svg+xml'></object><br/>"]
     ).

flatten_thread(Parser, OutPath, ThreadId) ->
    Profile = aflame_trace_parser:get_flat_profile(Parser, ThreadId),
    case maps:size(Profile) of
        0 -> undefined;
        _ ->
            {ok, OutName} = write_thread(Parser, Profile, OutPath, ThreadId),
            OutName
    end.

profile_to_iolist(Profile) ->
    Res = maps:fold(
      fun (K, V, Acc) ->
          VBin = integer_to_binary(V),
          KList = lists:map(fun(K1) -> [K1|";"] end, K),
          [KList, " ", VBin, "\n", Acc]
      end,
      [],
      Profile
    ),
    Res.

write_thread(Parser, Profile, OutPath, ThreadId) ->
    #trace_thread{thread_name=ThreadName} =
        aflame_trace_parser:get_thread(Parser, ThreadId),
    ThreadIdBin = integer_to_binary(ThreadId),
    OutFile = <<OutPath/binary, "/", ThreadIdBin/binary, ".flat">>,
    {ok, File} = file:open(OutFile, [write]),
    file:truncate(File),
    file:write(File, profile_to_iolist(Profile)),
    file:close(File),
    GraphFile = <<OutPath/binary, "/", ThreadIdBin/binary, ".svg">>,
    GraphCmd = lists:join(
                 " ",
                 [
                  "flamegraph",
                  "--title", quote_string(ThreadName),
                  "--hash",
                  "--countname", "microseconds",
                  binary_to_list(OutFile), ">", binary_to_list(GraphFile)
                 ]
                ),
    _Ret = os:cmd(GraphCmd),
    {ok, ThreadIdBin}.

quote_string(String) when is_list(String) ->
    quote_string(list_to_binary(String));
quote_string(String) when is_binary(String) ->
    Safe = binary:replace(
             String,
             [<<";">>, <<"&">>, <<"/">>, <<"\\">>, <<"'">>, <<"\"">>],
             <<"">>,
             [global]
            ),
    io_lib:format("'~s'", [Safe]).
