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
    OutNames = lists:map(
                 fun(ThreadName) -> flatten_thread(Parser, OutPath, ThreadName) end,
                 ThreadNames
                ),
    SvgNames = lists:filter(
                 fun(Name) -> Name /= undefined end,
                 OutNames
                ),
    write_index_file(Md5, lists:sort(SvgNames)),
    aflame_trace_parser:close(Parser).

write_index_file(Md5, SvgNames) ->
    {ok, OutFile} = file:open(aflame_fs:get_trace_index(Md5), [write]),
    ok = file:write(OutFile, ["<center><h1>", Md5, "</h1><br/>"]),
    % Always put the main thread at the top
    ok = write_svg_entry(OutFile, Md5, "main"),
    lists:map(
      fun (SvgName) ->
              case SvgName of
                  <<"main">> ->
                      ok;
                  Name ->
                      ok = write_svg_entry(OutFile, Md5, Name)
              end
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

flatten_thread(Parser, OutPath, ThreadName) ->
    Profile = aflame_trace_parser:get_flat_profile(Parser, ThreadName),
    case maps:size(Profile) of
        0 -> undefined;
        _ ->
            {ok, OutName} = write_thread(Profile, OutPath, ThreadName),
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

write_thread(Profile, OutPath, ThreadName) ->
    OutName = binary:replace(
                ThreadName,
                [<<"/">>, <<" ">>, <<"#">>],
                <<".">>,
                [global]
               ),
    OutFile = <<OutPath/binary, "/", OutName/binary, ".flat">>,
    {ok, File} = file:open(OutFile, [write]),
    file:truncate(File),
    file:write(File, profile_to_iolist(Profile)),
    file:close(File),
    GraphFile = <<OutPath/binary, "/", OutName/binary, ".svg">>,
    GraphCmd = lists:join(
                 " ",
                 [
                  "flamegraph",
                  "--title", quote_string(ThreadName),
                  "--hash",
                  "--countname", "microseconds",
                  quote_string(OutFile), ">", quote_string(GraphFile)
                 ]
                ),
    _Ret = os:cmd(GraphCmd),
    {ok, OutName}.

quote_string(String) ->
    io_lib:format("'~s'", [String]).
