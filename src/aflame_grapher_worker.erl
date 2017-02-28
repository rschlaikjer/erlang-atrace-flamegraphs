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
    ThreadIds = lists:sort(aflame_trace_parser:get_thread_ids(Parser)),
    OutNames = lists:map(
                 fun(ThreadId) -> flatten_thread(Parser, OutPath, ThreadId) end,
                 ThreadIds
                ),
    SvgNames = lists:filter(
                 fun(Name) -> Name /= undefined end,
                 OutNames
                ),
    write_index_file(Md5, SvgNames),
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
                  quote_string(OutFile), ">", quote_string(GraphFile)
                 ]
                ),
    _Ret = os:cmd(GraphCmd),
    {ok, ThreadIdBin}.

quote_string(String) ->
    io_lib:format("'~s'", [String]).
