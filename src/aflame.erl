-module(aflame).
-compile([{parse_transform, lager_transform}]).

-export([
    doAll/0,
    test/0,
    flatten_main/2,
    flatten_file/2]).

test() ->
    aflame:flatten_file(<<"/home/ross/zentrace/onResume.trace">>, <<"/home/ross/zentrace">>).

flatten_main(InFile, OutPath) ->
    {ok, Parser} = aflame_trace_parser:start_link({file, InFile}),
    flatten_thread(Parser, OutPath, <<"main">>),
    aflame_trace_parser:close(Parser).

flatten_file(InFile, OutPath) ->
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
    OutName = binary:replace(ThreadName, <<"/">>, <<".">>, [global]),
    OutFile = <<OutPath/binary, "/", OutName/binary, ".flat">>,
    lager:info("Start writing output file ~s~n", [OutFile]),
    {ok, File} = file:open(OutFile, [write]),
    file:truncate(File),
    file:write(File, Profile),
    lager:info("Finished writing ~s~n", [OutFile]).

doAll() ->
    Base = <<"/home/ross/zentrace/">>,
    [spawn(?MODULE, flatten, [
       <<Base/binary, Name/binary, ".trace">>,
       Base
   ]) || Name <- [
                  <<"onCreate1">>,
                  <<"onCreate2">>,
                  <<"onCreate3">>,
                  <<"onStart">>,
                  <<"onResume">>
                 ]].
