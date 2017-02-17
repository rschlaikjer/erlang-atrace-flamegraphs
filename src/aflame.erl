-module(aflame).

-export([
    flatten/2]).

flatten(InFile, OutFile) ->
    {ok, Parser} = aflame_trace_parser:start_link({file, InFile}),
    Profile = aflame_trace_parser:get_flat_profile(Parser, <<"main">>),
    [file:write_file(OutFile, <<B/binary, "\n">>, [append]) || B <- Profile],
    ok.
