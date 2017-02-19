-module(aflame_fs).

-compile([{parse_transform, lager_transform}]).

-define(TEMP_DIR, "upload").
-define(OUTPUT_DIR, "generated").
-define(BLOCK_SIZE, 1048576).

-export([
         get_temp_file/0,
         get_trace_file/1,
         output_dir/1,
         rename_to_md5/1
        ]).

base_dir() ->
    {ok, Paths} = application:get_env(aflame, paths),
    proplists:get_value(base_directory, Paths).

temp_dir() ->
    filename:join(base_dir(), ?TEMP_DIR).

output_dir() ->
    filename:join(base_dir(), ?OUTPUT_DIR).
output_dir(Md5) ->
    filename:join(output_dir(), Md5).

get_trace_file(Md5) ->
    filename:join(
      [
       output_dir(Md5),
       io_lib:format("~s.trace", [Md5])
      ]).

get_temp_file() ->
    Name = uuid:to_string(uuid:uuid4()),
    FqName = filename:join(temp_dir(), Name),
    ok = filelib:ensure_dir(FqName),
    lager:info("Creating temp file ~p~n", [FqName]),
    {ok, Fd} = file:open(FqName, [write]),
    {ok, Name, Fd}.

rename_to_md5(Name) ->
    FqName = filename:join(temp_dir(), Name),
    Hash = crypto:hash_init(md5),
    lager:info("Hashing input file ~p~n", [FqName]),
    {ok, Fd} = file:open(FqName, [read]),
    {ok, Hash1} = hash_file(Fd, Hash),
    Digest = crypto:hash_final(Hash1),
    Md5 = digest_to_ascii(Digest),
    file:close(Fd),
    NewFqName = get_trace_file(Md5),
    ok = filelib:ensure_dir(NewFqName),
    ok = file:rename(FqName, NewFqName),
    {ok, Md5}.

hash_file(Fd, Hash) ->
    case file:read(Fd, ?BLOCK_SIZE) of
        {ok, Data} ->
            crypto:hash_update(Hash, Data),
            hash_file(Fd, Hash);
        eof ->
            {ok, Hash};
        {error, Reason} ->
            {error, Reason}
    end.

digest_to_ascii(Digest) ->
    lists:flatten(
      [[io_lib:format("~2.16.0B",[X]) || <<X:8>> <= Digest ]]
     ).
