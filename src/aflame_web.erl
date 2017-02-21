-module(aflame_web).
-define(HTTP_CHUNK_SIZE, 1048576).
-define(MAX_TRACE_SIZE, 134217728).

-export([
         start/0,
         init/3,
         handle/2,
         terminate/3
        ]).

-compile([{parse_transform, lager_transform}]).

start() ->
    {ok, ServerInfo} = application:get_env(aflame, server),
    Port = proplists:get_value(http_port, ServerInfo),
    Dispatch = cowboy_router:compile(
                 [
                  {'_', [
                         {"/", cowboy_static, {priv_file, aflame, "static/index.html"}},
                         {"/static/[...]", cowboy_static, {priv_dir, aflame, "static"}},
                         {'_', ?MODULE, []}
                        ]}
                 ]),
    {ok, _} = cowboy:start_http(
                http, 100, [{port, Port}], [{env, [{dispatch, Dispatch}]}]
               ),
    ok.

max_trace_size() ->
    {ok, ServerInfo} = application:get_env(aflame, server),
    MaxSize = proplists:get_value(max_upload_size, ServerInfo),
    MaxSize.

init(_Transport, Req, [])->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Path, Req} = cowboy_req:path(Req),
    Args = string:tokens(binary_to_list(Path), "/"),
    try handle_rest(Req, Args) of
        {ok, Req2} -> {ok, Req2, State}
    catch
        throw:{Code, ResponseText} ->
            {ok, Req2} = write_reply(Req, ResponseText, Code),
            {ok, Req2, State};
        throw:Throw ->
            lager:error("Caught throw:~p~n", [Throw]),
            {ok, Req2} = internal_error(Req),
            {ok, Req2, State};
        error:Error ->
            lager:error("Caught ~p error:~p~n", [Error, erlang:get_stacktrace()]),
            {ok, Req2} = internal_error(Req),
            {ok, Req2, State}
    end.

write_reply(Req, Data) ->
    write_reply(Req, Data, 200).
write_reply(Req, Data, Code) ->
    cowboy_req:reply(
      Code, [
             {<<"content-type">>, <<"text/plain; charset=utf-8">>}
            ], Data, Req).

internal_error(Req) ->
    write_reply(Req, "Internal error", 500).

handle_rest(Req, ["upload_trace"]) ->
    upload_trace(Req);
handle_rest(Req, ["trace", Trace]) ->
    view_trace(Req, Trace);
handle_rest(Req, ["trace", Trace, SvgName]) ->
    view_svg(Req, Trace, SvgName);
handle_rest(Req, Path) ->
    lager:info("Requested unknown url: ~p~n", [Path]),
    write_reply(Req, "Unknown URL", 404).

view_trace(Req, Trace) ->
    case aflame_fs:trace_exists(Trace) of
        false -> write_reply(Req, "Trace not found", 404);
        true ->
            IndexFile = filename:join(
                          aflame_fs:output_dir(Trace),
                          "index.html"
                         ),
            case filelib:is_file(IndexFile) of
                false ->
                    reply_file(Req, "priv/static/processing.html");
                true ->
                    reply_file(Req, IndexFile)
            end
    end.


reply_file(Req, FileName) ->
    F = fun (Socket, Transport) ->
                Transport:sendfile(Socket, FileName)
        end,
    Req2 = cowboy_req:set_resp_body_fun(F, Req),
    cowboy_req:reply(
      200,
      [{"content-type", "text/html; encoding=utf-8"}],
      Req2
     ).

view_svg(Req, Trace, Svg) ->
    case aflame_fs:trace_exists(Trace) of
        false -> write_reply(Req, "Trace not found", 404);
        true ->
            File = filename:join(
                     aflame_fs:output_dir(Trace),
                     Svg
                    ),
            F = fun (Socket, Transport) ->
                        Transport:sendfile(Socket, File)
                end,
            Req2 = cowboy_req:set_resp_body_fun(F, Req),
            cowboy_req:reply(
              200,
              [{"content-type", "image/svg+xml; encoding=utf-8"}],
              Req2
             )
    end.

upload_trace(Req) ->
    case cowboy_req:parse_header(<<"content-type">>, Req) of
        {ok, {<<"multipart">>, <<"form-data">>, _}, Req2} ->
            upload_trace_multipart(Req2);
        Other -> write_reply(
                   Req,
                   io_lib:format("Unexpected request type ~p~n", [Other])
                  )
    end.

upload_trace_multipart(Req) ->
    case cowboy_req:part(Req) of
        {ok, Headers, Req2} ->
            Req4 = case cow_multipart:form_data(Headers) of
                       {data, FieldName} ->
                           lager:info("Got unknown data field: ~p~n", [FieldName]),
                           {ok, _Body, Req3} = cowboy_req:part_body(Req2),
                           Req3;
                       {file, <<"trace_file">>, _Filename, _CType, _CTransferEncoding} ->
                           {ok, OutName, OutFile} = aflame_fs:get_temp_file(),
                           case stream_trace_to_file(Req2, OutFile) of
                               {ok, Req3} ->
                                   file:close(OutFile),
                                   {ok, Md5} = aflame_fs:rename_to_md5(OutName),
                                   lager:info("Wrote new trace to ~p~n", [Md5]),
                                   aflame_grapher_worker:process_trace(Md5),
                                   BMd5 = list_to_binary(Md5),
                                   {ok, Req5} = cowboy_req:reply(
                                                  302,
                                                  [{<<"Location">>, <<"/trace/", BMd5/binary>>}],
                                                  <<"">>,
                                                  Req3
                                                 ),
                                   Req5;
                               {error, trace_too_large} ->
                                   {ok, Req3} = write_reply(
                                                  Req,
                                                  io_lib:format("Tracefile too large - max size ~p~n", [max_trace_size()])
                                                 ),
                                   Req3
                           end;
                       {file, FieldName, Filename, _CType, _CTransferEncoding} ->
                           lager:info("Got unknown file field: ~p / ~p~n", [FieldName, Filename]),
                           {ok, Req2}
                   end,
            upload_trace_multipart(Req4);
        {done, Req2} ->
            {ok, Req2}
    end.

stream_trace_to_file(Req, OutFile) ->
    stream_trace_to_file(Req, OutFile, 0).
stream_trace_to_file(_Req, _OutFile, Bytes) when Bytes > ?MAX_TRACE_SIZE ->
    {error, trace_too_large};
stream_trace_to_file(Req, OutFile, Bytes) ->
    case cowboy_req:part_body(Req) of
        {ok, Data, Req1} ->
            file:write(OutFile, Data),
            {ok, Req1};
        {more, Data, Req1} ->
            file:write(OutFile, Data),
            stream_trace_to_file(Req1, OutFile, Bytes + byte_size(Data))
    end.

terminate(_Reason, _Req, _State) ->
    ok.
