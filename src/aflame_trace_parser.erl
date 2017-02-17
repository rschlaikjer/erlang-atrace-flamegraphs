-module(aflame_trace_parser).
-include("include/records.hrl").
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

%% Public API
-export([start_link/1,
         test/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
          trace_data :: binary(),
          ets_threads :: ets:tid(),
          ets_methods :: ets:tid()
         }).

start_link({file, FileName}) when is_binary(FileName) ->
    gen_server:start_link(?MODULE, [{file, FileName}], []).

test() ->
    {ok, Pid} = start_link({file, <<"/home/ross/onResume.trace">>}),
    Pid.

init([{file, FileName}]) ->
    lager:info("Reading trace data for file ~p~n", [FileName]),
    case file:read_file(FileName) of
        {ok, RawTrace} -> init([{binary, RawTrace}]);
        {error, Reason} -> {stop, {file_error, Reason}}
    end;
init([{binary, RawTrace}]) ->
    lager:info("Started parser with ~p bytes of trace data~n", [byte_size(RawTrace)]),
    {ThreadETS, MethodETS} = init_ets(),
    gen_server:cast(self(), parse_trace),
    {ok, #state{
            trace_data=RawTrace,
            ets_threads=ThreadETS,
            ets_methods=MethodETS
           }}.

handle_call(Request, From, State) ->
    lager:info("Call ~p From ~p", [Request, From]),
    {reply, ignored, State}.

handle_cast(parse_trace, State=#state{}) ->
    State1 = parse(State),
    {noreply, State1};
handle_cast(Msg, State) ->
    lager:info("Cast ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:info("Info ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(OldVsn, State, _Extra) ->
    lager:info("~p updated from vsn ~p", [?MODULE, OldVsn]),
    {ok, State}.

%%
%% Internal methods
%%

init_ets() ->
    ThreadETS = ets:new(threads_map, [set, protected, {keypos, 2}]),
    MethodETS = ets:new(methods_map, [set, protected, {keypos, 2}]),
    {ThreadETS, MethodETS}.

add_thread_record(#state{ets_threads=Ets}, Thread=#trace_thread{}) ->
    lager:info("Adding thread: ~p~n", [Thread]),
    ets:insert(Ets, Thread).

add_method_record(#state{ets_methods=Ets}, Method=#trace_method{}) ->
    lager:info("Adding method: ~p~n", [Method]),
    ets:insert(Ets, Method).

parse(State=#state{trace_data=Data}) ->
    parse_threads(State, Data),
    parse_methods(State, Data),
    ok.

parse_threads(State, Data) ->
    % Excerpt the section of the trace between the *threads marker and the
    % *methods marker
    ThreadSection = excerpt_binary(Data, <<"*threads">>, <<"*methods">>),

    % Break the section into lines
    ThreadLines = binary:split(ThreadSection, <<"\n">>, [global]),

    % For each line, parse it and if it parses add it to the thread ETS
    lists:map(
      fun(Line) ->
              case parse_thread_line(Line) of
                  Thread=#trace_thread{} -> add_thread_record(State, Thread);
                  Other -> lager:error("Failed to parse line ~p: ~p~n", [Line, Other])
              end
      end,
      ThreadLines
     ),
    ok.

parse_thread_line(Line) ->
    LineParts = binary:split(Line, <<"\t">>, [global]),
    case LineParts of
        [Id, Name] -> #trace_thread{
                         thread_id=binary_to_integer(Id),
                         thread_name=Name
                        };
        Other -> Other
    end.

parse_methods(State, Data) ->
    % Excerpt the section of the trace between the *methods marker and the
    % *end marker
    MethodSection = excerpt_binary(Data, <<"*methods">>, <<"*end">>),

    % Break the method section into lines
    MethodLines = binary:split(MethodSection, <<"\n">>, [global]),

    % For each line, parse it and if it parses add it to the thread ETS
    lists:map(
      fun(Line) ->
              case parse_method_line(Line) of
                  Method=#trace_method{} -> add_method_record(State, Method);
                  Other -> lager:error("Failed to parse line ~p: ~p~n", [Line, Other])
              end
      end,
      MethodLines
     ),
    ok.

parse_method_line(Line) ->
    LineParts = binary:split(Line, <<"\t">>, [global]),
    case LineParts of
        [MethodHex, ClassName, MethodName, Signature, Source] ->
            MethodId = erlang:binary_to_integer(
                         binary:part(MethodHex, {2, byte_size(MethodHex)-2}),
                         16
                        ),
            #trace_method{
               method_id=MethodId,
               class_name=ClassName,
               method_name=MethodName,
               signature=Signature,
               source_file=Source
              };
        Other -> Other
    end.

excerpt_binary(Data, StartKey, EndKey) ->
    {StartKeyPos, StartKeyLen} = binary:match(Data, StartKey),
    {EndKeyStart, _} = binary:match(Data, EndKey),
    ExcerptStart = StartKeyPos+StartKeyLen+1,
    ExcerptSize = EndKeyStart - ExcerptStart - 1,
    binary:part(Data, ExcerptStart, ExcerptSize).