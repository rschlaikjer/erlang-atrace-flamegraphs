-module(aflame_trace_parser).
-include("include/records.hrl").
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

-define(TRACE_HEADER_MAGIC, "SLOW").
-define(TIMEOUT, infinity).

%% Public API
-export([start_link/1,
         close/1,
         get_thread/2,
         get_thread_ids/1,
         get_method/2,
         get_flat_profiles/1,
         accumulate_flat_stack/2,
         get_flat_profile/2,
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
          ets_methods :: ets:tid(),
          trace_records :: [tuple()]
         }).

%% Public interface methods

start_link({file, FileName}) when is_list(FileName) ->
    start_link({file, list_to_binary(FileName)});
start_link({file, FileName}) when is_binary(FileName) ->
    gen_server:start_link(?MODULE, [{file, FileName}], []).

test() ->
    {ok, Pid} = start_link({file, <<"/home/ross/onResume.trace">>}),
    Pid.

close(Pid) ->
    gen_server:stop(Pid).

get_thread_ids(Pid) ->
    gen_server:call(Pid, get_thread_ids, ?TIMEOUT).

get_thread(Pid, ThreadId) ->
    gen_server:call(Pid, {get_thread, ThreadId}, ?TIMEOUT).

get_method(Pid, MethodId) ->
    gen_server:call(Pid, {get_method, MethodId}, ?TIMEOUT).

get_flat_profiles(Pid) ->
    gen_server:call(Pid, {get_flat_profile, all}, ?TIMEOUT).

get_flat_profile(Pid, ThreadId) ->
    gen_server:call(Pid, {get_flat_profile, {thread_id, ThreadId}}, ?TIMEOUT).

%% Gen Server implementation

init([{file, FileName}]) ->
    case file:read_file(FileName) of
        {ok, RawTrace} -> init([{binary, RawTrace}]);
        {error, Reason} -> {stop, {file_error, Reason}}
    end;
init([{binary, RawTrace}]) ->
    {ThreadETS, MethodETS} = init_ets(),
    gen_server:cast(self(), parse_trace),
    {ok, #state{
            trace_data=RawTrace,
            ets_threads=ThreadETS,
            ets_methods=MethodETS
           }}.

handle_call({get_thread, Id}, _From, State) ->
    {reply, get_thread_by_id(State, Id), State};
handle_call(get_thread_ids, _From, State) ->
    {reply, all_thread_ids(State), State};
handle_call({get_method, Id}, _From, State) ->
    {reply, get_method_by_id(State, Id), State};
handle_call({get_flat_profile, all}, _From, State) ->
    ThreadNames = all_thread_names(State),
    {reply, [
             get_profile_for_thread(State, Name)
             || Name <- ThreadNames], State};
handle_call({get_flat_profile, {thread_id, ThreadId}}, _From, State) ->
    {reply, get_profile_for_thread(State, ThreadId), State};
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
    ThreadETS = ets:new(threads_map, [set, protected, {keypos, #trace_thread.thread_id}]),
    MethodETS = ets:new(methods_map, [set, protected, {keypos, #trace_method.method_id}]),
    {ThreadETS, MethodETS}.

add_thread_record(#state{ets_threads=Ets}, Thread=#trace_thread{}) ->
    ets:insert(Ets, Thread).

get_thread_by_id(#state{ets_threads=Ets}, Id) when is_integer(Id) ->
    case ets:lookup(Ets, Id) of
        [Thread] -> Thread;
        [] -> not_found
    end.

all_thread_names(#state{ets_threads=Ets}) ->
    [Name || [Name] <- ets:match(Ets, #trace_thread{thread_name='$1', _='_'})].

all_thread_ids(#state{ets_threads=Ets}) ->
    [Id || [Id] <- ets:match(Ets, #trace_thread{thread_id='$1', _='_'})].

add_method_record(#state{ets_methods=Ets}, Method=#trace_method{}) ->
    ets:insert(Ets, Method).

get_method_by_id(#state{ets_methods=Ets}, Id) when is_integer(Id) ->
    case ets:lookup(Ets, Id) of
        [Method] -> Method;
        [] -> not_found
    end.

get_profile_for_thread(State=#state{trace_records=Records}, ThreadId) ->
    Thread = get_thread_by_id(State, ThreadId),
    ThreadCalls = [
                   Record || Record <- Records,
                             Record#call_record.thread_id == Thread#trace_thread.thread_id
                  ],
    FlatStack = accumulate_flat_stack(State, ThreadCalls),
    FlatStack.

accumulate_flat_stack(State, Calls) ->
    accumulate_flat_stack(State, Calls, [], [], maps:new()).
accumulate_flat_stack(_State, [], _TempStack, _NameStack, FlatStackMap) ->
    % Fold over the map, and turn all the keys into semicolon delimited binaries
    maps:fold(
      fun(K, V, Acc) ->
        maps:put(
          lists:reverse(K),
          V,
          Acc
        )
      end,
      maps:new(),
      FlatStackMap
    );
accumulate_flat_stack(State, [Call|Calls], TempStack, NameStack, FlatStackMap) ->
    MethodId = Call#call_record.method_id,
    IsMethodExit = MethodId rem 2 == 1,
    Method = case get_method_by_id(State, MethodId - (MethodId rem 2)) of
                 not_found ->
                     MethodIdBin = integer_to_binary(MethodId),
                     #trace_method{
                         class_name = <<"?Class">>,
                         method_name = <<"?Method<", MethodIdBin/binary, ">">>
                        };
                 M -> M
             end,
    ClassName = Method#trace_method.class_name,
    MethodName = Method#trace_method.method_name,
    MethodDesc = <<ClassName/binary, "#", MethodName/binary>>,

    NewNameStack =
    case IsMethodExit of
        false -> [MethodDesc|NameStack];
        true -> case NameStack of
                    [] -> [];
                    [_OurName|Others] -> Others
                end
    end,

    {StackEntry, NewTempStack} =
    case IsMethodExit of
        % If this is the entry of a method, then just push it to our running stack.
        false -> {undefined, [Call|TempStack]};
        % On method exit, use the entry point of the call to calculate how much
        % time we + our callees consumed. Then add our total time from our
        % parent's child_time field, if we have a parent, and push a flat stack
        % entry onto the list
        true ->
            case TempStack of
                % The beginning of the trace can have
                % methods with no entry
                [] -> {undefined, []};
                [StartFrame|[]] ->
                    % We are the only one on the stack, don't
                    % need to update a parent
                    SelfAndChildTime = Call#call_record.wall_time_delta - StartFrame#call_record.wall_time_delta,
                    SelfTime = SelfAndChildTime - StartFrame#call_record.child_time,
                    Frame = [MethodDesc | NewNameStack],
                    {{Frame, SelfTime}, []};
                [StartFrame|[Parent|Stack]] ->
                    % Calc elapsed from StartFrame and Call
                    SelfAndChildTime = Call#call_record.wall_time_delta - StartFrame#call_record.wall_time_delta,
                    SelfTime = SelfAndChildTime - StartFrame#call_record.child_time,

                    % Update the parent frame with how long this child took
                    NewParent = Parent#call_record{
                                  child_time = Parent#call_record.child_time + SelfAndChildTime
                                 },

                    % StackLine = binary_join(lists:reverse([MethodDesc | TempStack]), <<";">>),
                    % StackEntry = <<StackLine/binary, " ", SampleCount/binary>>,
                    Frame = [MethodDesc | NewNameStack],
                    {{Frame, SelfTime}, [NewParent | Stack]}
            end
    end,

    NewStackMap =
    case StackEntry of
        undefined -> FlatStackMap;
        {StackLine, Time} ->
            maps:update_with(
              StackLine,
              fun (OldTime) -> OldTime + Time end,
              Time,
              FlatStackMap
             )
    end,

    accumulate_flat_stack(State, Calls, NewTempStack, NewNameStack, NewStackMap).

parse(State=#state{trace_data=Data}) ->
    % Load threads, methods into ETS
    parse_threads(State, Data),
    parse_methods(State, Data),
    % Load the trace header
    TraceHeader = parse_trace_header(Data),
    % Load the trace records
    TraceRecords = parse_trace_records(Data, TraceHeader),
    State#state{
      trace_records=TraceRecords
     }.

parse_trace_header(Data) ->
    % Seek to the magic for the trace header
    {HeaderPos, _} = binary:match(Data, <<?TRACE_HEADER_MAGIC>>),
    % Extract the fields as binaries
    <<?TRACE_HEADER_MAGIC, Version:2/binary, DataOffset:2/binary,
      StartTime:8/binary, RecordSize:2/binary>> = binary:part(Data, {HeaderPos, 18}),
    % Decode the numbers as littl-endian
    #records_header{
       version=binary:decode_unsigned(Version, little),
       header_offset=HeaderPos,
       data_offset=binary:decode_unsigned(DataOffset, little),
       start_offset=binary:decode_unsigned(StartTime, little),
       record_size=binary:decode_unsigned(RecordSize, little)
      }.

parse_trace_records(Data, Header=#records_header{}) ->
    % Extract the subsection that contains actual records
    HeaderOffset = Header#records_header.header_offset,
    DataOffset = Header#records_header.data_offset,
    SectionStart = DataOffset + HeaderOffset,
    SectionEnd = byte_size(Data),
    RecordSection = binary_part(Data, {SectionStart, SectionEnd - SectionStart}),

    % Return a list of parsed records
    RecordSize = Header#records_header.record_size,
    Records = [
               Record || <<Record:RecordSize/binary>>
                         <= RecordSection
              ],
    ParsedRecords = [parse_trace_record(Record) || Record <- Records],
    ParsedRecords.

parse_trace_record(Record) ->
    <<ThreadId:2/binary, MethodId:4/binary,
      TimeDelta:4/binary, WallTimeDelta:4/binary>> = Record,
    #call_record{
       thread_id=binary:decode_unsigned(ThreadId, little),
       method_id=binary:decode_unsigned(MethodId, little),
       time_delta=binary:decode_unsigned(TimeDelta, little),
       wall_time_delta=binary:decode_unsigned(WallTimeDelta, little),
       child_time=0
      }.

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
               source_file=Source,
               line_number=0
              };
        [MethodHex, ClassName, MethodName, Signature, Source, SourceLine] ->
            MethodId = erlang:binary_to_integer(
                         binary:part(MethodHex, {2, byte_size(MethodHex)-2}),
                         16
                        ),
            #trace_method{
               method_id=MethodId,
               class_name=ClassName,
               method_name=MethodName,
               signature=Signature,
               source_file=Source,
               line_number=SourceLine
              };
        Other -> Other
    end.

excerpt_binary(Data, StartKey, EndKey) ->
    {StartKeyPos, StartKeyLen} = binary:match(Data, StartKey),
    {EndKeyStart, _} = binary:match(Data, EndKey),
    ExcerptStart = StartKeyPos+StartKeyLen+1,
    ExcerptSize = EndKeyStart - ExcerptStart - 1,
    binary:part(Data, ExcerptStart, ExcerptSize).
