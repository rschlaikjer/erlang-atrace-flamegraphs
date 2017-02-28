# AFlame
Visualizer for android profiling information

AFlame is a tool for extracting data from Android method trace files without
having to use
[Traceview](https://developer.android.com/studio/profile/traceview.html).

In order to generate some trace files, wrap your code under test like so:

    Debug.startMethodTracing("traceName", 128 * 1024 * 1024);
    // Code under test
    Debug.stopMethodTracing();

If you are testing a particularly intensive section, you can use the sampling
tracing method available on newer APIs that considerably ameliorates the
tracer's performance impact.

This project relies on Brendan Gregg's excellent [Flamegraph](https://github.com/brendangregg/FlameGraph)
project to generate the graphs themselves.

To test out this tool without running it yourself, you can upload your trace
file to https://aflame.rhye.org/ and get a result pretty quickly. If you don't
have any traces yet, you can take a look at a sample trace from a recipe app
[here](https://aflame.rhye.org/trace/612054F4E2322B45066E3A882ABD51FB)

I have posted in depth about the Android trace format
[here](https://blog.rhye.org/post/android-profiling-flamegraphs/).

![onCreate](/main.png?raw=true "Main thread")

To run a local copy of AFlame (with [rebar](http://www.rebar3.org/)):

    git clone git@github.com:rschlaikjer/erlang-atrace-flamegraphs.git aflame
    cd aflame
    # Update files/app.config to use a port and trace output location that suit
    ERL_FLAGS="-config files/app.config" rebar3 shell
    # Open it up in your browser
    xdg-open http://localhost:8192/

## Android Trace file format
The information here is based on the ART implementation of tracing, which can be viewed here:
https://android.googlesource.com/platform/art/+/master/runtime/trace.cc
https://android.googlesource.com/platform/art/+/master/runtime/trace.h

### *version section
Plain text section of key=value pairs. Includes general info about the trace.

- data-file-overflow: Whether or not this trace overflowed its buffer and so is missing events
- clock: One of `thread-cpu` (Clock is CPU time), `wall` (Clock is wall clock) or `dual` (Both are present)
- elapsed-time-usec: total time elapsed during the trace
- num-method-calls: total number of calls contained in the trace
- clock-call-overhead-nsec: The time it takes to actually check the clock.
- vm: Art or Dalvik
- pid: PID of the process under trace

For traces with GC tracking, the header can also contain:
- alloc-count: Number of allocated objects
- alloc-size: Size of allocated objects (bytes)
- gc-count: Number of GCs completed during trace

### *threads section
Plain text section of tab separated pairs.
Each pair is a base 10 thread ID, and a thread name.
This numeric thread ID is referenced from the trace data section.

### *methods section
Plain text section of tab separated pairs.
Each line is a 5-tuple of:
- Method ID (in hex). A primary key for referring to a method entry.
- Class name
- Method name
- Type signature
- Source file
- Source line (not always present)

The method ID is referenced from the trace data section.
The number here is actually the real method ID left shifted two bits - this is
explained in the data section.

### Raw trace data section
All numbers are stored as little-endian.

Header:
- Magic (4 bytes): 'SLOW'
- Version (2 bytes): Most traces seem to be version 3.
- Data Offset (2 bytes): The number of bytes after the header that the data section begins.
- Start Time (8 bytes): The time (microseconds) that the trace began
- Recordsize (2 bytes): The number of bytes in each trace record.

Data records (v3):
- ThreadId (2 bytes): References the threads table above
- MethodId (4 bytes): Compound method ID and method Action (enter 0x0, exit 0x1 and unwind 0x2)
- TimeDelta (4 bytes): Offset into the trace at which this record was logged (microsecs)
- Wall clock delta (4 bytes) Offset into the trace (wall time) at which this record was logged

Each method record denotes an event that occurred in the given method. The
method itself is identified by the high 14 bits of the compound method ID,
which matches against the methods table higher in the file. The action is one
of three currently defined:
- 0x0 (Method enter): Recorded when the runtime enters a method call
- 0x1 (Method exit): Recorded when the runtime exits a method call
- 0x2 (Unwind): Recorded when the runtime unwinds the stack. Seems to be
                functionally equivalent to a return.

I interpret the unwind as a return for the purposes of the graphs; this is
based on what I can glean from reading up on when an unwind is issued by the
interpreter
[here](https://android.googlesource.com/platform/art/+/master/runtime/interpreter/interpreter_common.cc#399)
, and the part of the interpreter that then checks for `kDexNoIndex`
[here](https://android.googlesource.com/platform/art/+/master/runtime/interpreter/interpreter.cc#315)

By pushing and popping the records onto a virtual stack per thread, you can recreate the trace, and use the offsets to work
out how much time was spent in each function.

All parsing code can be seen [here](https://github.com/rschlaikjer/erlang-atrace-flamegraphs/blob/master/src/aflame_trace_parser.erl#L259).
