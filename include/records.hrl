-record(version_header, {
          data_file_overflow :: 'true' | 'false',
          clock_type :: 'dual',
          elapsed_time :: pos_integer(),
          num_method_calls :: pos_integer(),
          clock_call_overhead :: pos_integer(),
          vm :: 'art' | 'dalvik',
          pid :: pos_integer()
         }).

-record(trace_thread, {
          thread_id :: pos_integer(),
          thread_name :: binary()
         }).

-record(trace_method, {
          method_id :: pos_integer(),
          class_name :: binary(),
          method_name :: binary(),
          signature :: binary(),
          source_file :: binary(),
          line_number :: pos_integer()
         }).

-record(records_header, {
          version :: 1..3,
          header_offset :: pos_integer(),
          data_offset :: pos_integer(),
          start_offset :: pos_integer(),
          record_size :: pos_integer()
         }).

-record(call_record, {
          thread_id :: pos_integer(),
          method_id :: pos_integer(),
          method_action :: 'enter' | 'exit' | 'unwind',
          time_delta :: pos_integer(),
          wall_time_delta :: pos_integer(),
          child_time :: pos_integer()
         }).
