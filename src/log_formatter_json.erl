%% Copyright (c) 2020 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
%% REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
%% AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
%% INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
%% OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
%% PERFORMANCE OF THIS SOFTWARE.

-module(log_formatter_json).

-export([format/4]).

-spec format(unicode:chardata(), logger:level(), logger:metadata(),
             log_formatter:config()) ->
        unicode:chardata().
format(String, Level, Metadata, _Config) ->
  Domain = maps:get(domain, Metadata, []),
  Time = maps:get(time, Metadata, erlang:system_time(microsecond)),
  IgnoredMetadata = [domain, time, % duplicate
                     error_logger, logger_formatter, report_cb, % useless
                     gl], % added by the logger
  Metadata2 = maps:without(IgnoredMetadata, Metadata),
  Msg = #{level => atom_to_binary(Level),
          domain => log_formatter:format_domain(Domain),
          time => Time,
          message => unicode:characters_to_binary(String),
          data => format_metadata(Metadata2)},
  [json:serialize(Msg), $\n].

-spec format_metadata(logger:metadata()) -> logger:metadata().
format_metadata(Metadata) ->
  maps:fold(fun format_metadata_entry/3, #{}, Metadata).

-spec format_metadata_entry(atom(), term(), logger:metadata()) ->
        logger:metadata().
format_metadata_entry(file, Value, Acc) ->
  Acc#{file => unicode:characters_to_binary(Value)};
format_metadata_entry(mfa, {M, F, A}, Acc) ->
  Acc#{mfa => [atom_to_binary(M), atom_to_binary(F), A]};
format_metadata_entry(pid, Value, Acc) ->
  Acc#{pid => list_to_binary(pid_to_list(Value))};
format_metadata_entry(Key, Value, Acc) ->
  case is_json_serializable(Value) of
    true ->
      Acc#{Key => Value};
    false ->
      Data = io_lib:format(<<"~0tp">>, [Value]),
      Acc#{Key => unicode:characters_to_binary(Data)}
  end.

-spec is_json_serializable(term()) -> boolean().
is_json_serializable(null) ->
  true;
is_json_serializable(V) when is_boolean(V); is_number(V); is_binary(V) ->
  true;
is_json_serializable(V) when is_list(V) ->
  lists:all(fun is_json_serializable/1, V);
is_json_serializable(V) when is_map(V) ->
  lists:all(fun
              ({MK, MV}) when is_atom(MK); is_binary(MK); is_list(MK) ->
                is_json_serializable(MV);
              ({_, _}) ->
                false
            end, maps:to_list(V));
is_json_serializable(_) ->
  false.
