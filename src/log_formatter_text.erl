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

-module(log_formatter_text).

-export([format/4]).

-spec format(unicode:chardata(), logger:level(), logger:metadata(),
             log_formatter:config()) ->
        unicode:chardata().
format(String, Level, Metadata, Config) ->
  Domain = maps:get(domain, Metadata, []),
  TimeString = case maps:get(include_time, Config, false) of
                 true ->
                   Time = maps:get(time, Metadata,
                                   erlang:system_time(microsecond)),
                   [format_time(Time), $\s];
                 false ->
                   []
               end,
  Prefix = io_lib:format(<<"~s~-*s ~-*w ">>,
                         [TimeString,
                          9, log_formatter:format_level(Level),
                          24, Domain]),
  Indent = iolist_size(Prefix),
  IndentedString = indent_string_extra_lines(String, Indent),
  IgnoredMetadata = [domain, time, % duplicate
                     error_logger, logger_formatter, report_cb, % useless
                     mfa, file, line, % added by log macros
                     pid, gl], % added by the logger
  Metadata2 = maps:without(IgnoredMetadata, Metadata),
  IndentedMetadata = if
                       map_size(Metadata2) =:= 0 ->
                         [];
                       true ->
                         [$\n,
                          indent_string(format_metadata(Metadata2), Indent)]
                     end,
  [Prefix, IndentedString, IndentedMetadata, $\n].

-spec indent_string(unicode:chardata(), non_neg_integer()) ->
        unicode:chardata().
indent_string(String, N) ->
  Padding = io_lib:format("~*s", [N, ""]),
  [Padding, indent_string_extra_lines(String, N)].

-spec indent_string_extra_lines(unicode:chardata(), non_neg_integer()) ->
        unicode:chardata().
indent_string_extra_lines(String, N) ->
  Padding = io_lib:format("~*s", [N, ""]),
  string:replace(String, "\n", [$\n, Padding], all).

-spec format_time(SystemTime :: integer()) -> unicode:chardata().
format_time(SystemTime) ->
  calendar:system_time_to_rfc3339(SystemTime, [{unit, microsecond},
                                               {offset, "Z"}]).

-spec format_metadata(logger:metadata()) -> unicode:chardata().
format_metadata(Metadata) ->
  Pairs = maps:to_list(Metadata),
  lists:map(fun format_metadata_pair/1, Pairs).

-spec format_metadata_pair({atom(), term()}) -> unicode:chardata().
format_metadata_pair({Name, Value}) ->
  [atom_to_binary(Name), $:, $\s, io_lib:format(<<"~0tp">>, [Value]), $\n].
