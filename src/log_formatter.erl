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

-module(log_formatter).

-export([format/2, format_level/1, format_domain/1]).

-export_type([format/0, config/0, msg/0]).

-type format() :: text | json.

-type config() :: #{debug => boolean(),
                    format => format(),
                    include_time => boolean(),
                    atom() => any()}.

-type msg() :: {io:format(), [term()]}
             | {report, logger:report()}
             | {string, unicode:chardata()}.

-spec format(logger:log_event(), config()) ->
        unicode:chardata().
format(Event = #{level := Level, msg := Msg, meta := Metadata}, Config) ->
  case maps:get(debug, Config, false) of
    true ->
      io:format("log event:~n~p~n", [Event]);
    false ->
      ok
  end,
  format_msg(Msg, Level, Metadata, Config).

-spec format_msg(msg(), logger:level(), logger:metadata(), config()) ->
        unicode:chardata().

format_msg({string, String}, Level, Metadata, Config) ->
  case maps:get(format, Config, text) of
    text ->
      log_formatter_text:format(String, Level, Metadata, Config);
    json ->
      log_formatter_json:format(String, Level, Metadata, Config);
    Format ->
      error({unknown_format, Format})
  end;

format_msg({report, Report}, Level, Metadata = #{report_cb := Fun}, Config) when
    is_function(Fun, 1) ->
  {FormatData, Args} = Fun(Report),
  Format = unicode:characters_to_binary(FormatData),
  format_msg({Format, Args}, Level, Metadata, Config);

format_msg({report, Report}, Level, Metadata = #{report_cb := Fun}, Config) when
    is_function(Fun, 2) ->
  String = Fun(Report, #{}),
  format_msg({string, String}, Level, Metadata, Config);

format_msg({report, Report}, Level, Metadata, Config) ->
  format_msg({report, Report}, Level,
             Metadata#{report_cb => fun logger:format_report/1},
             Config);

format_msg({Format, Args}, Level, Metadata, Config) ->
  String = io_lib:format(Format, Args),
  format_msg({string, String}, Level, Metadata, Config).

-spec format_level(logger:level()) -> unicode:chardata().
format_level(Level) ->
  atom_to_binary(Level).

-spec format_domain([atom()]) -> binary().
format_domain(Domain) ->
  Parts = lists:map(fun erlang:atom_to_binary/1, Domain),
  iolist_to_binary(lists:join($., Parts)).
