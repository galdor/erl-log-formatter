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
format(String0, Level, Metadata, Config) ->
  %% Code is quite hairy due to the multiple possible combinations of elements
  %% in the line. Additionally, the optional colorization makes it necessary
  %% to adjust indentation and padding since color escape sequences add
  %% characters to strings but are not visible.
  Domain = maps:get(domain, Metadata, []),
  TimeString = case maps:get(include_time, Config, false) of
                 true ->
                   Time = maps:get(time, Metadata,
                                   erlang:system_time(microsecond)),
                   [format_time(Time), $\s];
                 false ->
                   []
               end,
  DomainString0 = log_formatter:format_domain(Domain),
  {DomainString, DomainLengthDiff} =
    maybe_colorize(DomainString0, green, Config),
  Prefix = io_lib:format(<<"~s~-*s ~-*ts ">>,
                         [TimeString,
                          9, log_formatter:format_level(Level),
                          24 + DomainLengthDiff, DomainString]),
  {String, StringLengthDiff} =
    case maps:find(event, Metadata) of
      {ok, Event} ->
        EventString = log_formatter:format_event(Event),
        EventPrefix0 = [$[, EventString, $], $\s],
        {EventPrefix, EventPrefixLengthDiff} =
          maybe_colorize(EventPrefix0, yellow, Config),
        {[EventPrefix, String0], EventPrefixLengthDiff};
      error ->
        {String0, 0}
    end,
  TrimmedString = string:trim(String, trailing, " \n\t"),
  PaddedString = pad_multiline_string(TrimmedString, 80 + StringLengthDiff),
  Indent = iolist_size(Prefix),
  IndentedString = indent_multiline_string(PaddedString, Indent),
  IgnoredMetadata = [domain, time, event, % duplicate
                     error_logger, logger_formatter, report_cb, % useless
                     mfa, file, line, % added by log macros
                     pid, gl], % added by the logger
  Metadata2 = maps:without(IgnoredMetadata, Metadata),
  MetadataString = if
                       map_size(Metadata2) =:= 0 ->
                         [];
                       true ->
                         [<<"  ">>, format_metadata(Metadata2, Config)]
                     end,
  [Prefix, IndentedString, MetadataString, $\n].

-spec indent_multiline_string(unicode:chardata(), non_neg_integer()) ->
        unicode:chardata().
indent_multiline_string(String, N) ->
  Padding = lists:duplicate(N, $\s),
  string:replace(String, "\n", [$\n, Padding], all).

-spec pad_multiline_string(unicode:chardata(), non_neg_integer()) ->
        unicode:chardata().
pad_multiline_string(String, N) ->
  Bin = unicode:characters_to_binary(String),
  case string:length(last_line(Bin)) of
    Len when Len < N ->
      Padding = lists:duplicate(N-Len, $\s),
      [Bin, Padding];
    _ ->
      Bin
  end.

-spec last_line(binary()) -> binary().
last_line(<<>>) ->
  <<>>;
last_line(Bin) ->
  case binary:last(Bin) of
    $\n ->
      last_line(binary:part(Bin, 0, byte_size(Bin)-1));
    _ ->
      case binary:matches(Bin, <<"\n">>) of
        [] ->
          Bin;
        Matches ->
          {Pos, _} = lists:last(Matches),
          binary:part(Bin, Pos+1, byte_size(Bin)-Pos-1)
      end
  end.

-spec format_time(SystemTime :: integer()) -> unicode:chardata().
format_time(SystemTime) ->
  calendar:system_time_to_rfc3339(SystemTime, [{unit, microsecond},
                                               {offset, "Z"}]).

-spec format_metadata(logger:metadata(), log_formatter:config()) ->
        unicode:chardata().
format_metadata(Metadata, Config) ->
  Pairs = maps:to_list(Metadata),
  PairStrings = lists:map(fun (Pair) ->
                              format_metadata_pair(Pair, Config)
                          end, Pairs),
  lists:join($\s, PairStrings).

-spec format_metadata_pair({atom(), term()}, log_formatter:config()) ->
        unicode:chardata().
format_metadata_pair({Name0, Value}, Config) ->
  {Name, _} = maybe_colorize(atom_to_binary(Name0), blue, Config),
  [Name, $=, format_metadata_value(Value)].

-spec format_metadata_value(term()) -> unicode:chardata().
format_metadata_value(Value) ->
  Data = log_formatter:format_metadata_value(Value),
  quote_string(iolist_to_binary(Data)).

-spec quote_string(binary()) -> unicode:chardata().
quote_string(String) ->
  json:serialize(String).

-spec maybe_colorize(unicode:chardata(), log_formatter_term:color(),
                     log_formatter:config()) ->
        {unicode:chardata(), LengthDiff :: non_neg_integer()}.
maybe_colorize(Data, Color, Config) ->
  Length1 = string:length(Data),
  case maps:get(color, Config, false) of
    true ->
      Data2 = log_formatter_term:colorize(Data, Color),
      Length2 = string:length(Data2),
      {Data2, Length2 - Length1};
    false ->
      {Data, 0}
  end.
