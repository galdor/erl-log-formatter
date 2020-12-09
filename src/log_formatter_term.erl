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

-module(log_formatter_term).

-export([colorize/2, colorize/3]).

-export_type([color/0]).

%% There is no point in supporting terminals that do not support basic ANSI
%% escape sequences, so we do not even try.

-type color() :: black | red | green | yellow | blue | magenta | cyan | white
               | default.

-spec colorize(unicode:chardata(), color()) -> unicode:chardata().
colorize(Data, ForegroundColor) ->
  [foreground_color_sequence(ForegroundColor),
   Data,
   reset_sequence()].

-spec colorize(unicode:chardata(), color(), color()) -> unicode:chardata().
colorize(Data, ForegroundColor, BackgroundColor) ->
  [foreground_color_sequence(ForegroundColor),
   background_color_sequence(BackgroundColor),
   Data,
   reset_sequence()].

-spec reset_sequence() -> string().
reset_sequence() ->
  sequence("0m").

-spec foreground_color_sequence(color()) -> string().
foreground_color_sequence(Color) ->
  sequence(io_lib:format("0;~bm", [30 + color_code(Color)])).

-spec background_color_sequence(color()) -> string().
background_color_sequence(Color) ->
  sequence(io_lib:format("0;~bm", [30 + color_code(Color)])).

-spec color_code(color()) -> 0..9.
color_code(black) -> 0;
color_code(red) -> 1;
color_code(green) -> 2;
color_code(yellow) -> 3;
color_code(blue) -> 4;
color_code(magenta) -> 5;
color_code(cyan) -> 6;
color_code(white) -> 7;
color_code(default) -> 9.

-spec sequence(string()) -> string().
sequence(S) ->
  "\e[" ++ S.
