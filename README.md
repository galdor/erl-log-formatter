# Project
This repository contains a customizable formatter for the
[logger](https://erlang.org/doc/man/logger.html) OTP application.

It currently supports a human-readable text output and a JSON output.

# Features
## Metadata
The following metadata entry are handled separately:
- `domain`: see the [official
  documentation](https://erlang.org/doc/man/logger_filters.html#domain-2) for
  more information.
- `event`: a list of atoms identifying what is being logged.

## Text output
In text mode, the formatter returns one or more lines of text representing the
log message. Messages are prefixed with the log level, domain string (if
present) and event string (if present); they are followed by the set of
metadata formatted as a sequence of "key=value" strings.

## JSON output
In JSON mode, the formatter returns one JSON object serialized as a single line
of text for each log message.

The following members are used:
- `level`: the log level string (e.g. `"error"`);
- `domain`: the
  [domain](https://erlang.org/doc/man/logger_filters.html#domain-2) of the log
  message formatted as a string (e.g. `"otp.sasl"`). Optional.
- `event`: if the message had an `event` metadata entry, a string representing
  the event. For example, if event was `[http, request_received]`, the field
  will be formatted as `http.request_received`. Optional.
- `time`: the timestamp of the message as a microsecond UNIX timestamp.
- `message`: the message string.
- `data`: an object containing metadata associated with the message.

# Usage
Make sure to add the application as dependency in the Rebar3 configuration
file. After that, the formatter is configured as any other logger formatter,
in the system configuration file.

Example:
```erlang
[{kernel,
  [{logger,
    [{handler, default, logger_std_h,
      #{level => debug,
        filter_default => log,
        formatter => {log_formatter, #{format => text}}}}]},
   {logger_level, debug}]}].
```

See the [logger
documentation](https://erlang.org/doc/apps/kernel/logger_chapter.html) for
more information.

## Configuration
The following options are available in the formatter configuration map:
- `debug`: print log events on the standard output; used when developing the
  formatter itself.
- `format`: the output format, either `text` or `json`.
- `include_time`: include the time in each formatted message (`text` format
  only).

# Contact
If you find a bug or have any question, feel free to open a GitHub issue or to
contact me [by email](mailto:khaelin@gmail.com).

Please note that I do not currently review or accept any contribution.
