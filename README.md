# Project
This repository contains a customizable formatter for the
[logger](https://erlang.org/doc/man/logger.html) OTP application.

It currently supports a human-readable text output and a JSON output.

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
