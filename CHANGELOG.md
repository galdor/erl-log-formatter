% erl-log-formatter changelog

# Next Version
## Bugs
- Fix formatting of non-JSON metadata in JSON mode.
## Misc
- Format the domain in text mode.

# 1.2.1
## Bugs
- Fix text formatting for binary strings in metadata.

# 1.2.0
## Features
- For JSON logs, format the domain as a single string where domain parts are
  separated by full stop characters.
- Align metadata in text logs for better readability.
## Misc
- Switch from jsx to [erl-json](https://github.com/galdor/erl-json).

# 1.1.0
## Bugs
- Fix the name of the `jsx` dependency.

# 1.0.0
First public version.
