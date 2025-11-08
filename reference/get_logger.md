# Setup and return the logger

Setup and return the logger

## Usage

``` r
get_logger(
  format =
    "{sprintf(\"%-7s\", level)} [{format(time, \"%Y-%m-%d %H:%M:%S\")}] {msg}",
  appender = "stdout",
  level = "INFO"
)
```

## Arguments

- format:

  The format of the log message. This format will be passed to
  [logger::layout_glue_generator](https://daroczig.github.io/logger/reference/layout_glue_generator.html).
  The default format will generate a log message like:
  `INFO [2021-01-01 00:00:00] Hello world`

- appender:

  The appender to use. Default is "stdout" Either a string or a logger
  appender object. If string is provided, it will be converted to the
  corresponding logger appender object. For example, "stdout" will be
  converted to
  [logger::appender_stdout](https://daroczig.github.io/logger/reference/appender_stdout.html)

- level:

  The log level. Default is "INFO" Either a string or a logger level
  object. If string is provided, it will be converted to the
  corresponding logger level object. For example, "INFO" will be
  converted to
  [logger::INFO](https://daroczig.github.io/logger/reference/log_levels.html)

## Value

A list of logging functions
