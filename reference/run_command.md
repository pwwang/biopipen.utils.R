# Run a command

Run a command

## Usage

``` r
run_command(
  cmd,
  fg = FALSE,
  wait = TRUE,
  print_command = cat,
  stdout = "",
  stderr = "",
  ...
)
```

## Arguments

- cmd:

  A command to run

- fg:

  Whether to run the command in the foreground

- wait:

  Whether to wait for the command to finish

- print_command:

  The handler to print the command

- stdout:

  See [base::system2](https://rdrr.io/r/base/system2.html)

- stderr:

  See [base::system2](https://rdrr.io/r/base/system2.html)

- ...:

  Additional arguments to pass to `system2`

## Value

The output of the command If `stdout` or `stderr` is `TRUE`, the output
is returned as a character vector Otherwise, the output is returned as
an integer exit code
