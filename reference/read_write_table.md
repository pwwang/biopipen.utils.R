# Read a table, like read.table, but with annotated factor levels

The factor levels are annotated like: `# factor-levels: group=A|B|C`
Column names are used literally, so `check.names` defaults to `FALSE`.

The factor levels are annotated like: `# factor-levels: group=A|B|C`

## Usage

``` r
read_table(file, factor_level_sep = "|", ...)

load_table(file, factor_level_sep = "|", ...)

write_table(x, file, factor_level_sep = "|", ...)

save_table(x, file, factor_level_sep = "|", ...)
```

## Arguments

- file:

  The file to write

- factor_level_sep:

  The separator for factor levels, default is "\|"

- ...:

  Additional arguments passed to write.table

- x:

  The data frame to write

## Value

A data frame with annotated factor levels
