# Read a table, like read.delim, but with annotated factor levels

The factor levels are annotated like: `# factor-levels: group=A|B|C`
Column names are used literally, so `check.names` defaults to `FALSE`.

The factor levels are annotated like: `# factor-levels: group=A|B|C`

## Usage

``` r
read_table(file, factor_level_sep = "|", ...)

load_table(file, factor_level_sep = "|", ...)

write_table(
  x,
  file,
  factor_level_sep = "|",
  sep = "\t",
  row.names = FALSE,
  ...
)

save_table(
  x,
  file,
  factor_level_sep = "|",
  sep = "\t",
  row.names = FALSE,
  ...
)
```

## Arguments

- file:

  The file to read

- factor_level_sep:

  The separator for factor levels, default is "\|"

- ...:

  Additional arguments passed to write.table

- x:

  the object to be written, preferably a matrix or data frame. If not,
  it is attempted to coerce `x` to a data frame.

- sep:

  The field separator string, default is `\t` (different from
  write.table default)

- row.names:

  Whether to include row names, default is FALSE (different from
  write.table default)

## Value

A data frame with annotated factor levels
