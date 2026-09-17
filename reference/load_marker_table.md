# Load a marker table

Read a marker file and return either a canonical marker table or the raw
object (a named list / matrix for RDS files, or the path string for xlsx
files).

## Usage

``` r
load_marker_table(path, cols = NULL)
```

## Arguments

- path:

  The path of the marker file. A `file://` prefix is stripped, and a
  `#col1,col2,...` suffix selects columns (by name, alias or 1-based
  index).

- cols:

  The columns to select. If NULL, they are taken from the `#` suffix of
  `path` when there is one.

## Value

A canonical marker data.frame, or the raw object for native-format
files.
