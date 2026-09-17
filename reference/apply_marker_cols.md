# Canonicalize the marker table columns and select a subset of them

Canonicalize the marker table columns and select a subset of them

## Usage

``` r
apply_marker_cols(df, cols, path)
```

## Arguments

- df:

  A marker table.

- cols:

  The columns to select, by name (exact, case-insensitive or alias) or
  by 1-based index. If NULL, all columns are kept.

- path:

  The path of the marker file, used in error messages.

## Value

The marker table with canonical column names and the selected columns.
