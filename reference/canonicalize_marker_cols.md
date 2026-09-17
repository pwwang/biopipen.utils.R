# Canonicalize the marker table column names

Rename the columns of a marker table to their canonical names, using the
aliases in `.MARKER_ALIASES` (matched case-insensitively).

## Usage

``` r
canonicalize_marker_cols(df)
```

## Arguments

- df:

  A marker table. Non-data.frames are returned as-is.

## Value

The marker table with canonical column names.
