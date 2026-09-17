# Check whether a marker table has the canonical columns

A canonical marker table has both the `cell_type` and `gene` columns.

## Usage

``` r
is_marker_canonical(df)
```

## Arguments

- df:

  A marker table.

## Value

`TRUE` if `df` is a data.frame with `cell_type` and `gene` columns,
`FALSE` otherwise.
