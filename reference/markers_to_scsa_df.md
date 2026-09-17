# Convert a marker table to the python wrappers' marker file format

A headerless two-column table of the cell types and their genes, as
taken by the `-m/--marker` of the python wrappers (`scsa`, `maca`,
`scmapnet`). Only positive markers are kept (the format has no
direction).

## Usage

``` r
markers_to_scsa_df(df)
```

## Arguments

- df:

  A canonical marker table.

## Value

A data.frame with `cellName` and `gene` columns.
