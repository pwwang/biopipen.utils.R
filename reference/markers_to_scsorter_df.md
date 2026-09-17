# Convert a marker table to the scSorter format

Negative-direction markers become negative weights.

## Usage

``` r
markers_to_scsorter_df(df, tissue = NULL, cancer = NULL, species = NULL)
```

## Arguments

- df:

  A canonical marker table.

- tissue:

  The tissue to filter by. If NULL, no filtering is done.

- cancer:

  The cancer to filter by. If NULL, no filtering is done.

- species:

  The species to filter by. If NULL, no filtering is done.

## Value

A data.frame with `Type`, `Marker` and (when the marker table has a
`direction` or `weight` column) `Weight` columns.
