# Convert a marker table to the ScType format

One row per cell type, with the positive and negative markers collapsed
into comma-separated `geneSymbolmore1`/`geneSymbolmore2` columns.

## Usage

``` r
markers_to_sctype_df(df, tissue = NULL, cancer = NULL, species = NULL)
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

A data.frame in the ScType format.
