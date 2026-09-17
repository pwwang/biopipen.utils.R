# Convert a marker table to the scCATCH format

Only positive markers are kept (negative ones cannot be represented).
The `cell_type` column is renamed to `celltype` and the
`pmid`/`subtype1`/`subtype2`/`subtype3` columns are added when missing.

## Usage

``` r
markers_to_sccatch_df(df, tissue = NULL, cancer = NULL, species = NULL)
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

A data.frame in the scCATCH format.
