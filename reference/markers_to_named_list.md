# Convert a marker table to a named list

The names are the cell types and the values are the genes. Only positive
markers are kept (negative ones cannot be represented).

## Usage

``` r
markers_to_named_list(df, tissue = NULL, cancer = NULL, species = NULL)
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

A named list of genes, named by the cell types.
