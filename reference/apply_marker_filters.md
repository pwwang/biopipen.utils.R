# Filter a marker table by tissue/cancer/species

A filter that cannot be honored (the column is missing, or no rows match
the value) is an error: silently running the tool on unfiltered or empty
markers would produce wrong annotations.

## Usage

``` r
apply_marker_filters(df, tissue = NULL, cancer = NULL, species = NULL)
```

## Arguments

- df:

  A canonical marker table.

- tissue:

  The tissue to filter by, against the `tissue` column. If NULL, no
  filtering is done.

- cancer:

  The cancer to filter by, against the `cancer` column. If NULL, no
  filtering is done.

- species:

  The species to filter by, against the `species` column. If NULL, no
  filtering is done.

## Value

The filtered marker table.
