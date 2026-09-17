# Convert a marker table to the UCell signature format

A named list of gene vectors, one per cell type, in the UCell signature
format.

## Usage

``` r
markers_to_ucell_list(df)
```

## Arguments

- df:

  A canonical marker table.

## Value

A named list of gene vectors, named by the cell types.

## Details

UCell reads the marker direction from the gene name: a gene whose name
ends in `-` is a negative marker (it contributes `w_neg` times its
score, subtracted), while a trailing `+` is stripped. Negative-direction
rows therefore get a `-` appended to the gene symbol and positive rows
pass through unchanged. UCell has no per-gene weights, so a `weight`
column is deliberately ignored.
