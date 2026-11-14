# Normalize nullable string columns in an h5ad file

anndata \>= 0.13 writes every string column of `obs`, `var` and
`raw/var`, including the index column, as a group with `values` and
`mask` datasets (nullable string encoding) instead of a plain string
dataset. SeuratDisk expects plain datasets, so rewrite those groups on a
temporary copy of the file. `mask == TRUE` means the value is missing.

## Usage

``` r
.normalize_h5ad_nullable_strings(infile, log)
```

## Arguments

- infile:

  Input h5ad file

- log:

  Logger

## Value

`infile` if it has no nullable string column, otherwise the path to a
temporary copy with those columns rewritten

## Details

`hdf5r` cannot store `NA_character_` faithfully (it writes the literal
"NA"), so missing values are written as `""` instead.
