# Perform cell QC

Perform cell QC

## Usage

``` r
PerformSeuratCellQC(object, cell_qc)
```

## Arguments

- object:

  Seurat object

- cell_qc:

  Cell QC criteria. It is an expression string to pass to
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html)
  function to filter the cells. It can also be a list of expressions,
  where the names of the list are sample names. You can have a default
  expression in the list with the name "DEFAULT" for the samples that
  are not listed.

## Value

The Seurat object with cell QC results in `@misc$cell_qc_df`
