# Perform cell and gene QC on a Seurat object

This is a convenience wrapper of
[`PerformSeuratCellQC()`](https://pwwang.github.io/biopipen.utils.R/reference/PerformSeuratCellQC.md)
and
[`PerformGeneQC()`](https://pwwang.github.io/biopipen.utils.R/reference/PerformGeneQC.md)
for a single sample, as done by
[`LoadSeuratAndPerformQC()`](https://pwwang.github.io/biopipen.utils.R/reference/LoadSeuratAndPerformQC.md)
per sample before the samples are merged.

## Usage

``` r
PerformSeuratQC(object, cell_qc = NULL, gene_qc = NULL)
```

## Arguments

- object:

  Seurat object of a single sample

- cell_qc:

  Cell QC criteria. It is an expression string to pass to
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html)
  function to filter the cells. It can also be a list of expressions,
  where the names of the list are sample names. You can have a default
  expression in the list with the name "DEFAULT" for the samples that
  are not listed.

- gene_qc:

  Gene QC criteria A list containing the following fields:

  - min_cells: Minimum number of cells a gene should be expressed in to
    be kept

  - excludes: A string or strings to exclude certain genes. Regular
    expressions are supported. Multiple strings can also be separated by
    commas in a single string.

## Value

The Seurat object with the `.QC` column in `meta.data` and, if `gene_qc`
is given, the gene QC results in `@misc$gene_qc`

## Examples

``` r
# \donttest{
obj <- PerformSeuratQC(
    SeuratObject::pbmc_small,
    cell_qc = "nFeature_RNA > 40",
    gene_qc = list(min_cells = 3)
)
table(obj$.QC)
#> 
#> FALSE  TRUE 
#>    15    65 
head(obj@misc$gene_qc)
#>                 Sample  Feature Count   QC
#> MS4A1    SeuratProject    MS4A1    12 TRUE
#> CD79B    SeuratProject    CD79B    23 TRUE
#> CD79A    SeuratProject    CD79A    13 TRUE
#> HLA-DRA  SeuratProject  HLA-DRA    55 TRUE
#> TCL1A    SeuratProject    TCL1A     8 TRUE
#> HLA-DQB1 SeuratProject HLA-DQB1    30 TRUE
# }
```
