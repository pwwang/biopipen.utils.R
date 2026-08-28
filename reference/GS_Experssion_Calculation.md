# Calculate the mean expression level of gene sets

Ported from the scPS script (https://github.com/Thakar-Lab/scPS).

## Usage

``` r
GS_Experssion_Calculation(Seurat_data, GeneSet)
```

## Arguments

- Seurat_data:

  A Seurat object (Seurat V5) with a normalized `SCT` or `RNA` assay

- GeneSet:

  A `GeneSet` or a `GeneSetCollection`

## Value

A matrix of mean expression levels, gene sets x cells
