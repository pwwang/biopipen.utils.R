# Calculate combined PC scores of gene sets for scPS scoring

Runs PCA on the expression of each gene set and combines the components
weighted by their explained variance. Ported from the scPS script
(https://github.com/Thakar-Lab/scPS).

## Usage

``` r
GS_PCA_Calculation(Seurat_data, GeneSet)
```

## Arguments

- Seurat_data:

  A Seurat object (Seurat V5) with a scaled `RNA` assay (`scale.data`
  layer) or an `SCT` assay

- GeneSet:

  A `GeneSet` or a `GeneSetCollection`

## Value

A matrix of combined PC scores, gene sets x cells
