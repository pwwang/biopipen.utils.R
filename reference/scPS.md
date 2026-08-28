# Calculate scPS scores of gene sets for single cells

scPS (single-cell Pathway Score) scores each cell for each gene set as
the product of the combined PC score (from PCA on the scaled expression
of the gene set) and the mean expression level of the gene set in the
cell. Ported from the scPS script (https://github.com/Thakar-Lab/scPS)
(see references).

## Usage

``` r
scPS(Seurat_data, GeneSet)
```

## Arguments

- Seurat_data:

  A Seurat object (Seurat V5) with a scaled `RNA` assay (`scale.data`
  layer) or an `SCT` assay, and a normalized `data` layer

- GeneSet:

  A `GeneSet` or a `GeneSetCollection`, or a path to a GMT file of gene
  sets

## Value

A matrix of scPS scores, gene sets x cells
