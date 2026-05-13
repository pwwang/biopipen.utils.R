# Finish the QC process including the visualization

This will remove the cells and genes that are not passing the QC, and
also remove the intermediate data used for QC visualization.

## Usage

``` r
FinishSeuratQC(object, keep_contam_assay = FALSE)
```

## Arguments

- object:

  Seurat object

- keep_contam_assay:

  A logic value to control where to keep the "Contaminated" assay

## Value

The Seurat object with the QC process finished
