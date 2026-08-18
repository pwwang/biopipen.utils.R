# Ensure marker genes are in the scale.data layer of an assay of the Seurat object

Ensure marker genes are in the scale.data layer of an assay of the
Seurat object

## Usage

``` r
EnsureSeuratScaleData(object, features, assay = NULL, umi_assay = "RNA")
```

## Arguments

- object:

  Seurat object

- features:

  Character vector, or a list of character vectors, of feature names to
  ensure in the scale.data layer

- assay:

  Assay to use. If NULL, the default assay will be used.

- umi_assay:

  Assay to use for the UMI counts. Default is "RNA". This is used to get
  the counts for scaling.

## Value

The Seurat object with the features ensured in the scale.data layer
