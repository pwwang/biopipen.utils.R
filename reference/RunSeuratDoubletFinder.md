# Run DoubletFinder on a Seurat object

Run DoubletFinder on a Seurat object

## Usage

``` r
RunSeuratDoubletFinder(
  object,
  ident = NULL,
  ncores = 1,
  PCs = 30,
  pN = 0.25,
  doublets = 0.075,
  log = NULL,
  allow_warnings = FALSE
)
```

## Arguments

- object:

  Seurat object

- ident:

  Cluster identity to use for homotypic doublet proportion estimation.
  If NULL or not exists, will run clustering first to get cluster
  identities.

- ncores:

  Number of cores to use

- PCs:

  Number of statistically-significant principal components

- pN:

  The number of generated artificial doublets, expressed as a proportion
  of the merged real-artificial data. Default is set to 0.25, based on
  observation that DoubletFinder performance is largely pN-invariant
  (see McGinnis, Murrow and Gartner 2019, Cell Systems).

- doublets:

  The expected proportion of doublets in the dataset. Default is set to
  0.075

## Value

The Seurat object with doublet detection results in
`@misc$doubletFinder`
