# Run seurat UMAP

In additional to
[`Seurat::RunUMAP()`](https://satijalab.org/seurat/reference/RunUMAP.html),
we provide an additional arguments to use markers as features for UMAP,
which makes the UMAP more separated for different clusters.

## Usage

``` r
RunSeuratUMAP(object, RunUMAPArgs = list(), cache = NULL, log = NULL)
```

## Arguments

- object:

  Seurat object

- RunUMAPArgs:

  Arguments to pass to
  [`Seurat::RunUMAP()`](https://satijalab.org/seurat/reference/RunUMAP.html).
  `RunUMAPArgs$features` can be a character vector of features directly
  used for UMAP, or a list with the following fields:

  - `order`: The order of the markers to use for UMAP, e.g.
    "desc(abs(avg_log2FC))"

  - `n`: The number of total features to use for UMAP, e.g. 30 If
    `RunUMAPArgs$features` is a list, it will run
    [`RunSeuratDEAnalysis()`](https://pwwang.github.io/biopipen.utils.R/reference/RunSeuratDEAnalysis.md)
    to get the markers for each group, and then select the top
    `n`/`ngroups` features for each group based on the `order` field.

- cache:

  Directory to cache the all markers, which can be reused later if DE
  analysis is desired.

- log:

  The logger to use. If NULL, a default logger will be used.

## Value

The Seurat object with UMAP results
