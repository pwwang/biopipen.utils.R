# Run seurat unsupervised clustering

Run seurat unsupervised clustering

## Usage

``` r
RunSeuratClustering(
  object,
  RunPCAArgs = list(),
  RunUMAPArgs = list(),
  FindNeighborsArgs = list(),
  FindClustersArgs = list(),
  log = NULL,
  cache = NULL
)
```

## Arguments

- object:

  Seurat object

- RunPCAArgs:

  Arguments to pass to
  [`Seurat::RunPCA()`](https://satijalab.org/seurat/reference/RunPCA.html)

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

- FindNeighborsArgs:

  Arguments to pass to
  [`Seurat::FindNeighbors()`](https://satijalab.org/seurat/reference/FindNeighbors.html)

- FindClustersArgs:

  Arguments to pass to
  [`Seurat::FindClusters()`](https://satijalab.org/seurat/reference/FindClusters.html)

- log:

  Logger

- cache:

  Directory to cache the results. Set to `FALSE` to disable caching

## Value

The Seurat object with clustering results
