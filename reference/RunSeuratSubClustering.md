# Run subset clustering on a Seurat object

It's unlike
[`Seurat::FindSubCluster`](https://satijalab.org/seurat/reference/FindSubCluster.html),
which only finds subclusters of a single cluster. Instead, it will
perform the whole clustering procedure on the subset of cells. One can
use metadata to specify the subset of cells to perform clustering on.

## Usage

``` r
RunSeuratSubClustering(
  object,
  subset,
  name = "subcluster",
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

- subset:

  A string of expression to pass to
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html)
  function to filter the cells.

- name:

  Name of the run. It will be used as the prefix for the reduction name,
  keys and cluster names. For reduction keys, it will be
  `toupper(<name>)` + "PC\_" and `toupper(<name>)` + "UMAP\_". For
  cluster names, it will be `<name>` + "." + resolution. And the final
  cluster name will be `<name>`. Default is "subcluster".

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

The original Seurat object (not the subsetted one) with the subclusters
results in `@meta.data` and `@reductions`.

## Examples

``` r
# \donttest{
obj <- SeuratObject::pbmc_small
# Just run UMAP to compare with the subclusters
obj <- suppressMessages(Seurat::RunUMAP(obj, dims = 1:10))
obj <- suppressWarnings(suppressMessages(RunSeuratSubClustering(
   obj, subset = "groups == 'g1'", name = "g1subcluster"
)))
#> INFO    [2025-12-04 16:18:24] Subsetting seurat object ...
#> INFO    [2025-12-04 16:18:24] - Running RunPCA ...
#> INFO    [2025-12-04 16:18:24] - Running FindNeighbors ...
#> INFO    [2025-12-04 16:18:25] - Running FindClusters ...
#> INFO    [2025-12-04 16:18:25]   Using resolution(s): 0.8
#> Modularity Optimizer version 1.3.0 by Ludo Waltman and Nees Jan van Eck
#> 
#> Number of nodes: 44
#> Number of edges: 946
#> 
#> Running Louvain algorithm...
#> Maximum modularity in 10 random starts: 0.2121
#> Number of communities: 2
#> Elapsed time: 0 seconds
#> INFO    [2025-12-04 16:18:26]   Found subclusters (with resolution 0.8):
#> INFO    [2025-12-04 16:18:26]    | s1(25), s2(19)
#> INFO    [2025-12-04 16:18:26] - Running RunUMAP ...

scplotter::CellDimPlot(
    obj, reduction = "umap", group_by = "groups",
    title = "UMAP of all cells",
    subtitle = "groups = g1 will be subclustered"
)

scplotter::CellDimPlot(
    obj, reduction = "umap", group_by = "g1subcluster",
    title = "Subclusters on the original UMAP"
)

scplotter::CellDimPlot(obj,
    reduction = "g1subcluster.umap", group_by = "g1subcluster"
)

# }
```
