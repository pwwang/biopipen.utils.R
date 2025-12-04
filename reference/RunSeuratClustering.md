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

## Examples

``` r
# \donttest{
obj <- RunSeuratClustering(SeuratObject::pbmc_small)
#> INFO    [2025-12-04 00:15:52] Running RunPCA ...
#> Warning: You're computing too large a percentage of total singular values, use a standard svd instead.
#> Warning: Requested number is larger than the number of available items (20). Setting to 20.
#> Warning: Requested number is larger than the number of available items (20). Setting to 20.
#> Warning: Requested number is larger than the number of available items (20). Setting to 20.
#> Warning: Requested number is larger than the number of available items (20). Setting to 20.
#> Warning: Requested number is larger than the number of available items (20). Setting to 20.
#> PC_ 1 
#> Positive:  SDPR, PF4, PPBP, TUBB1, CA2, TREML1, MYL9, PGRMC1, RUFY1, PARVB 
#> Negative:  HLA-DPB1, HLA-DQA1, S100A9, S100A8, GNLY, RP11-290F20.3, CD1C, AKR1C3, IGLL5, VDAC3 
#> PC_ 2 
#> Positive:  HLA-DPB1, HLA-DQA1, S100A8, S100A9, CD1C, RP11-290F20.3, PARVB, IGLL5, MYL9, SDPR 
#> Negative:  GNLY, AKR1C3, VDAC3, PGRMC1, TUBB1, PF4, TREML1, RUFY1, CA2, PPBP 
#> PC_ 3 
#> Positive:  S100A9, S100A8, RP11-290F20.3, AKR1C3, PARVB, GNLY, PPBP, PGRMC1, MYL9, TUBB1 
#> Negative:  HLA-DQA1, CD1C, IGLL5, HLA-DPB1, RUFY1, PF4, VDAC3, SDPR, TREML1, CA2 
#> PC_ 4 
#> Positive:  IGLL5, RP11-290F20.3, VDAC3, PPBP, TUBB1, TREML1, PF4, CA2, PARVB, MYL9 
#> Negative:  CD1C, AKR1C3, S100A8, GNLY, HLA-DPB1, HLA-DQA1, S100A9, PGRMC1, RUFY1, SDPR 
#> PC_ 5 
#> Positive:  MYL9, PARVB, IGLL5, TREML1, AKR1C3, PGRMC1, HLA-DPB1, S100A9, TUBB1, PF4 
#> Negative:  VDAC3, RP11-290F20.3, RUFY1, CD1C, HLA-DQA1, CA2, S100A8, PPBP, GNLY, SDPR 
#> INFO    [2025-12-04 00:15:53] Running FindNeighbors ...
#> Computing nearest neighbor graph
#> Computing SNN
#> INFO    [2025-12-04 00:15:55] Running FindClusters ...
#> INFO    [2025-12-04 00:15:55]   Using resolution(s): 0.8
#> Modularity Optimizer version 1.3.0 by Ludo Waltman and Nees Jan van Eck
#> 
#> Number of nodes: 80
#> Number of edges: 2352
#> 
#> Running Louvain algorithm...
#> Maximum modularity in 10 random starts: 0.4014
#> Number of communities: 2
#> Elapsed time: 0 seconds
#> INFO    [2025-12-04 00:15:56]   Found clusters (with resolution 0.8):
#> INFO    [2025-12-04 00:15:56]    | c1(44), c2(36)
#> INFO    [2025-12-04 00:15:56] Running RunUMAP ...
#> Warning: The default method for RunUMAP has changed from calling Python UMAP via reticulate to the R-native UWOT using the cosine metric
#> To use Python UMAP via reticulate, set umap.method to 'umap-learn' and metric to 'correlation'
#> This message will be shown once per session
#> 00:15:56 UMAP embedding parameters a = 0.9922 b = 1.112
#> 00:15:56 Read 80 rows and found 19 numeric columns
#> 00:15:56 Using Annoy for neighbor search, n_neighbors = 30
#> 00:15:56 Building Annoy index with metric = cosine, n_trees = 50
#> 0%   10   20   30   40   50   60   70   80   90   100%
#> [----|----|----|----|----|----|----|----|----|----|
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> *
#> |
#> 00:15:56 Writing NN index file to temp file /tmp/RtmpTgKohH/file2523442c5981
#> 00:15:56 Searching Annoy index using 1 thread, search_k = 3000
#> 00:15:56 Annoy recall = 100%
#> 00:15:56 Commencing smooth kNN distance calibration using 1 thread
#>  with target n_neighbors = 30
#> 00:15:56 7 smooth knn distance failures
#> 00:15:58 Initializing from normalized Laplacian + noise (using RSpectra)
#> 00:15:58 Commencing optimization for 500 epochs, with 2664 positive edges
#> 00:15:58 Using rng type: pcg
#> 00:15:59 Optimization finished
GetIdentityColumn(obj)
#> [1] "seurat_clusters"
table(obj$seurat_clusters)
#> 
#> c1 c2 
#> 44 36 
# }
```
