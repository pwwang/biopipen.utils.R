# Run doublet detection on a Seurat object

Run doublet detection on a Seurat object

## Usage

``` r
RunSeuratDoubletDetection(
  object,
  tool = "DoubletFinder",
  DoubletFinderArgs = list(),
  scDblFinderArgs = list(),
  filter = TRUE,
  log = NULL,
  cache = NULL
)
```

## Arguments

- object:

  Seurat object

- tool:

  Doublet detection tool. Either "DoubletFinder" or "scDblFinder"

- DoubletFinderArgs:

  Arguments to pass to
  [RunSeuratDoubletFinder](https://pwwang.github.io/biopipen.utils.R/reference/RunSeuratDoubletFinder.md)

- scDblFinderArgs:

  Arguments to pass to
  [RunSeuratScDblFinder](https://pwwang.github.io/biopipen.utils.R/reference/RunSeuratScDblFinder.md)

- filter:

  Whether to filter out doublets. Default is TRUE. If you want to
  visualize the singlets and doublets, set it to FALSE so that the
  doublets are not filtered out. Then you will need to filter them out
  manually. For example:
  `object <- subset(object, subset = DoubletFinder_DropletType != "doublet")`

- log:

  Logger

- cache:

  Directory to cache the results. Set to `FALSE` to disable caching

## Value

The Seurat object with doublet detection results in `@misc$doublets`
