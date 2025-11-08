# Run Seurat MapQuery to reference

This will normalize the query object, find transfer anchors, and map the
query to the reference.

## Usage

``` r
RunSeuratMap2Ref(
  object,
  ref,
  use,
  ident = "seurat_clusters",
  refnorm = c("auto", "LogNormalize", "SCT", "SCTransform"),
  skip_if_normalized = TRUE,
  split_by = NULL,
  ncores = 1,
  MapQueryArgs = list(refdata = list()),
  FindTransferAnchorsArgs = list(),
  SCTransformArgs = list(),
  NormalizeDataArgs = list(),
  log = NULL,
  cache = NULL
)
```

## Arguments

- object:

  Seurat object

- ref:

  Reference Seurat object or a file path to a Seurat object with .rds or
  .h5seurat extension

- use:

  The name in the metadata of the reference to use as identity for the
  query after mapping

- ident:

  The name of the identity in metadata after mapping

- refnorm:

  The normalization method used by the reference. The same normalization
  method will be used for the query.

  - "auto": automatically detect the normalization method used by the
    reference

  - "LogNormalize": LogNormalize

  - "SCT": SCTransform

  - "SCTransform": SCTransform (alias for SCT)

- skip_if_normalized:

  Skip normalization if the query is already normalized with the same
  method as the reference

- split_by:

  The name of the metadata to split the query object by and do the
  mapping separately

- ncores:

  Number of cores to use for parallel processing for the split query
  objects

- MapQueryArgs:

  Arguments to pass to
  [`Seurat::MapQuery()`](https://satijalab.org/seurat/reference/MapQuery.html).
  The `use` argument will be added to the `refdata` list.

- FindTransferAnchorsArgs:

  Arguments to pass to
  [`Seurat::FindTransferAnchors()`](https://satijalab.org/seurat/reference/FindTransferAnchors.html).

- SCTransformArgs:

  Arguments to pass to
  [`Seurat::SCTransform()`](https://satijalab.org/seurat/reference/SCTransform.html).
  Will be used to normalize the query object if `refnorm` is set to
  "SCT"

- NormalizeDataArgs:

  Arguments to pass to
  [`Seurat::NormalizeData()`](https://satijalab.org/seurat/reference/NormalizeData.html).
  Will be used to normalize the query object if `refnorm` is set to
  "LogNormalize"

- log:

  Logger

- cache:

  Directory to cache the results. Set to `FALSE` to disable caching

## Value

The Seurat object with the mapped data

## See also

https://satijalab.org/seurat/articles/integration_mapping.html
