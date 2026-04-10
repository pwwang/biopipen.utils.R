# Run Seurat MapQuery to reference

This will find transfer anchors between query and reference, and map the
query to the reference. For SCT-normalized references,
[`Seurat::FindTransferAnchors()`](https://satijalab.org/seurat/reference/FindTransferAnchors.html)
handles query normalization internally by recomputing residuals using
the reference's SCT model (`recompute.residuals = TRUE` by default), so
a manual call to
[`Seurat::SCTransform()`](https://satijalab.org/seurat/reference/SCTransform.html)
on the query is NOT needed. For LogNormalize references, the query is
normalized with
[`Seurat::NormalizeData()`](https://satijalab.org/seurat/reference/NormalizeData.html)
prior to anchor finding.

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

  For LogNormalize, skip
  [`Seurat::NormalizeData()`](https://satijalab.org/seurat/reference/NormalizeData.html)
  if the query's default assay is already 'RNA' (i.e., already
  log-normalized). For SCT with
  `FindTransferAnchorsArgs$recompute.residuals = FALSE`, skip
  [`Seurat::SCTransform()`](https://satijalab.org/seurat/reference/SCTransform.html)
  if the default assay is already 'SCT'. This parameter has no effect in
  the default SCT case (`recompute.residuals = TRUE`), where
  [`Seurat::FindTransferAnchors()`](https://satijalab.org/seurat/reference/FindTransferAnchors.html)
  handles normalization internally using the reference model.

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
  Only used when `refnorm` is set to "SCT" AND
  `FindTransferAnchorsArgs$recompute.residuals` is explicitly set to
  `FALSE`. In the default case (`recompute.residuals = TRUE`),
  [`Seurat::FindTransferAnchors()`](https://satijalab.org/seurat/reference/FindTransferAnchors.html)
  recomputes query residuals from raw counts using the reference SCT
  model, making manual
  [`Seurat::SCTransform()`](https://satijalab.org/seurat/reference/SCTransform.html)
  on the query unnecessary.

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

https://satijalab.org/seurat/articles/multimodal_reference_mapping.html

https://satijalab.org/seurat/articles/covid_sctmapping
