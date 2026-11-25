# Run seurat t-SNE

In addition to
[`Seurat::RunTSNE()`](https://satijalab.org/seurat/reference/RunTSNE.html),
we provide an additional argument to use markers as features for t-SNE,
which makes the t-SNE more separated for different clusters.

## Usage

``` r
RunSeuratTSNE(object, RunTSNEArgs = list(), cache = NULL, log = NULL)
```

## Arguments

- object:

  Seurat object

- RunTSNEArgs:

  Arguments to pass to
  [`Seurat::RunTSNE()`](https://satijalab.org/seurat/reference/RunTSNE.html).
  `RunTSNEArgs$features` can be a character vector of features directly
  used for t-SNE, or a list with the following fields:

  - `order`: The order of the markers to use for t-SNE, e.g.
    "desc(abs(avg_log2FC))"

  - `n`: The number of total features to use for t-SNE, e.g. 30 If
    `RunTSNEArgs$features` is a list, it will run
    [`RunSeuratDEAnalysis()`](https://pwwang.github.io/biopipen.utils.R/reference/RunSeuratDEAnalysis.md)
    to get the markers for each group, and then select the top
    `n`/`ngroups` features for each group based on the `order` field. If
    `RunTSNEArgs$features` is a numeric value, it will be treated as the
    `n` field in the list above, with the default `order` being
    "desc(abs(avg_log2FC))".

- cache:

  Directory to cache the all markers, which can be reused later if DE
  analysis is desired.

- log:

  The logger to use. If NULL, a default logger will be used.

## Value

The Seurat object with t-SNE results

## Details

Scalar `RunTSNEArgs$dims = N` expands to `1:N`. When both
`RunTSNEArgs$features` and `RunTSNEArgs$dims` are provided,
`RunTSNEArgs$dims` will be ignored and set to `NULL`. The `features`
path always reads the default assay because
[`Seurat::RunTSNE()`](https://satijalab.org/seurat/reference/RunTSNE.html)
has no `assay` argument. If neither `RunTSNEArgs$features` nor
`RunTSNEArgs$dims` is provided, `RunTSNEArgs$dims` will be set to
`1:min(30, ceiling(ncells/3), ncol(object@reductions[[reduction]]))`,
where `ncells` is the number of cells in the object (or
`length(RunTSNEArgs$cells)` when `cells` is set), and `reduction` is
`RunTSNEArgs$reduction` (default: `object@misc$integrated_new_reduction`
or `"pca"`). When `RunTSNEArgs$perplexity` is not provided, it defaults
to `max(1, min(30, floor((ncells - 1) / 3)))` so small objects stay
within Rtsne's perplexity bound.

On the `dims` path with `tsne.method = "Rtsne"`, cells with identical
coordinates in the selected embedding dimensions are deduplicated before
the run: a warning reports how many cells are affected, t-SNE runs on
the unique coordinates only, and each duplicate is then mapped onto its
representative's coordinates, so the resulting reduction still covers
every cell. `RunTSNEArgs$cells` (default: all cells) is narrowed to the
representatives; the recorded command parameters then carry a `dedup`
entry with the counts. If duplicate rows are only detected once Rtsne
runs on the `features` path, its duplicate error is caught, a warning is
emitted, and the run is retried once with `check_duplicates = FALSE`
(duplicate cells may then overlap in the embedding). Duplicate cells
never raise an error, even when `RunTSNEArgs$check_duplicates = TRUE` is
supplied.

## Examples

``` r
# \donttest{
obj <- RunSeuratTSNE(SeuratObject::pbmc_small, list(dims = 10))
#> WARN    [2026-11-25 00:18:38]   5 of 80 cells have identical coordinates in reduction 'pca' (dims 1:10); running t-SNE on the 75 unique cells and mapping each duplicate onto its representative's coordinates
#> INFO    [2026-11-25 00:18:38]   reduction=pca, dims=1:10, tsne.method=Rtsne, perplexity=24, reduction.name=tsne
#> Warning: Number of cells changing from 80 to 75
#> Warning: Number of cells changing from 75 to 80
# }
```
