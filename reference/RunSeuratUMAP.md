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
    `n`/`ngroups` features for each group based on the `order` field. If
    `RunUMAPArgs$features` is a numeric value, it will be treated as the
    `n` field in the list above, with the default `order` being
    "desc(abs(avg_log2FC))". `RunUMAPArgs$from` can be a path to a TSV
    file containing pre-computed UMAP coordinates to overwrite the
    results of
    [`Seurat::RunUMAP()`](https://satijalab.org/seurat/reference/RunUMAP.html).
    The path should be in the format
    `"file:///path/to/file#id_col,coord_col1,coord_col2"`, where
    `id_col` is the column with cell barcodes and the remaining columns
    are the UMAP coordinate columns (e.g., `UMAP_1,UMAP_2`). The cell
    barcodes must match those in the Seurat object, and the coordinate
    column names must match the reduction's `cell.embeddings` column
    names. You can also use 1-based column indices instead of names,
    e.g., `"file:///path/to/file#1,2,3"`.

- cache:

  Directory to cache the all markers, which can be reused later if DE
  analysis is desired.

- log:

  The logger to use. If NULL, a default logger will be used.

## Value

The Seurat object with UMAP results

## Details

When both `RunUMAPArgs$features` and `RunUMAPArgs$dims` are provided,
`RunUMAPArgs$dims` will be ignored. If neither `RunUMAPArgs$features`
nor `RunUMAPArgs$dims` is provided, `RunUMAPArgs$dims` will be set to
`1:min(30, ceiling(ncells/3), ncol(object@reductions[[reduction]]))`,
where `ncells` is the number of cells in the object, and `reduction` is
`RunUMAPArgs$reduction` (default: "pca").
