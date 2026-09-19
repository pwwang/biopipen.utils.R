# Run a cell type annotation tool

A unified interface over the cell type annotation tools used by
biopipen's `CellTypeAnnotation` process. The tool is picked by `tool`
and its tool-specific parameters are passed in `args`; see
[`celltype_annotation_tools()`](https://pwwang.github.io/biopipen.utils.R/reference/celltype_annotation_tools.md)
for the supported tools and the level each of them annotates at.

## Usage

``` r
RunCellTypeAnnotation(
  object,
  tool,
  args = list(),
  ident = NULL,
  cache = NULL,
  log = NULL
)
```

## Arguments

- object:

  Seurat object, or a path to an RDS/qs/qs2/h5ad/h5seurat file to read
  the object from.

- tool:

  One of the supported tool names, see
  [`celltype_annotation_tools()`](https://pwwang.github.io/biopipen.utils.R/reference/celltype_annotation_tools.md).

- args:

  Named list of tool arguments, e.g. `cell_types` for the `direct` tool.

- ident:

  Metadata column with the clusters, required by cluster-level tools,
  optional for cell-level ones.

- cache:

  Directory for conversions and tool scratch files, default
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- log:

  Logger.

## Value

A list with:

- `mapping`: a named list mapping each cluster to its cell type, or for
  `type = "cell"` a data.frame of per-cell labels (cell IDs as row
  names, the annotation in the first column).

- `type`: `"cluster"` when the result is per cluster, `"cell"` when it
  is per cell.

- `cells`: the per-cell data.frame when `type = "cluster"` but the tool
  also produced per-cell labels, `NULL` otherwise.

- `more`: extra cluster mappings when the tool supports them (e.g. the
  `more_cell_types` of the `direct` tool), `NULL` otherwise.

## Details

Cluster-level tools (the ones with `level = "cluster"`) need `ident`,
the metadata column holding the clusters, and label each cluster as a
whole. Cell-level tools label individual cells; when `ident` is also
given, the per-cell labels are aggregated to cluster-level ones by
majority vote.

## Examples

``` r
# \donttest{
obj <- SeuratObject::pbmc_small  # clusters in `groups`: g1, g2, g3
rec <- RunCellTypeAnnotation(
    obj, "direct",
    args = list(cell_types = list(g1 = "T", g2 = "B", g3 = "Mono")),
    ident = "groups"
)
#> INFO    [2026-09-19 06:11:23] Running cell type annotation tool 'direct' ...
#> WARN    [2026-09-19 06:11:23] The following clusters do not exist: g3
rec$type
#> [1] "cluster"
rec$mapping
#> $g1
#> [1] "T"
#> 
#> $g2
#> [1] "B"
#> 
# }
```
