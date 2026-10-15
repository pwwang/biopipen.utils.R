# Annotate the cell types of a Seurat object in one call

Runs a cell type annotation tool with
[`RunCellTypeAnnotation()`](https://pwwang.github.io/biopipen.utils.R/reference/RunCellTypeAnnotation.md)
and writes the result into the object with
[`EmbedSeuratCellTypeAnnotation()`](https://pwwang.github.io/biopipen.utils.R/reference/EmbedSeuratCellTypeAnnotation.md),
so that a Seurat object goes in and an annotated Seurat object comes
out. Use the two functions directly instead when the record itself is
needed — biopipen's `CellTypeAnnotation` process keeps it per case to
write its `cluster2celltype`/`cell2celltype` tables.

## Usage

``` r
RunSeuratCellTypeAnnotation(
  object,
  tool,
  args = list(),
  ident = NULL,
  case = NULL,
  anno_col = "CellType",
  add_prefix = TRUE,
  set_ident = TRUE,
  merge = FALSE,
  cache = NULL,
  log = NULL
)
```

## Arguments

- object:

  Seurat object

- tool:

  Name of the tool to run, see
  [`celltype_annotation_tools()`](https://pwwang.github.io/biopipen.utils.R/reference/celltype_annotation_tools.md).
  Case insensitive.

- args:

  Arguments for the tool, as documented in
  [`RunCellTypeAnnotation()`](https://pwwang.github.io/biopipen.utils.R/reference/RunCellTypeAnnotation.md).

- ident:

  The metadata column with the clusters, or `"ident"` for the identity
  column. Defaults to the identity column for cluster-based calls.

- case:

  Name of the case the annotation comes from, used as the prefix of the
  new column names unless `add_prefix` is `FALSE`. `NULL` (default) adds
  no prefix.

- anno_col:

  Name of the metadata column to store the annotation in.

- add_prefix:

  Whether to prefix the new column names with `case`.

- set_ident:

  Whether to set the identity of the object to the annotation column.

- merge:

  Whether to merge the clusters annotated with the same cell type.
  Otherwise a suffix (`.1`, `.2`, ...) is added to the cell types.

- cache:

  Directory for conversions and tool scratch files, default
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- log:

  Logger.

## Value

The Seurat object with the annotation added to the metadata

## Details

`ident` is resolved the same way the `CellTypeAnnotation` process
resolves it: the literal `"ident"` means the identity column, and the
column the mapping is keyed on defaults to the identity column only for
cluster-based calls (a cluster-level tool, or any call given an
`ident`). A cell-level tool with no `ident` stays cell-level. For
`celltypist` with `over_clustering`, the mapping is keyed on the
`over_clustering` column rather than on `ident`.

Calling this once per case with a different `case` reproduces the
per-case columns of the process:
[`EmbedSeuratCellTypeAnnotation()`](https://pwwang.github.io/biopipen.utils.R/reference/EmbedSeuratCellTypeAnnotation.md)
prefixes every new column with the case name unless `add_prefix` is
`FALSE`.

## Examples

``` r
# \donttest{
obj <- SeuratObject::pbmc_small
obj <- RunSeuratCellTypeAnnotation(
    obj, "direct",
    args = list(cell_types = list(g1 = "T", g2 = "B")),
    ident = "groups"
)
#> INFO    [2026-10-15 04:36:19] Running cell type annotation tool 'direct' ...
#> INFO    [2026-10-15 04:36:19] Adding annotation as new column: CellType
table(obj$CellType)
#> 
#>  T  B 
#> 44 36 
# }
```
