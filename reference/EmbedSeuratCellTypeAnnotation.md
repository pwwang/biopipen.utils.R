# Add cell type annotation results to a Seurat object

Writes the record returned by
[`RunCellTypeAnnotation()`](https://pwwang.github.io/biopipen.utils.R/reference/RunCellTypeAnnotation.md)
into the object's metadata, so that the annotation can be used by the
downstream steps. The per-cell labels the tool produced (`record$cells`
for cluster-level tools that also predict per cell, `record$mapping` for
cell-level tools) are added as metadata columns, and the annotation
itself is stored in `anno_col`.

## Usage

``` r
EmbedSeuratCellTypeAnnotation(
  object,
  record,
  case = NULL,
  ident = NULL,
  anno_col = "CellType",
  add_prefix = TRUE,
  set_ident = TRUE,
  merge = FALSE,
  subset = NULL,
  log = NULL
)
```

## Arguments

- object:

  Seurat object

- record:

  A record returned by
  [`RunCellTypeAnnotation()`](https://pwwang.github.io/biopipen.utils.R/reference/RunCellTypeAnnotation.md)

- case:

  Name of the case the record came from, used as the prefix of the new
  column names unless `add_prefix` is `FALSE`. `NULL` (default) adds no
  prefix.

- ident:

  The metadata column the `record$mapping` is keyed on, ie. the `ident`
  the tool was run with. Only used for `record$type = "cluster"`, where
  it defaults to the identity column
  ([`GetIdentityColumn()`](https://pwwang.github.io/biopipen.utils.R/reference/GetIdentityColumn.md)).

- anno_col:

  Name of the metadata column to store the annotation in.

- add_prefix:

  Whether to prefix the new column names with `case`.

- set_ident:

  Whether to set the identity of the object to the annotation column.

- merge:

  Whether to merge the clusters annotated with the same cell type.
  Otherwise a suffix (`.1`, `.2`, ...) is added to the cell types.

- subset:

  A character vector of cell names the `record` covers. When given, the
  new columns are filled with `NA` for the cells not in `subset`. For
  cluster-level records the mapping is restricted to the subset cells
  even when the other cells share their clusters. `NULL` (default)
  applies the record to every cell.

- log:

  Logger.

## Value

The Seurat object with the annotation added to the metadata

## Details

Unlike
[`RunCellTypeAnnotation()`](https://pwwang.github.io/biopipen.utils.R/reference/RunCellTypeAnnotation.md),
no tool is run here: the caller runs the tool and passes the record in,
so the same record can be applied to different objects or under
different column names. Calling this once per record with a different
`case` reproduces the per-case columns of biopipen's
`CellTypeAnnotation` process.

## Examples

``` r
# \donttest{
obj <- SeuratObject::pbmc_small
rec <- RunCellTypeAnnotation(
    obj, "direct",
    args = list(cell_types = list(g1 = "T", g2 = "B")),
    ident = "groups"
)
#> INFO    [2026-10-18 06:44:35] Running cell type annotation tool 'direct' ...
obj <- EmbedSeuratCellTypeAnnotation(obj, rec, ident = "groups")
#> INFO    [2026-10-18 06:44:35] Adding annotation as new column: CellType
table(obj$CellType)
#> 
#>  T  B 
#> 44 36 
# }
```
