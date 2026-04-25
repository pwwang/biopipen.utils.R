# Get the column name in meta.data that works as identity

Get the column name in meta.data that works as identity

## Usage

``` r
GetIdentityColumn(object, return_all = FALSE)
```

## Arguments

- object:

  Seurat object

- return_all:

  Whether to return all candidate columns. If FALSE, only return the
  best candidate column with the shortest name. The best candidate
  column is the one that matches the Idents(object) and has the shortest
  name. If there are multiple columns with the same shortest name,
  return the one with "cluster" or "annotation" or "type" in the name.
  If there is still a tie, return the first one. If return_all is TRUE,
  return all candidate columns that match the Idents(object) as a
  character vector. The best candidate column will be the first one in
  the returned vector.

## Value

The column name in meta.data that works as identity If there are
multiple columns matching, return the shortest one.

## Examples

``` r
obj <- SeuratObject::pbmc_small
GetIdentityColumn(obj)
#> [1] "RNA_snn_res.1"

SeuratObject::Idents(obj) <- "groups"
GetIdentityColumn(obj)
#> [1] "groups"
```
