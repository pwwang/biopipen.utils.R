# Get the column name in meta.data that works as identity

Get the column name in meta.data that works as identity

## Usage

``` r
GetIdentityColumn(object)
```

## Arguments

- object:

  Seurat object

## Value

The column name in meta.data that works as identity

## Examples

``` r
obj <- SeuratObject::pbmc_small
GetIdentityColumn(obj)
#> [1] "RNA_snn_res.1"

SeuratObject::Idents(obj) <- "groups"
GetIdentityColumn(obj)
#> [1] "groups"
```
