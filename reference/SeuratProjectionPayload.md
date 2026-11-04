# Extract the dimensional reductions of a Seurat object as a compact payload

Returns one entry per dimensional reduction of `object` (except those
named in `exclude`) with the cell embeddings, the reduction key and its
assay, so that a forked annotation run can hand a case's projections
back without keeping the whole object alive.

## Usage

``` r
SeuratProjectionPayload(object, exclude = NULL)
```

## Arguments

- object:

  Seurat object

- exclude:

  Names of reductions to skip

## Value

A list with one entry per reduction: `name`, `embeddings` (matrix, cells
x dims), `key`, `assay`.
