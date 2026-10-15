# Convert a marker table to the ScInfeR format

A data.frame with `celltype`, `marker` and `weight` columns, the
`ct_marker_df` that `ScInfeR::predict_celltype_scRNA_seurat()` takes.

## Usage

``` r
markers_to_scinfer_df(df, tissue = NULL, cancer = NULL, species = NULL)
```

## Arguments

- df:

  A canonical marker table.

- tissue:

  The tissue to filter by. If NULL, no filtering is done.

- cancer:

  The cancer to filter by. If NULL, no filtering is done.

- species:

  The species to filter by. If NULL, no filtering is done.

## Value

A data.frame with `celltype`, `marker` and `weight` columns.

## Details

Only positive markers are kept: ScInfeR has no direction, so a
negative-direction marker would be counted as a marker of the cell type.
The tool accepts per-marker weights but the universal marker table does
not have to carry them: without a `weight` column every marker gets
weight `1` (ScInfeR's own database weights each marker).
