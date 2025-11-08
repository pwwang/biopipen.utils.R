# Run DESeq2 differential expression analysis

Run DESeq2 differential expression analysis

## Usage

``` r
.run_deseq2(exprs, meta, group_by, ident_1, ident_2, paired_by, log, ncores)
```

## Arguments

- exprs:

  Expression matrix with genes as rows and samples as columns.

- meta:

  Metadata data frame with sample information.

- group_by:

  Column name in `meta` to group samples for differential expression
  analysis

- ident_1:

  First identity to compare against

- ident_2:

  Second identity to compare against

- paired_by:

  Column name in `meta` for paired samples.

- log:

  Logger

- ncores:

  Number of cores to use for parallel processing.

## Value

A data frame with DESeq2 differential expression results.
