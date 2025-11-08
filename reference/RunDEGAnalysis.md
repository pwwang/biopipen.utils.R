# Run differential gene expression analysis

Run differential gene expression analysis

## Usage

``` r
RunDEGAnalysis(
  exprs,
  group_by,
  ident_1,
  ident_2 = NULL,
  paired_by = NULL,
  meta = "@meta",
  tool = c("DESeq2", "edgeR", "deseq2", "edger"),
  log = get_logger(),
  cache = NULL,
  ncores = 1
)
```

## Arguments

- exprs:

  Expression matrix with genes as rows and samples as columns.

- group_by:

  Column name in `meta` to group samples for differential expression
  analysis

- ident_1:

  First identity to compare against

- ident_2:

  Second identity to compare against If not specified, the rest of the
  samples will be used as the second identity.

- paired_by:

  Column name in `meta` for paired samples. For example, `Subject`, for
  each subject, there should be only one sample in each group (`ident_1`
  and `ident_2`).

- meta:

  Metadata data frame with sample information. If a `Sample` column is
  present, it will be used as the sample identifier, which is the column
  name in `exprs`.

- tool:

  Tool to use for differential expression analysis. Currently supports
  "DESeq2", "edgeR", and "limma".

- log:

  Logger

- cache:

  Directory to store cache files. If NULL, a temporary directory will be
  used.

- ncores:

  Number of cores to use for parallel processing. If set to 1, the
  analysis will run in single-threaded mode. If set to a value greater
  than 1, the analysis will run in multi-threaded mode. This is only
  applicable for DESeq2.

## Value

A data frame with differential expression results. With attributes:

- `object`: The input expression matrix

- `meta`: The metadata used for the analysis

- `paired_by`: The column name used for paired samples, if applicable.

- `group_by`: The column name used for grouping samples.

- `ident_1`: The first identity used for comparison.

- `ident_2`: The second identity used for comparison.
