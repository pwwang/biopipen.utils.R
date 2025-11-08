# Pre-rank genes based on expression data

Pre-rank genes based on expression data

## Usage

``` r
RunGSEAPreRank(
  exprs,
  classes,
  case,
  control = NULL,
  method = c("signal_to_noise", "abs_signal_to_noise", "t_test", "ratio_of_classes",
    "diff_of_classes", "log2_ratio_of_classes", "s2n", "abs_s2n")
)
```

## Arguments

- exprs:

  Expression data matrix (genes x samples)

- classes:

  A vector of class labels for each sample Must be in the same order as
  the columns of `exprs`

- case:

  The case group name in the `classes` vector

- control:

  The control group name in the `classes` vector If `NULL`, the control
  group will be the other groups in `classes`

- method:

  The method to use for ranking One of "signal_to_noise",
  "abs_signal_to_noise", "t_test", "ratio_of_classes",
  "diff_of_classes", "log2_ratio_of_classes", "s2n", "abs_s2n"

## Value

A vector of the rank values with names as the gene names

## See also

https://gseapy.readthedocs.io/en/latest/run.html#gseapy.gsea
