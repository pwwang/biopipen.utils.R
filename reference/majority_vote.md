# Majority vote the labels of each cluster

For every cluster, the label held by the most cells is picked. Labels
equal to `unknown` (and `NA`s) are ignored while counting; clusters
without any known label are mapped to `unknown`.

## Usage

``` r
majority_vote(labels, clusters, unknown = "unknown")
```

## Arguments

- labels:

  A vector of cell-level labels.

- clusters:

  A vector of cell-level clusters, of the same length as `labels`.

- unknown:

  The sentinel label to ignore while counting. Default is `"unknown"`.

## Value

A named list mapping each cluster to its voted label; a cluster with no
known label is mapped to `unknown`.
