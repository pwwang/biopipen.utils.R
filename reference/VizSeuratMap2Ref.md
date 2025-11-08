# Visualize features between the query and reference Seurat objects by [`RunSeuratMap2Ref()`](https://pwwang.github.io/biopipen.utils.R/reference/RunSeuratMap2Ref.md)

Visualize features between the query and reference Seurat objects by
[`RunSeuratMap2Ref()`](https://pwwang.github.io/biopipen.utils.R/reference/RunSeuratMap2Ref.md)

## Usage

``` r
VizSeuratMap2Ref(
  query,
  ref,
  features,
  split_by = NULL,
  plot_type = c("dim", "violin", "box", "bar", "ridge", "heatmap", "dot"),
  reduction = NULL,
  ident = NULL,
  combine = TRUE,
  nrow = NULL,
  ncol = ifelse(length(features) > 1, 1, 2),
  byrow = NULL,
  axes = NULL,
  axis_titles = NULL,
  guides = NULL,
  design = NULL,
  ...
)
```

## Arguments

- query:

  A Seurat object with query data

- ref:

  A Seurat object with reference data

- features:

  Features from the query and reference to visualize. The format is
  'query:ref' or 'feature', where 'feature' is the same in both query
  and reference. The query and reference features must be the same type
  (numeric or factor/character). All features must be in the same type.
  A special case is to visualize the mapping score, for example
  `"seurat_clusters.score"`. If so, only one feature can be provided,
  and the plot type must be "dim". The reference will be plotted with
  the identity of the reference dataset.

- split_by:

  Column name in the query object to split the plot by, will not be
  supported. The plot will be split by the query/reference features
  instead.

- plot_type:

  Type of plot to generate. One of 'dim', 'violin', 'box', 'bar',
  'ridge', 'heatmap' and 'dot'.

  - 'dim': Dimensionality reduction plot. If the features are numeric,
    [`scplotter::FeatureStatPlot()`](https://pwwang.github.io/scplotter/reference/FeatureStatPlot.html)
    will be used. If the features are factor/character,
    [`scplotter::CellDimPlot()`](https://pwwang.github.io/scplotter/reference/CellDimPlot.html)
    will be used.

  - other:
    [`scplotter::FeatureStatPlot()`](https://pwwang.github.io/scplotter/reference/FeatureStatPlot.html)
    will be used.

- reduction:

  Dimensionality reduction to use for the plot. If NULL, the default
  reduction will be used for both query and reference. If the format is
  'reduction_q:reduction_r', the first part will be used for the query
  and the second part for the reference. If the format is 'reduction',
  the same reduction will be used for both query and reference.

- ident:

  Column name in the query and reference object to use for the plot. If
  NULL, the default identity will be used for both query and reference.
  If the format is 'ident_q:ident_r', the first part will be used for
  the query and the second part for the reference. If the format is
  'ident', the same identity will be used for both query and reference.

- combine:

  Whether to combine the plots into one plot. If FALSE, the plots will
  be returned as a list.

- nrow:

  Number of rows to use for the combined plot. If NULL, the number of
  rows will be calculated based on the number of features and ncol. If
  ncol is NULL, the number of columns will be calculated based on the
  number of features and nrow.

- ncol:

  Number of columns to use for the combined plot. If NULL, the number of
  columns will be calculated based on the number of features and nrow.
  If nrow is NULL, the number of rows will be calculated based on the
  number of features and ncol.

- byrow:

  Whether to combine the plots by row or column.

- axes:

  Whether to show the axes for the combined plot.

- axis_titles:

  Whether to show the axis titles for the combined plot.

- guides:

  Whether to show the guides for the combined plot.

- design:

  Design for the combined plot. See also
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).

- ...:

  Additional arguments to pass to the plot function
