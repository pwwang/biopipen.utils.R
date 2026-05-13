# Visualize contamination correction results of Seurat object

Visualize contamination correction results of Seurat object

## Usage

``` r
VizSeuratContamination(
  object,
  plot_type = c("ridge", "histogram", "bar", "violin", "box", "heatmap", "dot"),
  metric = c("contam", "contamination", "expr", "expression"),
  markers = NULL,
  ...
)
```

## Arguments

- object:

  A Seurat object with contamination correction results

- plot_type:

  Type of plot to generate

- metric:

  Metric to visualize, one of "contam", "contamination" and
  "expression". "contam" is an alias of "contamination". For `scCDC`,
  only sample-level contamination fraction is available. For `decontX`,
  cell-level contamination fraction is available.

- markers:

  List of marker genes to visualize expression of contamination genes.
  For `scCDC`, the `GCGs` will be used by default.

- ...:

  Additional arguments to pass to the plot function.
