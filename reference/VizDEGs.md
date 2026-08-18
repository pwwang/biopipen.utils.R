# Visualize DEGs

Visualize differentially expressed genes

## Usage

``` r
VizDEGs(
  markers,
  object = NULL,
  plot_type = c("volcano", "volcano_log2fc", "volcano_pct", "jitter", "jitter_log2fc",
    "jitter_pct", "heatmap_log2fc", "heatmap_pct", "dot_log2fc", "dot_pct", "heatmap",
    "violin", "box", "bar", "ridge", "dot"),
  each = NULL,
  facet_each = FALSE,
  comparison_by = NULL,
  p_adjust = TRUE,
  cutoff = NULL,
  order_by = "desc(abs(avg_log2FC))",
  select = ifelse(plot_type %in% c("volcano", "volcano_log2fc", "volcano_pct",
    "jitter", "jitter_log2fc", "jitter_pct", "heatmap_log2fc", "heatmap_pct"), 5, 10),
  outprefix = NULL,
  devpars = list(res = 100),
  more_formats = c(),
  save_code = FALSE,
  ...
)
```

## Arguments

- markers:

  A data frame of differential expression results, typically the output
  of
  [`Seurat::FindMarkers()`](https://satijalab.org/seurat/reference/FindMarkers.html)
  or
  [`Seurat::FindAllMarkers()`](https://satijalab.org/seurat/reference/FindAllMarkers.html).
  Must contain columns `"gene"` (or gene symbols as rownames),
  `"p_val"`, and `"avg_log2FC"`. For percentage-based plots
  (`volcano_pct`, `jitter_pct`, `heatmap_pct`, `dot_pct`), columns
  `"pct.1"` and `"pct.2"` are also required.

- object:

  A Seurat object. Required for expression-based plot types:
  `"heatmap"`, `"violin"`, `"box"`, `"bar"`, `"ridge"`, and `"dot"`. Not
  used for DE summary plot types. Default: `NULL`.

- plot_type:

  The type of plot to generate. One of `"volcano"`, `"volcano_log2fc"`,
  `"volcano_pct"`, `"jitter"`, `"jitter_log2fc"`, `"jitter_pct"`,
  `"heatmap_log2fc"`, `"heatmap_pct"`, `"dot_log2fc"`, `"dot_pct"`,
  `"heatmap"`, `"violin"`, `"box"`, `"bar"`, `"ridge"`, or `"dot"`. See
  Description for details on each type.

- each:

  A column name in `markers` indicating the grouping from which each
  marker was identified (e.g., the `cluster` column from
  `FindAllMarkers()`). Supports the `"marker_column:metadata_column"`
  syntax for linking to Seurat object metadata (see **Metadata column
  mapping** section). For jitter and DE heatmap/dot plot types, `each`
  is required and defines the x-axis or column groups. For expression
  plot types, `each` controls faceting or splitting. Default: `NULL`.

- facet_each:

  Logical. If `TRUE`, facet the plot by `each` groups instead of
  splitting into separate plots. Most useful for expression plot types.
  For volcano plots, controls whether faceting or split_by dispatch is
  used. Default: `FALSE`.

- comparison_by:

  A column name in `markers` indicating the comparison (e.g., `"g1:g2"`
  for a pairwise comparison, or a single group name for one-vs-rest).
  Required for expression-based plot types (`"heatmap"`, `"violin"`,
  `"box"`, `"bar"`, `"ridge"`, `"dot"`). Supports the
  `"marker_column:metadata_column"` syntax (see **Metadata column
  mapping** section). If the comparison values contain a colon (e.g.,
  `"G2M:G1"`), the two groups on either side of the colon are used to
  subset the object. If only a single group is present, a one-vs-other
  comparison is assumed. Default: `NULL`.

- p_adjust:

  Logical. If `TRUE` (default), use adjusted p-value (`p_val_adj`
  column) for significance calculations and y-axis transformations. If
  `FALSE`, use raw p-value (`p_val` column).

- cutoff:

  Numeric. The p-value (or adjusted p-value, depending on `p_adjust`)
  threshold for labeling significance. For volcano plots, sets
  `y_cutoff`. For heatmap-based DE plots (`heatmap_log2fc`,
  `heatmap_pct`), controls which cells receive significance marks.
  Default: `NULL` (no cutoff; defaults to `0.05` for volcano plots).

- order_by:

  A string expression to order markers by (evaluated with
  [`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)).
  Can reference columns in `markers` as well as columns from the object
  metadata (when `object` is provided and `each` enables merging). Only
  the first value of merged metadata columns is used. Example:
  `"desc(avg_log2FC)"`. The ordering affects which markers are selected
  when `select` is numeric. Default: `desc(abs(avg_log2FC))`.

- select:

  How to select markers for labeling or display. See **Marker selection
  and filtering** section for full details.

  - Numeric: Top N markers per `each` group (default: `5` for
    volcano/jitter types, `10` for others).

  - Character expression: Filter condition for
    [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html).

  - Character vector: Multiple filter expressions; those containing the
    `each` column name filter the overall data, others filter within
    remaining data.

- outprefix:

  Prefix of the output file

- devpars:

  List of parameters to save the plot

- more_formats:

  Additional formats to save the plot in addition to 'png'

- save_code:

  Whether to save the code to reproduce the plot

- ...:

  Arguments passed on to
  [`scplotter::MarkersPlot`](https://pwwang.github.io/scplotter/reference/MarkersPlot.html)

  `subset_by`

  :   Deprecated. Use `each` instead.

  `subset_as_facet`

  :   Deprecated. Use `facet_each` instead.

  `show_labels`

  :   Logical. For `heatmap_log2fc` and `heatmap_pct` plot types only.
      If `TRUE`, display numeric values in heatmap cells. When combined
      with `cutoff`, both values and significance marks are shown.
      Default: `FALSE`.

  `sig_mark`

  :   Character. The symbol or compound mark used to annotate
      statistically significant cells in `heatmap_log2fc` and
      `heatmap_pct` plots. Must be a valid ComplexHeatmap mark: single
      characters (`"-"`, `"|"`, `"+"`, `"/"`, `"\\"`, `"x"`, `"o"`) or
      compound marks (`"[*]"`, `"<*>"`, `"(*)"`, `"{*}"`). Note that
      `"*"` conflicts with `show_labels = TRUE` because both use the
      label layer — use a compound mark instead. Default: `"*"`.

## Value

A ggplot object if 'outprefix' is NULL, otherwise, save the plot to the
output directory

## See also

[`scplotter::MarkersPlot()`](https://pwwang.github.io/scplotter/reference/MarkersPlot.html)

## Examples

``` r
# \donttest{
degs <- RunSeuratDEAnalysis(scplotter::pancreas_sub, "SubCellType")
VizDEGs(degs, plot_type = "volcano_pct")

VizDEGs(degs, plot_type = "volcano_log2fc")

VizDEGs(degs, plot_type = "jitter_log2fc", each = "SubCellType")

VizDEGs(degs,
    plot_type = "heatmap_log2fc", cutoff = 0.05,
    select = 5, each = "SubCellType"
)


# Visualize expression of the top DEGs
# Suppose we did comparison between G2M and S phase in each SubCellType
degs$Phase <- "G2M:S"

VizDEGs(degs,
    object = scplotter::pancreas_sub, plot_type = "violin",
    select = 2, comparison_by = "Phase", each = "SubCellType"
)
#> Centering and scaling data matrix
#> Warning: Layer counts isn't present in the assay object; returning NULL
#> Warning: Layer counts isn't present in the assay object; returning NULL
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.

VizDEGs(degs,
    object = scplotter::pancreas_sub, plot_type = "box",
    select = 2, comparison_by = "Phase", each = "SubCellType"
)
#> Centering and scaling data matrix
#> Warning: Layer counts isn't present in the assay object; returning NULL
#> Warning: Layer counts isn't present in the assay object; returning NULL

VizDEGs(degs,
    object = scplotter::pancreas_sub, plot_type = "bar",
    select = 2, comparison_by = "Phase", each = "SubCellType"
)
#> Centering and scaling data matrix
#> Warning: Layer counts isn't present in the assay object; returning NULL
#> Warning: Layer counts isn't present in the assay object; returning NULL

VizDEGs(degs,
    object = scplotter::pancreas_sub, plot_type = "ridge",
    select = 1, comparison_by = "Phase", each = "SubCellType"
)
#> Centering and scaling data matrix
#> Warning: Layer counts isn't present in the assay object; returning NULL
#> Warning: Layer counts isn't present in the assay object; returning NULL
#> Picking joint bandwidth of 0.0834
#> Picking joint bandwidth of 0.0961
#> Picking joint bandwidth of 0.307
#> Picking joint bandwidth of 0.0264
#> Picking joint bandwidth of 0.043
#> Picking joint bandwidth of 0.0335
#> Picking joint bandwidth of 0.302
#> Picking joint bandwidth of 0.061
#> Picking joint bandwidth of 0.135
#> Picking joint bandwidth of 0.164
#> Picking joint bandwidth of 0.405
#> Picking joint bandwidth of 0.181
#> Picking joint bandwidth of 0.052
#> Picking joint bandwidth of 0.203
#> Picking joint bandwidth of 0.16
#> Picking joint bandwidth of 0.0835
#> Picking joint bandwidth of 0.132
#> Picking joint bandwidth of 0.15
#> Picking joint bandwidth of 0.192
#> Picking joint bandwidth of 0.106
#> Picking joint bandwidth of 0.612
#> Picking joint bandwidth of 0.294
#> Picking joint bandwidth of 0.365
#> Picking joint bandwidth of 0.151
#> Picking joint bandwidth of NaN
#> Picking joint bandwidth of NaN
#> Picking joint bandwidth of NaN
#> Picking joint bandwidth of NaN
#> Picking joint bandwidth of NaN
#> Picking joint bandwidth of NaN
#> Picking joint bandwidth of NaN
#> Picking joint bandwidth of NaN
#> Warning: No shared levels found between `names(values)` of the manual scale and the
#> data's fill values.
#> Picking joint bandwidth of 0.0834
#> Picking joint bandwidth of 0.0961
#> Picking joint bandwidth of 0.307
#> Picking joint bandwidth of 0.0264
#> Picking joint bandwidth of 0.043
#> Picking joint bandwidth of 0.0335
#> Picking joint bandwidth of 0.302
#> Picking joint bandwidth of 0.061
#> Picking joint bandwidth of 0.135
#> Picking joint bandwidth of 0.164
#> Picking joint bandwidth of 0.405
#> Picking joint bandwidth of 0.181
#> Picking joint bandwidth of 0.052
#> Picking joint bandwidth of 0.203
#> Picking joint bandwidth of 0.16
#> Picking joint bandwidth of 0.0835
#> Picking joint bandwidth of 0.132
#> Picking joint bandwidth of 0.15
#> Picking joint bandwidth of 0.192
#> Picking joint bandwidth of 0.106
#> Picking joint bandwidth of 0.612
#> Picking joint bandwidth of 0.294
#> Picking joint bandwidth of 0.365
#> Picking joint bandwidth of 0.151
#> Picking joint bandwidth of NaN
#> Picking joint bandwidth of NaN
#> Picking joint bandwidth of NaN
#> Picking joint bandwidth of NaN
#> Picking joint bandwidth of NaN
#> Picking joint bandwidth of NaN
#> Picking joint bandwidth of NaN
#> Picking joint bandwidth of NaN
#> Warning: No shared levels found between `names(values)` of the manual scale and the
#> data's fill values.

VizDEGs(degs,
    object = scplotter::pancreas_sub, plot_type = "heatmap",
    cluster_columns = FALSE, comparison_by = "Phase", each = "SubCellType"
)
#> Centering and scaling data matrix
#> Warning: Layer counts isn't present in the assay object; returning NULL
#> Warning: Layer counts isn't present in the assay object; returning NULL

VizDEGs(degs,
    object = scplotter::pancreas_sub, plot_type = "dot",
    select = 1, comparison_by = "Phase", each = "SubCellType"
)
#> Centering and scaling data matrix
#> Warning: Layer counts isn't present in the assay object; returning NULL
#> Warning: Layer counts isn't present in the assay object; returning NULL

# }
```
