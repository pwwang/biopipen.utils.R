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
  p_adjust = TRUE,
  cutoff = NULL,
  order_by = "desc(abs(avg_log2FC))",
  select = ifelse(plot_type %in% c("volcano", "volcano_log2fc", "volcano_pct",
    "jitter", "jitter_log2fc", "jitter_pct", "heatmap_log2fc", "heatmap_pct"), 5, 10),
  outprefix = NULL,
  devpars = list(res = 100),
  more_formats = c(),
  save_code = FALSE,
  log = NULL,
  log_prefix = "",
  cache = NULL,
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
  `FindAllMarkers()`). Required for jitter and DE heatmap/dot plot
  types, where it defines the x-axis or column groups. For volcano plot
  types, it splits the plot by group (or facets it, with
  `facet_each = TRUE`). For expression plot types, `each` is used to
  select the markers within each group; a plain column name does not
  split the plot — use the `"marker_column:metadata_column"` syntax (see
  **Metadata column mapping**) to also split the plot by the mapped
  metadata column. Alternatively, pass `":metadata_column"` with an
  empty marker part to split the expression plot by the metadata column
  directly, without selecting markers per group (markers are selected
  overall) and without merging metadata. Default: `NULL`.

- facet_each:

  Logical. Only for volcano plot types: if `TRUE`, facet the volcano
  plot by the `each` groups instead of splitting it into separate
  subplots. Ignored for other plot types. Default: `FALSE`.

- p_adjust:

  Logical. If `TRUE` (default), use adjusted p-value (`p_val_adj`
  column) for significance calculations and y-axis transformations. If
  `FALSE`, use raw p-value (`p_val` column).

- cutoff:

  Numeric. The p-value (or adjusted p-value, depending on `p_adjust`)
  threshold for labeling significance. For volcano plots, sets
  `y_cutoff`. For DE heatmap plots (`heatmap_log2fc`, `heatmap_pct`),
  controls which cells receive significance marks. For expression plot
  types with a numeric `select`, only markers with a p-value below
  `cutoff` are eligible for selection. Ignored by DE dot plots
  (`dot_log2fc`, `dot_pct`). Default: `NULL` (no cutoff; defaults to
  `0.05` for volcano plots).

- order_by:

  A string of one or more comma-separated expressions used to order the
  markers (evaluated with
  [`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)).
  Can reference columns in `markers` as well as metadata columns merged
  in via a colon-form `each` (see **Metadata column mapping**). Only the
  first value of each merged metadata column is kept. Example:
  `"desc(avg_log2FC)"` or `"desc(avg_log2FC), desc(pct.1)"`. The
  ordering determines which markers are selected when `select` is
  numeric. For jitter plots, it is also passed to
  [`plotthis::JitterPlot()`](https://pwwang.github.io/plotthis/reference/jitterplot.html).
  Default: `"desc(abs(avg_log2FC))"`.

- select:

  How to select markers for display or labeling. See **Marker selection
  and filtering** section for full details.

  - Numeric: Top N markers per `each` group, or overall when `each` is
    `NULL` (default: `5` for volcano/jitter types and for expression
    plot types when `each` selects markers per group, `10` otherwise).

  - Single expression: Filter condition for
    [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html).

  - Character vector of multiple expressions (DE heatmap/dot plot types
    only): expressions mentioning the `each` column name filter the
    overall data, others filter within the remaining data.

- outprefix:

  Prefix of the output file

- devpars:

  List of parameters to save the plot

- more_formats:

  Additional formats to save the plot in addition to 'png'

- save_code:

  Whether to save the code to reproduce the plot

- log:

  A logger object

- log_prefix:

  Prefix to add to the log messages

- cache:

  Directory to cache the plot. Default to
  [`gettempdir()`](https://pwwang.github.io/biopipen.utils.R/reference/gettempdir.md)

- ...:

  Arguments passed on to
  [`scplotter::MarkersPlot`](https://pwwang.github.io/scplotter/reference/MarkersPlot.html)

  `group_by`

  :   Used only for expression-based plot types (ignored for DE summary
      plot types). A column in the Seurat object's metadata to group
      cells by, e.g., a condition column — useful when the DEs were
      calculated between conditions (such as cell cycle phases) and you
      want to compare the expression of the markers across those
      conditions. A single value is passed directly to
      [`FeatureStatPlot`](https://pwwang.github.io/scplotter/reference/FeatureStatPlot.html):
      for `heatmap` and `dot` plots it is applied as the column
      annotation (`ident`), and it only takes effect when `each`
      includes a metadata column mapping; for `violin`, `box`, `bar`,
      and `ridge` plots it is passed as `group_by`. The
      `"marker_column:metadata_column"` syntax (see **Metadata column
      mapping**) restricts the object to only the cells involved in the
      comparisons: for example, if a `comparison` column in the markers
      data frame holds `"G1:G2M"`, passing
      `group_by = "comparison:Phase"` keeps only G1 and G2M cells in the
      plot, with the `Phase` column re-factored to these two levels in
      the order they first appear in the `comparison` column. Without
      the restriction, e.g., `group_by = "Phase"`, all phase cells (G1,
      G2M, and S) are included in the plot. Default: `NULL`.

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

  `flatten_markers`

  :   Logical. Only for the expression `heatmap` and `dot` plot types.
      When `each` is used to select markers per group, the markers are
      by default provided to
      [`FeatureStatPlot`](https://pwwang.github.io/scplotter/reference/FeatureStatPlot.html)
      as a named list (one entry per group), which splits the feature
      rows of the plot by group. With `flatten_markers = TRUE`, the
      selected markers are collapsed into a single vector so the plot
      shows one unsplit block of features — useful e.g. to mimic
      [`Seurat::DoHeatmap()`](https://satijalab.org/seurat/reference/DoHeatmap.html)
      on globally selected markers. Default: `FALSE`.

## Value

A ggplot object if 'outprefix' is NULL, otherwise, save the plot to the
output directory

## See also

[`scplotter::MarkersPlot()`](https://pwwang.github.io/scplotter/reference/MarkersPlot.html)

## Examples

``` r
# \donttest{
degs <- RunSeuratDEAnalysis(scplotter::pancreas_sub, "SubCellType")
#> 
VizDEGs(degs, plot_type = "volcano_pct")

VizDEGs(degs, plot_type = "volcano_log2fc")

VizDEGs(degs, plot_type = "jitter_log2fc", each = "SubCellType")
#> Warning: [JitterPlot] `raster` is ignored when `size_by` is mapped to a column; falling back to vector points.

VizDEGs(degs,
    plot_type = "heatmap_log2fc", cutoff = 0.05,
    select = 5, each = "SubCellType"
)

# mimic Seurat's DoHeatmap()
top_degs <- degs[order(degs$avg_log2FC, decreasing = TRUE), ]
Seurat::DoHeatmap(scplotter::pancreas_sub, features = unique(top_degs$gene)[1:10],
   slot = "data", group.by = "SubCellType")

VizDEGs(degs,
    object = scplotter::pancreas_sub,
    layer = "data", plot_type = "heatmap",
    # Make the heatmap cells filled with bars
    cell_type = "bars",
    # Make sure "select" the top (positive) DEGs by avg_log2FC
    order_by = "desc(avg_log2FC)",  select = 10,
    # Select overall top 10 DEGs for each SubCellType (empty name before :)
    # But use the SubCellType for column splits
    each = ":SubCellType",
    # Column names/title
    show_column_names = "inplace", column_names_side = "top", column_title_rot = 45,
    column_annotation = list(.column = list(name = FALSE)),
    # Row names
    row_names_side = "left", row_annotation = list(.row = list(name = FALSE)),
)
#> Warning: Layer counts isn't present in the assay object; returning NULL


# Suppose we did comparison between G2M and S phase in each SubCellType
degs$Phase <- "G2M:S"

VizDEGs(degs,
    object = scplotter::pancreas_sub, plot_type = "violin",
    position_dodge_preserve = "single",
    select = 2, group_by = "Phase:Phase", each = "SubCellType:SubCellType"
)
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
    select = 2, group_by = "Phase", each = "SubCellType:SubCellType"
)
#> Warning: Layer counts isn't present in the assay object; returning NULL

VizDEGs(degs,
    object = scplotter::pancreas_sub, plot_type = "bar",
    position_dodge_preserve = "single",
    select = 2, group_by = "Phase:Phase", each = "SubCellType:SubCellType"
)
#> Warning: Layer counts isn't present in the assay object; returning NULL

VizDEGs(degs,
    object = scplotter::pancreas_sub, plot_type = "ridge",
    select = 1, group_by = "Phase:Phase", each = "SubCellType:SubCellType"
)
#> Warning: Layer counts isn't present in the assay object; returning NULL
#> Picking joint bandwidth of 0.132
#> Picking joint bandwidth of 0.126
#> Picking joint bandwidth of 0.392
#> Picking joint bandwidth of 0.121
#> Picking joint bandwidth of 0.0751
#> Picking joint bandwidth of 0.18
#> Picking joint bandwidth of 0.341
#> Picking joint bandwidth of 0.11
#> Picking joint bandwidth of 0.152
#> Picking joint bandwidth of 0.124
#> Picking joint bandwidth of 0.462
#> Picking joint bandwidth of 0.151
#> Picking joint bandwidth of 0.571
#> Picking joint bandwidth of 0.239
#> Picking joint bandwidth of 0.417
#> Picking joint bandwidth of 0.196
#> Picking joint bandwidth of 0.0681
#> Picking joint bandwidth of 0.09
#> Picking joint bandwidth of 0.197
#> Picking joint bandwidth of 0.0363
#> Picking joint bandwidth of 0.14
#> Picking joint bandwidth of 0.128
#> Picking joint bandwidth of 0.318
#> Picking joint bandwidth of 0.0648
#> Picking joint bandwidth of 0.147
#> Picking joint bandwidth of 0.255
#> Picking joint bandwidth of 0.299
#> Picking joint bandwidth of 0.28
#> Picking joint bandwidth of 0.0752
#> Picking joint bandwidth of 0.437
#> Picking joint bandwidth of 0.521
#> Picking joint bandwidth of 0.171
#> Warning: No shared levels found between `names(values)` of the manual scale and the
#> data's fill values.
#> Picking joint bandwidth of 0.132
#> Picking joint bandwidth of 0.126
#> Picking joint bandwidth of 0.392
#> Picking joint bandwidth of 0.121
#> Picking joint bandwidth of 0.0751
#> Picking joint bandwidth of 0.18
#> Picking joint bandwidth of 0.341
#> Picking joint bandwidth of 0.11
#> Picking joint bandwidth of 0.152
#> Picking joint bandwidth of 0.124
#> Picking joint bandwidth of 0.462
#> Picking joint bandwidth of 0.151
#> Picking joint bandwidth of 0.571
#> Picking joint bandwidth of 0.239
#> Picking joint bandwidth of 0.417
#> Picking joint bandwidth of 0.196
#> Picking joint bandwidth of 0.0681
#> Picking joint bandwidth of 0.09
#> Picking joint bandwidth of 0.197
#> Picking joint bandwidth of 0.0363
#> Picking joint bandwidth of 0.14
#> Picking joint bandwidth of 0.128
#> Picking joint bandwidth of 0.318
#> Picking joint bandwidth of 0.0648
#> Picking joint bandwidth of 0.147
#> Picking joint bandwidth of 0.255
#> Picking joint bandwidth of 0.299
#> Picking joint bandwidth of 0.28
#> Picking joint bandwidth of 0.0752
#> Picking joint bandwidth of 0.437
#> Picking joint bandwidth of 0.521
#> Picking joint bandwidth of 0.171
#> Warning: No shared levels found between `names(values)` of the manual scale and the
#> data's fill values.

VizDEGs(degs,
    object = scplotter::pancreas_sub, plot_type = "heatmap", select = 3,
    cluster_columns = FALSE, group_by = "Phase:Phase", each = "SubCellType:SubCellType"
)
#> Warning: Layer counts isn't present in the assay object; returning NULL

VizDEGs(degs,
    object = scplotter::pancreas_sub, plot_type = "dot",
    select = 1, group_by = "Phase:Phase", each = "SubCellType:SubCellType"
)
#> Warning: Layer counts isn't present in the assay object; returning NULL

# }
```
