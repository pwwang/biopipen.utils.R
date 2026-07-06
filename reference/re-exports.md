# Re-exported functions from other packages

- [`enrichit::EnrichIt()`](https://pwwang.github.io/enrichit/reference/EnrichIt.html)
  (exported as RunEnrichment) - Enrichment analysis

- [`enrichit::ParseGMT()`](https://pwwang.github.io/enrichit/reference/ParseGMT.html) -
  Parse GMT file

## Usage

``` r
RunEnrichment(
  userlist,
  dbs,
  method = ifelse(tolower(style[1]) == "enrichr", "fisher", "hypergeometric"),
  use_matched_only = ifelse(tolower(style[1]) == "enrichr", FALSE, TRUE),
  padjust_method = c("BH", "bonferroni", "holm", "hochberg", "hommel", "BY", "fdr"),
  background = NULL,
  style = c("enrichr", "Enrichr", "clusterProfiler", "ClusterProfiler",
    "clusterprofiler"),
  return_all = FALSE
)

ParseGMT(gmtfile, swap_name_desc_if_needed = TRUE)

VizEnrichment(
  data,
  top_term = NULL,
  plot_type = c("bar", "dot", "lollipop", "network", "enrichmap", "wordcloud",
    "comparison", "heatmap"),
  x_by = NULL,
  size_by = NULL,
  fill_cutoff_name = NULL,
  fill_name = NULL,
  values_fill = 0,
  character_width = 50,
  expand = NULL,
  word_type = c("term", "feature"),
  split_by = NULL,
  split_by_sep = "_",
  facet_by = NULL,
  facet_scales = NULL,
  group_by = NULL,
  group_by_sep = "_",
  metric = "p.adjust",
  cutoff = NULL,
  palette = "Spectral",
  xlab = NULL,
  ylab = NULL,
  ...
)
```

## Arguments

- userlist:

  Vector of user-provided genes

- dbs:

  List of gene sets or paths to GMT files It can be a vector of database
  names. You can set the names of the vector, which will be used as the
  database names. Otherwise a list is expected, where each element is a
  database (e.g. parsed from a gmt file). If a database is given
  directly (without a name), the expression of this argument will be
  used as the database name.

- method:

  Method for computing p-value, either "fisher" or "hypergeometric" When
  `style` is "enrichr", the method defaults to "fisher". When `style` is
  "clusterProfiler", the method defaults to "hypergeometric".

- use_matched_only:

  Logical, whether to use only matched genes against the gene sets. This
  will affect the number of genes in the user list when computing the
  p-value. By default, when `style` is "enrichr", this is set to FALSE.
  When `style` is "clusterProfiler", this is set to TRUE.

- padjust_method:

  Method for adjusting p-values, either "BH", "bonferroni", "holm",
  "hochberg", "hommel", "BY", "fdr"

- background:

  Vector of all genes in the universe or a number of genes in the
  universe. If NULL, the number of genes in the gene set will be used.
  For "enrichr", the default is 20,000. For "clusterProfiler", the
  default is the number of unique genes in the gene set. Note that for
  "enrichr", if a vector is given, the length of it will be used, no
  checking will be done to see if userlist and genes from dbs are in the
  vector.

- style:

  Style of the output, either "enrichr" or "clusterProfiler"

- return_all:

  Logical, whether to return all results (all gene sets in dbs) or only
  those with at least one gene in the user list.

- gmtfile:

  Path to the GMT file

- swap_name_desc_if_needed:

  Logical, whether to swap name and description fields. They will be
  swapped only if:

  - `swap_name_desc_if_needed` is `TRUE`; and

  - The descriptions are not empty; and

  - The descriptions are shorter than the names; and

  - The descriptions are not ID-like (i.e., hsa00001, or 123456).

- data:

  A data frame with enrichment results. Must be the output of a
  clusterProfiler function (`enrichGO`, `enrichKEGG`, `enrichPathway`,
  `enrichWP`, etc.) or an enrichR result processed through
  [`plotthis::prepare_enrichr_result()`](https://pwwang.github.io/plotthis/reference/prepare_enrichr_result.html).
  The function auto-detects the format based on column names.

- top_term:

  Integer. Number of top terms (by significance) to display per
  group/facet combination. Default: `6` for all plot types except
  `"enrichmap"` which defaults to `100`. Note that terms are not
  filtered globally — the top terms are selected independently within
  each combination of `split_by`, `group_by`, and `facet_by` levels.

- plot_type:

  Character. The type of plot to generate. One of: `"bar"`, `"dot"`,
  `"lollipop"`, `"network"`, `"enrichmap"`, `"wordcloud"`,
  `"comparison"`, or `"heatmap"`. See the Description section for
  guidance on choosing a plot type. Default: `"bar"`.

- x_by:

  Character. Column name(s) to use for the x-axis. Works only for
  `"dot"` and `"lollipop"` plot types. Default: `NULL` (defaults to
  `"GeneRatio"` internally).

- size_by:

  Character. Column name(s) to map to point size. Works only for
  `"comparison"`, `"dot"`, and `"lollipop"` plot types. Default: `NULL`
  (defaults to `"GeneRatio"` for comparison, `"Count"` for dot and
  lollipop).

- fill_cutoff_name:

  Character. Legend label for terms that exceed the `cutoff` (shown in
  gray). Applies to `"comparison"`, `"dot"`, and `"lollipop"` plot
  types. Default: `NULL` (defaults to `"Non-significant"` when `cutoff`
  is set).

- fill_name:

  Character. Legend title for the fill color scale (the significance
  metric). Applies to `"comparison"`, `"dot"`, and `"lollipop"` plot
  types. Default: `NULL` (auto-generated as `"-log10(metric)"`).

- values_fill:

  Numeric. The fill value for missing entries in the heatmap matrix.
  Used only for `"heatmap"` plot type. Default: `0`.

- character_width:

  Integer. Maximum character width for term descriptions before
  line-wrapping. Applies to all plot types; for `"heatmap"` the wrapping
  is deferred to the Heatmap function. Default: `50`.

- expand:

  Numeric vector of length 1, 2, or 4. Axis expansion factors passed to
  [`plotthis::BarPlot()`](https://pwwang.github.io/plotthis/reference/barplot.html).
  Used only for `"bar"` plot type. Default: `NULL` (defaults to
  `c(0.1, 0.6, 0, 0.6)`).

- word_type:

  Character. What to display in the wordcloud. One of `"term"`
  (enrichment term descriptions) or `"feature"` (gene symbols from the
  enriched gene list). Used only for `"wordcloud"` plot type. Default:
  `"term"`.

- split_by:

  Character vector. Column name(s) in `data` to split the data and
  generate separate plots for each unique value. Multiple columns are
  concatenated with `split_by_sep`. Default: `NULL`.

- split_by_sep:

  Character. Separator used when concatenating multiple `split_by`
  columns. Default: `"_"`.

- facet_by:

  Character vector. Column name(s) in `data` to use for faceting
  (generating sub-panels within each plot). Default: `NULL`.

- facet_scales:

  Character. Facet scale behavior — `"fixed"` (same scales), `"free"`,
  `"free_x"`, or `"free_y"`. Default: `NULL` (defaults to `"free_y"` for
  bar, dot, lollipop, and comparison plots).

- group_by:

  Character vector. Column name(s) in `data` to group terms. Behavior
  depends on `plot_type`:

  - `"comparison"` — Groups are shown as x-axis categories in a dot plot
    comparing enrichment across groups. Required for this type.

  - `"heatmap"` — Groups are used as the columns of the heatmap (mapped
    to `columns_by` in
    [`plotthis::Heatmap()`](https://pwwang.github.io/plotthis/reference/Heatmap.html)).

  - All other types — `group_by` is not supported and will raise an
    error. Use `facet_by` or `split_by` instead.

  Multiple columns are concatenated with `group_by_sep`. Default:
  `NULL`.

- group_by_sep:

  Character. Separator used when concatenating multiple `group_by`
  columns. Used only for `"comparison"` plot type. Default: `"_"`.

- metric:

  Character. The column name in `data` to use as the significance metric
  for ordering and coloring terms. Common choices are `"p.adjust"`
  (default), `"pvalue"`, or `"qvalue"`. When the metric is a p-value
  column, a \\-log\_{10}\\ transformation is applied automatically so
  that more significant terms have higher values.

- cutoff:

  Numeric. A significance threshold to mark on the plot. Default: `NULL`
  (no marking). The behavior depends on `plot_type`:

  - `"bar"` — Adds a vertical dashed line at the transformed cutoff
    (e.g., \\-log\_{10}(0.05)\\).

  - `"dot"`, `"lollipop"`, `"comparison"` — Terms above the cutoff are
    colored gray with the legend label from `fill_cutoff_name`.

  - `"heatmap"` — Adds asterisk (`*`) labels to cells where the metric
    exceeds the cutoff.

  - `"network"`, `"enrichmap"`, `"wordcloud"` — No effect.

  This parameter only marks terms — it does not filter them. Use
  `top_term` to control how many terms are shown.

- palette:

  Character. Color palette name for the fill scale. See
  [`plotthis::show_palettes()`](https://pwwang.github.io/plotthis/reference/show_palettes.html)
  for available palettes. Default: `"Spectral"`.

- xlab:

  Character. Custom x-axis label. Default: `NULL` (auto-generated based
  on plot type and `x_by`/`metric`).

- ylab:

  Character. Custom y-axis label. Default: `NULL` (auto-generated based
  on plot type).

- ...:

  Additional arguments passed to the underlying plotthis plotting
  function, determined by `plot_type`:

  `"bar"`

  :   [`plotthis::BarPlot()`](https://pwwang.github.io/plotthis/reference/barplot.html)

  `"dot"`

  :   [`plotthis::DotPlot()`](https://pwwang.github.io/plotthis/reference/dotplot.html)

  `"lollipop"`

  :   [`plotthis::LollipopPlot()`](https://pwwang.github.io/plotthis/reference/dotplot.html)

  `"network"`

  :   [`plotthis::EnrichNetwork()`](https://pwwang.github.io/plotthis/reference/enrichmap1.html)

  `"enrichmap"`

  :   [`plotthis::EnrichMap()`](https://pwwang.github.io/plotthis/reference/enrichmap1.html)

  `"wordcloud"`

  :   [`plotthis::WordCloudPlot()`](https://pwwang.github.io/plotthis/reference/WordCloudPlot.html)

  `"comparison"`

  :   [`plotthis::DotPlot()`](https://pwwang.github.io/plotthis/reference/dotplot.html)

  `"heatmap"`

  :   [`plotthis::Heatmap()`](https://pwwang.github.io/plotthis/reference/Heatmap.html)

## Examples

``` r
if (FALSE) { # \dontrun{
  # See following links for examples
  # https://pwwang.github.io/enrichit/reference/EnrichIt.html
  # https://pwwang.github.io/enrichit/reference/ParseGMT.html
} # }
```
