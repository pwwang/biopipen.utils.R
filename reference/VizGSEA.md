# Visualize GSEA results

Visualize GSEA results

## Usage

``` r
VizGSEA(
  gsea_results,
  plot_type = c("summary", "gsea", "heatmap", "dot"),
  gene_ranks = "@gene_ranks",
  gene_sets = "@gene_sets",
  gs = NULL,
  group_by = NULL,
  values_by = "NES",
  signif_by = "p.adjust",
  signif_cutoff = 0.05,
  signif_only = TRUE,
  ...
)
```

## Arguments

- gsea_results:

  A data frame with the results of the fgsea analysis

- plot_type:

  The type of plot to create One of "summary", "gsea"

- gene_ranks:

  A numeric vector of gene ranks with genes as names The gene ranks are
  used to plot the gene sets. If `gene_ranks` is a character vector
  starting with `@`, the gene ranks will be taken from the attribute of
  `data`.

- gene_sets:

  A list of gene sets, typically from a record of a GMT file The names
  of the list should match the `ID` column of `data`. If `gene_sets` is
  a character vector starting with `@`, the gene sets will be taken from
  the attribute of `data`. The GSEA plots will be plotted for each gene
  set. So, the number of plots will be the number of gene sets. If you
  only want to plot a subset of gene sets, you can subset the
  `gene_sets` before passing it to this function.

- gs:

  The names of the gene sets to plot If `NULL`, all gene sets in
  `gene_sets` will be plotted.

- group_by:

  The column name to group by for heatmap and dot plot. They will be
  used as the columns in the heatmap or dot plot.

- values_by:

  The column name to use for the values in the heatmap or dot plot.
  Default is "NES" (normalized enrichment score).

- signif_by:

  The column name to use for significance in the heatmap or dot plot.
  Default is "p.adjust" (adjusted p-value). It can also be "pvalue". If
  NULL, no significance labels will be added to the heatmap.

- signif_cutoff:

  A numeric vector of significance cutoffs for the heatmap labels.
  Multiple values can be provided to indicate different levels (at
  most 3) of significance. For example, `c(0.05, 0.01, 0.001)` will
  label pathways with p-values less than 0.05 with "*", less than 0.01
  with "**", and less than 0.001 with "***".

- signif_only:

  If `TRUE`, only pathways that are significant in any group will be
  kept. The significance is determined by the `signif_by` column and
  `max(signif_cutoff)`. If `FALSE`, all pathways will be kept, but the
  significance labels will still be added to the heatmap.

- ...:

  Additional arguments passed to the plotting function

  - When `plot_type` is "summary", they are passed to
    [`plotthis::GSEASummaryPlot()`](https://pwwang.github.io/plotthis/reference/gsea.html)

  - When `plot_type` is "gsea", they are passed to
    [`plotthis::GSEAPlot()`](https://pwwang.github.io/plotthis/reference/gsea.html)

## Value

A ggplot object or a list of ggplot objects

## Examples

``` r
# \donttest{
set.seed(123)
exprs <- matrix(rnorm(1000, 0, 1), nrow = 100, ncol = 10)
colnames(exprs) <- paste0("Sample", 1:10)
rownames(exprs) <- paste0("Gene", 1:100)
classes <- sample(c("A", "B"), 10, replace = TRUE)
ranks <- RunGSEAPreRank(exprs, case = "A", control = "B", classes = classes)
genesets <- list(
    set1 = c("Gene73", "Gene30", "Gene97"),
    set2 = c("Gene4", "Gene5", "Gene6"),
    set3 = c("Gene7", "Gene8", "Gene9"),
    set4 = c("Gene10", "Gene11", "Gene12"),
    set5 = c("Gene13", "Gene14", "Gene15"),
    set6 = c("Gene16", "Gene17", "Gene18"),
    set7 = c("Gene19", "Gene20", "Gene21"),
    set8 = c("Gene22", "Gene23", "Gene24"),
    set9 = c("Gene25", "Gene26", "Gene27"),
    set10 = c("Gene12", "Gene86", "Gene87", "Gene83", "Gene71")
)
r <- RunGSEA(ranks, genesets)

# Visualize the GSEA results
VizGSEA(r, plot_type = "summary")
#>        log2err         ES        NES  size     ID Description       pvalue
#>          <num>      <num>      <num> <int> <fctr>      <char>        <num>
#>  1: 0.04811840  0.2371134  0.4333708     3   set2        set2 9.895616e-01
#>  2: 0.04939101 -0.3680773 -0.6393930     3   set4        set4 8.747628e-01
#>  3: 0.05676724  0.3739484  0.6834633     3   set6        set6 8.204593e-01
#>  4: 0.08063885 -0.5967110 -1.0365565     3   set7        set7 4.611006e-01
#>  5: 0.11331291  0.6492783  1.1866820     3   set9        set9 2.922756e-01
#>  6: 0.12325723  0.6694030  1.2234639     3   set5        set5 2.526096e-01
#>  7: 0.12625399 -0.7323092 -1.2721062     3   set3        set3 2.201139e-01
#>  8: 0.25296112 -0.8450015 -1.4678660     3   set8        set8 6.072106e-02
#>  9: 0.35248786 -0.7730952 -1.5490196     5  set10       set10 2.855028e-02
#> 10: 0.61052688  1.0000000  1.8276941     3   set1        set1 7.295427e-06
#>         p.adjust      core_enrichment
#>            <num>               <char>
#>  1: 9.895616e-01    Gene6/Gene4/Gene5
#>  2: 9.719587e-01               Gene10
#>  3: 9.719587e-01               Gene17
#>  4: 6.587151e-01        Gene19/Gene21
#>  5: 4.871260e-01               Gene25
#>  6: 4.871260e-01        Gene13/Gene15
#>  7: 4.871260e-01                Gene8
#>  8: 2.024035e-01        Gene24/Gene22
#>  9: 1.427514e-01 Gene86/Gene83/Gene71
#> 10: 7.295427e-05 Gene73/Gene30/Gene97

VizGSEA(r, plot_type = "gsea", gs = c("set10", "set2"))


r$Group <- "A"
r2 <- r
r2$Group <- "B"
r2$NES <- sample(r2$NES)
r2$padj <- sample(r2$padj * .1)
VizGSEA(rbind(r, r2), group_by = "Group", plot_type = "heatmap")

VizGSEA(rbind(r, r2), group_by = "Group", plot_type = "heatmap", signif_only = FALSE)

VizGSEA(rbind(r, r2), group_by = "Group", plot_type = "dot")

VizGSEA(rbind(r, r2), group_by = "Group", plot_type = "dot", signif_only = FALSE)

# }
```
