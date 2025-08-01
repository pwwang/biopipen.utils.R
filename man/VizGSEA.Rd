% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gsea.R
\name{VizGSEA}
\alias{VizGSEA}
\title{Visualize GSEA results}
\usage{
VizGSEA(
  gsea_results,
  plot_type = c("summary", "gsea", "heatmap", "dot"),
  gene_ranks = "@gene_ranks",
  gene_sets = "@gene_sets",
  gs = NULL,
  group_by = NULL,
  values_by = "NES",
  signif_by = "padj",
  signif_cutoff = 0.05,
  ...
)
}
\arguments{
\item{gsea_results}{A data frame with the results of the fgsea analysis}

\item{plot_type}{The type of plot to create
One of "summary", "gsea"}

\item{gene_ranks}{A numeric vector of gene ranks with genes as names
The gene ranks are used to plot the gene sets.
If \code{gene_ranks} is a character vector starting with \code{@}, the gene ranks will be taken from the attribute of \code{data}.}

\item{gene_sets}{A list of gene sets, typically from a record of a GMT file
The names of the list should match the \code{ID} column of \code{data}.
If \code{gene_sets} is a character vector starting with \code{@}, the gene sets will be taken from the attribute of \code{data}.
The GSEA plots will be plotted for each gene set. So, the number of plots will be the number of gene sets.
If you only want to plot a subset of gene sets, you can subset the \code{gene_sets} before passing it to this function.}

\item{gs}{The names of the gene sets to plot
If \code{NULL}, all gene sets in \code{gene_sets} will be plotted.}

\item{group_by}{The column name to group by for heatmap and dot plot.
They will be used as the columns in the heatmap or dot plot.}

\item{values_by}{The column name to use for the values in the heatmap or dot plot.
Default is "NES" (normalized enrichment score).}

\item{signif_by}{The column name to use for significance in the heatmap or dot plot.
Default is "padj" (adjusted p-value). It can also be "pval".
If NULL, no significance labels will be added to the heatmap.}

\item{signif_cutoff}{A numeric vector of significance cutoffs for the heatmap labels.
Multiple values can be provided to indicate different levels (at most 3) of significance.
For example, \code{c(0.05, 0.01, 0.001)} will label pathways with p-values less than 0.05 with "\emph{",
less than 0.01 with "\strong{", and less than 0.001 with "}}".}

\item{...}{Additional arguments passed to the plotting function
\itemize{
\item When \code{plot_type} is "summary", they are passed to \code{\link[plotthis:gsea]{plotthis::GSEASummaryPlot()}}
\item When \code{plot_type} is "gsea", they are passed to \code{\link[plotthis:gsea]{plotthis::GSEAPlot()}}
}}
}
\value{
A ggplot object or a list of ggplot objects
}
\description{
Visualize GSEA results
}
\examples{
\donttest{
set.seed(123)
exprs <- matrix(rnorm(1000), nrow = 100, ncol = 10)
colnames(exprs) <- paste0("Sample", 1:10)
rownames(exprs) <- paste0("Gene", 1:100)
classes <- c(rep("A", 5), rep("B", 5))
ranks <- RunGSEAPreRank(exprs, case = "A", control = "B", classes = classes)
genesets <- list(
    set1 = c("Gene1", "Gene2", "Gene3"),
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
VizGSEA(r, plot_type = "gsea", gs = c("set10", "set2"))

r$Group <- "A"
r2 <- r
r2$Group <- "B"
r2$NES <- sample(r2$NES)
VizGSEA(rbind(r, r2), group_by = "Group", plot_type = "heatmap")
VizGSEA(rbind(r, r2), group_by = "Group", plot_type = "dot")
}
}
