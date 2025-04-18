% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrna-viz.R
\name{VizSeuratMap2Ref}
\alias{VizSeuratMap2Ref}
\title{Visualize features between the query and reference Seurat objects by \code{\link[=RunSeuratMap2Ref]{RunSeuratMap2Ref()}}}
\usage{
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
}
\arguments{
\item{query}{A Seurat object with query data}

\item{ref}{A Seurat object with reference data}

\item{features}{Features from the query and reference to visualize.
The format is 'query:ref' or 'feature', where 'feature' is the same in both query and reference.
The query and reference features must be the same type (numeric or factor/character).
All features must be in the same type.
A special case is to visualize the mapping score, for example \code{"seurat_clusters.score"}. If so,
only one feature can be provided, and the plot type must be "dim". The reference will be plotted
with the identity of the reference dataset.}

\item{split_by}{Column name in the query object to split the plot by, will not be supported.
The plot will be split by the query/reference features instead.}

\item{plot_type}{Type of plot to generate.
One of 'dim', 'violin', 'box', 'bar', 'ridge', 'heatmap' and 'dot'.
\itemize{
\item 'dim': Dimensionality reduction plot.
If the features are numeric, \code{\link[scplotter:FeatureStatPlot]{scplotter::FeatureStatPlot()}} will be used.
If the features are factor/character, \code{\link[scplotter:CellDimPlot]{scplotter::CellDimPlot()}} will be used.
\item other: \code{\link[scplotter:FeatureStatPlot]{scplotter::FeatureStatPlot()}} will be used.
}}

\item{reduction}{Dimensionality reduction to use for the plot.
If NULL, the default reduction will be used for both query and reference.
If the format is 'reduction_q:reduction_r', the first part will be used for the query and the second part for the reference.
If the format is 'reduction', the same reduction will be used for both query and reference.}

\item{ident}{Column name in the query and reference object to use for the plot.
If NULL, the default identity will be used for both query and reference.
If the format is 'ident_q:ident_r', the first part will be used for the query and the second part for the reference.
If the format is 'ident', the same identity will be used for both query and reference.}

\item{combine}{Whether to combine the plots into one plot.
If FALSE, the plots will be returned as a list.}

\item{nrow}{Number of rows to use for the combined plot.
If NULL, the number of rows will be calculated based on the number of features and ncol.
If ncol is NULL, the number of columns will be calculated based on the number of features and nrow.}

\item{ncol}{Number of columns to use for the combined plot.
If NULL, the number of columns will be calculated based on the number of features and nrow.
If nrow is NULL, the number of rows will be calculated based on the number of features and ncol.}

\item{byrow}{Whether to combine the plots by row or column.}

\item{axes}{Whether to show the axes for the combined plot.}

\item{axis_titles}{Whether to show the axis titles for the combined plot.}

\item{guides}{Whether to show the guides for the combined plot.}

\item{design}{Design for the combined plot.
See also \code{\link[patchwork:wrap_plots]{patchwork::wrap_plots()}}.}

\item{...}{Additional arguments to pass to the plot function}
}
\description{
Visualize features between the query and reference Seurat objects by \code{\link[=RunSeuratMap2Ref]{RunSeuratMap2Ref()}}
}
