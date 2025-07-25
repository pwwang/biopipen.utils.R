% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrna-macros.R
\name{RunSeuratSubClustering}
\alias{RunSeuratSubClustering}
\title{Run subset clustering on a Seurat object}
\usage{
RunSeuratSubClustering(
  object,
  subset,
  name = "subcluster",
  RunPCAArgs = list(),
  RunUMAPArgs = list(),
  FindNeighborsArgs = list(),
  FindClustersArgs = list(),
  log = NULL,
  cache = NULL
)
}
\arguments{
\item{object}{Seurat object}

\item{subset}{A string of expression to pass to \code{dplyr::filter} function to filter the cells.}

\item{name}{Name of the run.
It will be used as the prefix for the reduction name, keys and cluster names.
For reduction keys, it will be \verb{toupper(<name>)} + "PC_" and \verb{toupper(<name>)} + "UMAP_".
For cluster names, it will be \verb{<name>} + "." + resolution.
And the final cluster name will be \verb{<name>}.
Default is "subcluster".}

\item{RunPCAArgs}{Arguments to pass to \code{\link[Seurat:RunPCA]{Seurat::RunPCA()}}}

\item{RunUMAPArgs}{Arguments to pass to \code{\link[Seurat:RunUMAP]{Seurat::RunUMAP()}}.
\code{RunUMAPArgs$features} can be a character vector of features directly used for UMAP,
or a list with the following fields:
\itemize{
\item \code{order}: The order of the markers to use for UMAP, e.g. "desc(abs(avg_log2FC))"
\item \code{n}: The number of total features to use for UMAP, e.g. 30
If \code{RunUMAPArgs$features} is a list, it will run \code{\link[=RunSeuratDEAnalysis]{RunSeuratDEAnalysis()}} to get the markers
for each group, and then select the top \code{n}/\code{ngroups} features for each group
based on the \code{order} field.
}}

\item{FindNeighborsArgs}{Arguments to pass to \code{\link[Seurat:FindNeighbors]{Seurat::FindNeighbors()}}}

\item{FindClustersArgs}{Arguments to pass to \code{\link[Seurat:FindClusters]{Seurat::FindClusters()}}}

\item{log}{Logger}

\item{cache}{Directory to cache the results. Set to \code{FALSE} to disable caching}
}
\value{
The original Seurat object (not the subsetted one) with the subclusters
results in \verb{@meta.data} and \verb{@reductions}.
}
\description{
It's unlike \code{\link[Seurat:FindSubCluster]{Seurat::FindSubCluster}}, which only finds subclusters of a single
cluster. Instead, it will perform the whole clustering procedure on the subset of
cells. One can use metadata to specify the subset of cells to perform clustering on.
}
\examples{
\donttest{
obj <- SeuratObject::pbmc_small
# Just run UMAP to compare with the subclusters
obj <- suppressMessages(Seurat::RunUMAP(obj, dims = 1:10))
obj <- suppressWarnings(suppressMessages(RunSeuratSubClustering(
   obj, subset = "groups == 'g1'", name = "g1subcluster"
)))

scplotter::CellDimPlot(
    obj, reduction = "umap", group_by = "groups",
    title = "UMAP of all cells",
    subtitle = "groups = g1 will be subclustered"
)
scplotter::CellDimPlot(
    obj, reduction = "umap", group_by = "g1subcluster",
    title = "Subclusters on the original UMAP"
)
scplotter::CellDimPlot(obj,
    reduction = "g1subcluster.umap", group_by = "g1subcluster"
)
}
}
