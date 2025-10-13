#' Visualize DEGs
#'
#' Visualize differentially expressed genes
#'
#' @inheritParams scplotter::MarkersPlot
#' @inheritDotParams scplotter::MarkersPlot
#' @param outprefix Prefix of the output file
#' @param devpars List of parameters to save the plot
#' @param more_formats Additional formats to save the plot in addition to 'png'
#' @param save_code Whether to save the code to reproduce the plot
#' @return A ggplot object if 'outprefix' is NULL, otherwise, save the plot to the output directory
#' @export
#' @importFrom rlang sym
#' @importFrom scales number
#' @importFrom dplyr slice_head arrange pull filter rename
#' @importFrom scplotter MarkersPlot
#' @seealso [scplotter::MarkersPlot()]
#' @examples
#' \donttest{
#' degs <- RunSeuratDEAnalysis(scplotter::pancreas_sub, "SubCellType")
#' VizDEGs(degs, plot_type = "volcano_pct")
#' VizDEGs(degs, plot_type = "volcano_log2fc")
#' VizDEGs(degs, plot_type = "jitter_log2fc", subset_by = "SubCellType")
#' VizDEGs(degs, plot_type = "heatmap_log2fc", cutoff = 0.05,
#'     select = 5, subset_by = "SubCellType")
#'
#' # Visualize expression of the top DEGs
#' # Suppose we did comparison between G2M and S phase in each SubCellType
#' degs$Phase <- "G2M:S"
#'
#' VizDEGs(degs, object = scplotter::pancreas_sub, plot_type = "violin",
#'     select = 2, comparison_by = "Phase", subset_by = "SubCellType")
#' VizDEGs(degs, object = scplotter::pancreas_sub, plot_type = "box",
#'     select = 2, comparison_by = "Phase", subset_by = "SubCellType")
#' VizDEGs(degs, object = scplotter::pancreas_sub, plot_type = "bar",
#'     select = 2, comparison_by = "Phase", subset_by = "SubCellType")
#' VizDEGs(degs, object = scplotter::pancreas_sub, plot_type = "ridge",
#'     select = 1, comparison_by = "Phase", subset_by = "SubCellType")
#' VizDEGs(degs, object = scplotter::pancreas_sub, plot_type = "heatmap",
#'     cluster_columns = FALSE, comparison_by = "Phase", subset_by = "SubCellType")
#' VizDEGs(degs, object = scplotter::pancreas_sub, plot_type = "dot",
#'     select = 1, comparison_by = "Phase", subset_by = "SubCellType")
#' }
VizDEGs <- function(
    markers,
    object = NULL,
    plot_type = c("volcano", "volcano_log2fc", "volcano_pct", "jitter", "jitter_log2fc",
        "jitter_pct", "heatmap_log2fc", "heatmap_pct", "dot_log2fc", "dot_pct", "heatmap",
        "violin", "box", "bar", "ridge", "dot"),
    subset_by = NULL,
    subset_as_facet = FALSE,
    comparison_by = NULL,
    p_adjust = TRUE,
    cutoff = NULL,
    order_by = NULL,
    select = ifelse(plot_type %in% c("volcano", "volcano_log2fc", "volcano_pct",
        "jitter", "jitter_log2fc", "jitter_pct"), 5, 10),
    outprefix = NULL,
    devpars = list(res = 100), more_formats = c(), save_code = FALSE,
    ...
) {
    # degs: p_val avg_log2FC pct.1 pct.2 p_val_adj gene group diff_pct
    stopifnot("[VizDEGs] 'outprefix' must be provided to save code" = !save_code || !is.null(outprefix))

    p <- MarkersPlot(
        markers = markers, object = object,
        plot_type = plot_type,
        subset_by = subset_by,
        subset_as_facet = subset_as_facet,
        comparison_by = comparison_by,
        p_adjust = p_adjust,
        cutoff = cutoff,
        order_by = order_by,
        select = select,
        ...
    )

    if (!is.null(outprefix)) {
        formats <- unique(c("png", more_formats))
        save_plot(p, outprefix, formats = formats, devpars = devpars)
        if (save_code) {
            args <- list(plot = p,
                setup = c("library(rlang)", "library(dplyr)", "library(gglogger)", "library(scplotter)", "load('data.RData')"),
                prefix = outprefix)
            args <- c(args, setdiff(ls(), c("p", "args", "formats", "devpars", "more_formats", "save_code", "outprefix")))
            do_call(save_plotcode, args)
        }
        return(NULL)
    } else {
        return(p)
    }
}

#' Visualize Cell QC metrics of Seurat object
#'
#' @param object A Seurat object with cell QC metrics
#' @param features Features to visualize
#' @param plot_type Type of plot to generate
#' One of 'violin', 'box', 'scatter', 'bar', 'ridge' and 'table'
#' If 'plot_type' is 'table', it will return a data frame with number of cells
#' before and after filtering
#' @param scatter_x Feature to use as x-axis in scatter plot.
#' If it is one of the features, it will be removed from the features
#' @param palette Color palette to use
#' @param ... Additional arguments to pass to the plot function
#' @return The plot
#' @export
#' @importFrom rlang %||% sym
#' @importFrom dplyr mutate if_else select all_of n
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom plotthis ViolinPlot BoxPlot ScatterPlot RidgePlot BarPlot
#' @examples
#' \donttest{
#' set.seed(8525)
#' sobj <- SeuratObject::pbmc_small
#' sobj$.QC <- sample(c(TRUE, FALSE), ncol(sobj), replace = TRUE)
#' sobj$Sample <- sample(c("Sample1", "Sample2"), ncol(sobj), replace = TRUE)
#' sobj$percent.mt <- runif(ncol(sobj), 0, 1)
#' sobj$percent.ribo <- runif(ncol(sobj), 0, 1)
#' sobj$percent.hb <- runif(ncol(sobj), 0, 1)
#' sobj$percent.plat <- runif(ncol(sobj), 0, 1)
#' sobj$nFeature_RNA <- as.integer(runif(ncol(sobj), 1000, 5000))
#' sobj$nCount_RNA <- as.integer(runif(ncol(sobj), 1000, 5000))
#'
#' # Visualize cell QC metrics
#' VizSeuratCellQC(sobj)
#' VizSeuratCellQC(sobj, plot_type = "scatter")
#' VizSeuratCellQC(sobj, plot_type = "bar")
#' VizSeuratCellQC(sobj, plot_type = "ridge")
#' VizSeuratCellQC(sobj, features = c("percent.mt", "percent.ribo"))
#' VizSeuratCellQC(sobj, features = c("percent.mt", "percent.ribo"), plot_type = "box")
#' VizSeuratCellQC(sobj, plot_type = "table")
#'
#' dim(sobj)
#' sobj <- FinishSeuratQC(sobj)
#' dim(sobj)
#' }
VizSeuratCellQC <- function(
    object,
    features = c("nFeature_RNA", "nCount_RNA", "percent.mt", "percent.ribo", "percent.hb", "percent.plat"),
    plot_type = c("violin", "box", "scatter", "bar", "ridge", "table"),
    scatter_x = "nCount_RNA", palette = "Set1",
    ...
) {
    stopifnot("[VizSeuratQC] 'object' must be a Seurat object" = inherits(object, "Seurat"))
    stopifnot("[VizSeuratQC] 'object' must be generated by [LoadSeuratAndPerformQC] or [PerformSeuratCellQC]" = ".QC" %in% colnames(object@meta.data))
    if (!all(features %in% colnames(object@meta.data))) {
        stop(paste0("[VizSeuratQC] 'features' must be columns in 'object@meta.data'. Not found: ", paste(setdiff(features, colnames(object@meta.data)), collapse = ", ")))
    }
    plot_type <- match.arg(plot_type)

    if (plot_type == "scatter") {
        features <- setdiff(features, scatter_x)
    }
    df <- object@meta.data %>%
        select(all_of(unique(c("Sample", ".QC", features, scatter_x)))) %>%
        pivot_longer(cols = features, names_to = "feature", values_to = "value") %>%
        mutate(QC = factor(if_else(!!sym(".QC"), "passed", "failed"), levels = c("failed", "passed")))

    if (plot_type == "scatter") {
        ScatterPlot(
            df, x = scatter_x, y = "value", color_by = "QC",
            facet_by = "feature", facet_scales = "free_y", palette = palette, ...
        )
    } else if (plot_type == "violin" || plot_type == "box") {
        if (length(unique(df$Sample)) == 1 && df$Sample[1] == "SeuratProject") {
            df$Sample = ""
        }
        if (plot_type == "violin") {
            ViolinPlot(df, x = "Sample", y = "value", palette = palette, add_point = TRUE,
                add_box = TRUE, facet_by = "feature", facet_scales = "free_y", group_by = "QC", ...
            )
        } else {
            BoxPlot(df, x = "Sample", y = "value", palette = palette, add_point = TRUE,
                facet_by = "feature", facet_scales = "free_y", group_by = "QC", ...
            )
        }
    } else if (plot_type == "bar") {
        if (length(unique(df$Sample)) == 1 && df$Sample[1] == "SeuratProject") {
            df$Sample = ""
        }
        args <- list(...)
        args$position <- args$position %||% ifelse(length(unique(df$Sample)) == 1, "dodge", "stack")
        args$ylab <- args$ylab %||% "Number of Cells"
        args$data <- df
        args$x <- "Sample"
        args$group_by <- "QC"
        args$facet_scales <- "free_y"
        args$palette <- palette
        do_call(BarPlot, args)
    } else if (plot_type == "ridge") {
        RidgePlot(df, x = "value", group_by = "QC", facet_by = "feature", palette = palette,
            facet_scales = "free_x", ...)
    } else {  # table
        df <- object@meta.data %>%
            select(!!sym("Sample"), QC = !!sym(".QC")) %>%
            mutate(QC = factor(if_else(!!sym("QC"), "passed", "failed"), levels = c("failed", "passed"))) %>%
            group_by(!!sym("Sample"), !!sym("QC")) %>%
            summarise(Cells = n(), .groups = "drop") %>%
            pivot_wider(names_from = "QC", values_from = "Cells", values_fill = 0)
        df$passed <- df$passed %||% 0
        df$failed <- df$failed %||% 0
        df$total <- df$passed + df$failed
        df <- rbind(
            df,
            data.frame(Sample = "All_Samples", passed = sum(df$passed), failed = sum(df$failed), total = sum(df$total))
        )
        df$passed <- as.integer(df$passed)
        df$failed <- as.integer(df$failed)
        df$total <- as.integer(df$total)
        df$Sample <- factor(df$Sample, levels = c(unique(df$Sample[df$Sample != "All_Samples"]), "All_Samples"))
        df <- df[order(df$Sample), , drop = FALSE]
        df
    }
}

#' Visualize gene QC metrics of Seurat object
#'
#' @param object A Seurat object with gene QC metrics
#' @param plot_type Type of plot to generate
#' One of 'histogram', 'box', 'violin', 'ridge' and 'table'
#' If 'plot_type' is 'table', it will return a data frame with number of genes
#' before and after filtering
#' @param palette Color palette to use
#' @param ylab Y-axis label
#' When plot_type is ridge, it will be used as x-axis label
#' @param ... Additional arguments to pass to the plot function [plotthis::BarPlot()]
#' @return The plot
#' @export
#' @importFrom plotthis Histogram BoxPlot ViolinPlot RidgePlot
#' @examples
#' \donttest{
#' set.seed(8525)
#' sobj <- SeuratObject::pbmc_small
#' sobj@misc$gene_qc <- data.frame(
#'   Sample = rep(c("Sample1", "Sample2"), each = nrow(sobj)),
#'   Feature = rep(rownames(sobj), 2),
#'   Count = as.integer(runif(nrow(sobj) * 2, 0, 100))
#' )
#' sobj@misc$gene_qc$QC <- (
#'   sobj@misc$gene_qc$Count >= 20 |
#'      sample(c(TRUE, FALSE), nrow(sobj) * 2, prob = c(0.8, 0.2), replace = TRUE)
#' )
#'
#' # Visualize gene QC metrics
#' VizSeuratGeneQC(sobj)
#' VizSeuratGeneQC(sobj, plot_type = "box")
#' VizSeuratGeneQC(sobj, plot_type = "violin")
#' VizSeuratGeneQC(sobj, plot_type = "ridge")
#' VizSeuratGeneQC(sobj, plot_type = "table")
#' }
VizSeuratGeneQC <- function(
    object, plot_type = c("histogram", "box", "violin", "ridge", "table"),
    palette = "Set1", ylab = "Number of cells a gene is expressed in", ...) {
    stopifnot("[VizSeuratQC] 'object' must be a Seurat object" = inherits(object, "Seurat"))
    stopifnot("[VizSeuratQC] 'object' is not generated by [LoadSeuratAndPerformQC] or no genes were filtered out" = is.data.frame(object@misc$gene_qc))

    object@misc$gene_qc$QC <- factor(ifelse(object@misc$gene_qc$QC, "passed", "failed"), levels = c("failed", "passed"))
    plot_type <- match.arg(plot_type)
    if (plot_type == "histogram") {
        Histogram(object@misc$gene_qc, x = "Count", group_by = "QC", facet_by = "Sample",
            palette = palette, ylab = ylab, ...)
    } else if (plot_type == "box") {
        BoxPlot(object@misc$gene_qc, x = "Sample", y = "Count", group_by = "QC",
            palette = palette, ylab = ylab, ...)
    } else if (plot_type == "violin") {
        ViolinPlot(object@misc$gene_qc, x = "Sample", y = "Count", group_by = "QC",
            palette = palette, ylab = ylab, ...)
    } else if (plot_type == "ridge") {
        RidgePlot(object@misc$gene_qc, x = "Count", group_by = "QC", facet_by = "Sample",
            palette = palette, xlab = ylab, ylab = "Number of features", ...)
    } else {
        df <- object@misc$gene_qc %>%
            group_by(!!sym("Sample"), !!sym("QC")) %>%
            summarise(Genes = n(), .groups = "drop") %>%
            pivot_wider(names_from = "QC", values_from = "Genes", values_fill = 0)
        df$passed <- df$passed %||% 0
        df$failed <- df$failed %||% 0
        df$total <- df$passed + df$failed
        all_features <- unique(object@misc$gene_qc$Feature)
        failed_features <- object@misc$gene_qc %>%
            filter(!!sym("QC") == "failed") %>%
            pull("Feature") %>%
            unique()

        df <- rbind(df, data.frame(
            Sample = "Final_Features",
            passed = length(all_features) - length(failed_features),
            failed = length(failed_features),
            total = length(all_features)
        ))
        df$passed <- as.integer(df$passed)
        df$failed <- as.integer(df$failed)
        df$total <- as.integer(df$total)
        df$Sample <- factor(df$Sample, levels = c(unique(df$Sample[df$Sample != "Final_Features"]), "Final_Features"))
        df <- df[order(df$Sample), , drop = FALSE]
        df
    }
}

#' Visualize detected doublets
#'
#' @param object A Seurat object with detected doublets
#' @param plot_type Type of plot to generate
#' One of 'dim', 'pie', 'pk', 'pK'.
#' dim/pie show the distribution of doublets by droplet type.
#' pk/pK show the relationship between BC metric and pK when using DoubletFinder.
#' @param palette Color palette to use
#' @param ... Additional arguments to pass to the plot function
#' * For 'dim',  additional arguments to pass to [scplotter::CellDimPlot]
#' * For 'pie',  additional arguments to pass to [plotthis::PieChart]
#' * For 'pk' or 'pK', additional arguments to pass to [plotthis::LinePlot]
#' @return The plot
#' @export
#' @importFrom plotthis PieChart LinePlot
#' @importFrom scplotter CellDimPlot
#' @examples
#' \donttest{
#' datadir <- system.file("extdata", "scrna", package = "biopipen.utils")
#' meta <- data.frame(
#'     Sample = c("Sample1", "Sample2"),
#'     RNAData = c(
#'         file.path(datadir, "Sample1"),
#'         file.path(datadir, "Sample2")
#'     )
#' )
#'
#' obj <- LoadSeuratAndPerformQC(meta, cache = FALSE, gene_qc = list(min_cells = 3),
#'     cell_qc = "nFeature_RNA > 500")
#' VizSeuratCellQC(obj)
#' VizSeuratCellQC(obj, plot_type = "scatter")
#' VizSeuratCellQC(obj, plot_type = "ridge")
#' VizSeuratGeneQC(obj)
#'
#' obj <- RunSeuratTransformation(obj)
#' obj <- RunSeuratIntegration(obj)
#' obj <- RunSeuratDoubletDetection(obj, tool = "scDblFinder", filter = FALSE)
#' VizSeuratDoublets(obj)
#' VizSeuratDoublets(obj, plot_type = "pie")
#' }
VizSeuratDoublets <- function(object, plot_type = c("dim", "pie", "pk", "pK"), palette = "Set2", ...) {
    stopifnot("[VizSeuratDoublets] 'object' must be a Seurat object" = inherits(object, "Seurat"))
    stopifnot("[VizSeuratDoublets] 'object' must be generated by [RunSeuratDoubletDetection]" = is.list(object@misc$doublets))
    stopifnot("[VizSeuratDoublets] 'filter' must be FALSE when running [RunSeuratDoubletDetection]" = !object@misc$doublets$filter)
    plot_type <- match.arg(plot_type)

    col <- paste0(object@misc$doublets$tool, "_DropletType")
    if (plot_type == "dim") {
        CellDimPlot(object, group_by = col, palette = palette, ...)
    } else if (plot_type == "pie") {
        PieChart(object@meta.data, x = col, palette = palette, label = ".y", ...)
    } else if (plot_type == "pk" || plot_type == "pK") {
        stopifnot("[VizSeuratDoublets] 'pk' or 'pK' plot is only available for 'DoubletFinder'" = object@misc$doublets$tool == "DoubletFinder")

        LinePlot(
            data = object@misc$doublets$bcmvn,
            fill_point_by_x_if_no_group = FALSE, color_line_by_x_if_no_group = FALSE,
            x = "pK", y = "BCmetric", highlight = "Selected", ...
        )
    }
}


#' Visualize features between the query and reference Seurat objects by [biopipen.utils::RunSeuratMap2Ref()]
#'
#' @param query A Seurat object with query data
#' @param ref A Seurat object with reference data
#' @param features Features from the query and reference to visualize.
#' The format is 'query:ref' or 'feature', where 'feature' is the same in both query and reference.
#' The query and reference features must be the same type (numeric or factor/character).
#' All features must be in the same type.
#' A special case is to visualize the mapping score, for example `"seurat_clusters.score"`. If so,
#' only one feature can be provided, and the plot type must be "dim". The reference will be plotted
#' with the identity of the reference dataset.
#' @param split_by Column name in the query object to split the plot by, will not be supported.
#' The plot will be split by the query/reference features instead.
#' @param plot_type Type of plot to generate.
#' One of 'dim', 'violin', 'box', 'bar', 'ridge', 'heatmap' and 'dot'.
#' * 'dim': Dimensionality reduction plot.
#'   If the features are numeric, [scplotter::FeatureStatPlot()] will be used.
#'   If the features are factor/character, [scplotter::CellDimPlot()] will be used.
#' * other: [scplotter::FeatureStatPlot()] will be used.
#' @param reduction Dimensionality reduction to use for the plot.
#' If NULL, the default reduction will be used for both query and reference.
#' If the format is 'reduction_q:reduction_r', the first part will be used for the query and the second part for the reference.
#' If the format is 'reduction', the same reduction will be used for both query and reference.
#' @param ident Column name in the query and reference object to use for the plot.
#' If NULL, the default identity will be used for both query and reference.
#' If the format is 'ident_q:ident_r', the first part will be used for the query and the second part for the reference.
#' If the format is 'ident', the same identity will be used for both query and reference.
#' @param combine Whether to combine the plots into one plot.
#' If FALSE, the plots will be returned as a list.
#' @param nrow Number of rows to use for the combined plot.
#' If NULL, the number of rows will be calculated based on the number of features and ncol.
#' If ncol is NULL, the number of columns will be calculated based on the number of features and nrow.
#' @param ncol Number of columns to use for the combined plot.
#' If NULL, the number of columns will be calculated based on the number of features and nrow.
#' If nrow is NULL, the number of rows will be calculated based on the number of features and ncol.
#' @param byrow Whether to combine the plots by row or column.
#' @param axes Whether to show the axes for the combined plot.
#' @param axis_titles Whether to show the axis titles for the combined plot.
#' @param guides Whether to show the guides for the combined plot.
#' @param design Design for the combined plot.
#' See also [patchwork::wrap_plots()].
#' @param ... Additional arguments to pass to the plot function
#'
#' @export
#' @importFrom SeuratObject DefaultDimReduc Idents
#' @importFrom plotthis palette_this
#' @importFrom scplotter CellDimPlot FeatureStatPlot
VizSeuratMap2Ref <- function(
    query, ref, features,
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
    ...) {
    stopifnot("[VizSeuratMap2Ref] 'query' and 'ref' must be Seurat objects" = inherits(query, "Seurat") && inherits(ref, "Seurat"))
    stopifnot("[VizSeuratMap2Ref] 'features' must be a character vector" = is.character(features))
    stopifnot("[VizSeuratMap2Ref] 'split_by' is not supported" = is.null(split_by))

    plot_type <- match.arg(plot_type)
    feat_types <- c()
    features_q <- c()
    features_r <- c()
    is_mappingscore_case <- FALSE
    for (feat in features) {
        feat_parts <- strsplit(feat, ":")[[1]]
        if (length(feat_parts) > 2) {
            stop("[VizSeuratMap2Ref] 'features' must be in the format 'query:ref' or 'query', got: ", feat)
        }

        feat_q <- feat_parts[1]
        feat_r <- ifelse(length(feat_parts) > 1, feat_parts[2], feat_q)
        features_q <- c(features_q, feat_q)
        features_r <- c(features_r, feat_r)

        if (endsWith(feat_q, ".score") && substr(feat_q, 1, nchar(feat_q) - 6) %in% colnames(query@meta.data) && feat_q == feat_r) {
            # Plotting mapping score is a special case
            # We need to use the ident for reference, and the score for query
            if (length(features) > 1) {
                stop("[VizSeuratMap2Ref] 'features' must be a single feature when plotting mapping score")
            }
            if (plot_type != "dim") {
                stop("[VizSeuratMap2Ref] 'plot_type' must be 'dim' when plotting mapping score")
            }
            feat_types <- c(feat_types, "mapping_score")
            is_mappingscore_case <- TRUE
        } else {

            if (is.null(query[[feat_q]])) {
                stop(paste0("[VizSeuratMap2Ref] 'query' does not contain feature: ", feat_q))
            }
            if (is.null(ref[[feat_r]])) {
                stop(paste0("[VizSeuratMap2Ref] 'ref' does not contain feature: ", feat_r))
            }

            query_feat <- if (is.data.frame(query[[feat_q]])) { query[[feat_q]][, 1] } else { query[[feat_q]] }
            ref_feat <- if (is.data.frame(ref[[feat_r]])) { ref[[feat_r]][, 1] } else { ref[[feat_r]] }

            if (is.numeric(query_feat) != is.numeric(ref_feat)) {
                stop(paste0("[VizSeuratMap2Ref] 'query' and 'ref' must have the same type for feature: ", feat_q, ":", feat_r, ".\n",
                    "- is.numeric(query$", feat_q, "): ", is.numeric(query_feat), "\n",
                    "- is.numeric(ref$", feat_r, "): ", is.numeric(ref_feat)))
            } else if (is.numeric(query_feat)) {
                feat_types <- c(feat_types, "numeric")
            } else {
                feat_types <- c(feat_types, "factor")
                query_levels <- if (is.factor(query_feat)) levels(query_feat) else unique(query_feat)
                ref_levels <- if (is.factor(ref_feat)) levels(ref_feat) else unique(ref_feat)
                all_levels <- unique(c(query_levels, ref_levels))
                query[[feat_q]] <- factor(query_feat, levels = all_levels)
                ref[[feat_r]] <- factor(ref_feat, levels = all_levels)
            }
        }
    }

    if (length(unique(feat_types)) > 1) {
        stop("[VizSeuratMap2Ref] 'query' and 'ref' should be either 'numeric' or 'character/factor' for all features, \n",
            "we got: ", paste0(feat_types, collapse = ", "))
    }

    if (feat_types[1] == "factor" && plot_type != "dim") {
        stop("[VizSeuratMap2Ref] 'plot_type' must be 'dim' when features are charactors/factors")
    }

    if (is.null(reduction)) {
        reduction_q <- DefaultDimReduc(query)
        reduction_r <- DefaultDimReduc(ref)
    } else if (grepl(":", reduction)) {
        reduction_q <- strsplit(reduction, ":")[[1]][1]
        reduction_r <- strsplit(reduction, ":")[[1]][2]
    } else {
        reduction_q <- reduction_r <- reduction
    }

    combine_plots <- getFromNamespace("combine_plots", "plotthis")
    if (isTRUE(is_mappingscore_case)) {
        ident_r <- NULL
        for (name in colnames(ref@meta.data)) {
            if (!is.factor(ref@meta.data[[name]])) next
            if (all(as.character(Idents(ref)) == as.character(ref@meta.data[[name]]))) {
                ident_r <- name
                break
            }
        }
        if (is.null(ident_r)) {
            ref@meta.data$Identity <- Idents(ref)
            ident_r <- "Identity"
        }
        p_q <- FeatureStatPlot(
            query, features = features_q,
            plot_type = plot_type,
            ident = ident_r,
            reduction = reduction_q,
            title = paste0("Query Dataset: ", features_q),
            ...
        )
        p_r <- CellDimPlot(
            ref, group_by = ident_r,
            reduction = reduction_r,
            title = "Reference Dataset",
            label = TRUE,
            ...
        )
        combine_plots(list(p_q, p_r), combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
            axes = axes, axis_titles = axis_titles,
            guides = guides, design = design)
    } else if (plot_type == "dim" && feat_types[1] != "numeric") {
        p_q <- lapply(features_q, function(feat) {
            CellDimPlot(
                query, group_by = feat,
                reduction = reduction_q,
                title = paste0("Query: ", feat),
                ...)
        })
        p_r <- lapply(features_r, function(feat) {
            CellDimPlot(
                ref, group_by = feat,
                reduction = reduction_r,
                title = paste0("Reference: ", feat),
                ...)
        })
        combine_plots(c(p_q, p_r), combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
            axes = axes, axis_titles = axis_titles,
            guides = guides, design = design)
    } else {
        if (is.null(ident)) {
            if (plot_type != "dim") {
                # Find the ident column in meta.data
                ident_q <- GetIdentityColumn(query)
                ident_r <- GetIdentityColumn(ref)
                if (is.null(ident_q)) {
                    query@meta.data$Identity <- Idents(query)
                    ident_q <- "Identity"
                }
                if (is.null(ident_r)) {
                    ref@meta.data$Identity <- Idents(ref)
                    ident_r <- "Identity"
                }
            } else {
                # We don't need ident for feature dim plot
                ident_q <- ident_r <- NULL
            }
        } else if (grepl(":", ident)) {
            ident_q <- strsplit(ident, ":")[[1]][1]
            ident_r <- strsplit(ident, ":")[[1]][2]
        } else {
            ident_q <- ident_r <- ident
        }
        p_q <- FeatureStatPlot(
            query, features = features_q,
            plot_type = plot_type,
            ident = ident_q,
            reduction = reduction_q,
            title = if (length(features_q) == 1) paste0("Query Dataset: ", features_q) else "Query Dataset",
            ...
        )
        p_r <- FeatureStatPlot(
            ref, features = features_r,
            plot_type = plot_type,
            ident = ident_r,
            reduction = reduction_r,
            title = if (length(features_r) == 1) paste0("Reference Dataset: ", features_r) else "Reference Dataset",
            ...
        )
        combine_plots(list(p_q, p_r), combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
            axes = axes, axis_titles = axis_titles,
            guides = guides, design = design)
    }
}
