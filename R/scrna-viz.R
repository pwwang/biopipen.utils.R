#' Visualize DEGs
#'
#' Visualize differentially expressed genes
#'
#' @param degs DEGs from RunSeuratDEAnalysis
#' @param plot_type Type of plot to generate
#' One of 'volcano_pct', 'volcano_log2fc', 'violin', 'box', 'bar', 'ridge', 'dim', 'heatmap', 'dot'
#' @param order_by An expression in string to order the genes
#' @param genes Number of genes genes to visualize (based on the 'order_by' expression)
#' Or an expression in string to filter the genes (passed by [dplyr::filter])
#' Only works when plot_type is not a volcano plot
#' @param outprefix Prefix of the output file
#' @param devpars List of parameters to save the plot
#' @param more_formats Additional formats to save the plot in addition to 'png'
#' @param save_code Whether to save the code to reproduce the plot
#' @param show_row_names Whether to show row names in the heatmap
#' @param show_column_names Whether to show column names in the heatmap
#' @param ... Additional arguments to pass to the plot function
#' * For 'volcano_pct' and 'volcano_log2fc', additional arguments to pass to 'scplotter::VolcanoPlot'
#' * For 'violin', 'box', 'bar', 'ridge', 'dim', 'heatmap', 'dot', additional arguments to pass to 'scplotter::FeatureStatPlot'
#' @return A ggplot object if 'outprefix' is NULL, otherwise, save the plot to the output directory
#' @export
#' @importFrom rlang sym
#' @importFrom scales number
#' @importFrom dplyr slice_head arrange pull filter
#' @examples
#' \donttest{
#' degs <- suppressWarnings(RunSeuratDEAnalysis(SeuratObject::pbmc_small, "groups", "g1", "g2"))
#' VizDEGs(degs, plot_type = "volcano_pct")
#' VizDEGs(degs, plot_type = "volcano_log2fc")
#' VizDEGs(degs, plot_type = "violin")
#' VizDEGs(degs, plot_type = "box")
#' VizDEGs(degs, plot_type = "bar")
#' VizDEGs(degs, plot_type = "ridge")
#' VizDEGs(degs, plot_type = "dim")
#' # VizDEGs(degs, plot_type = "heatmap")
#' # VizDEGs(degs, plot_type = "dot")
#' }
VizDEGs <- function(
    degs, plot_type = c("volcano_pct", "volcano_log2fc", "violin", "box", "bar", "ridge", "dim", "heatmap", "dot"),
    order_by = 'desc(abs(avg_log2FC))', genes = 10, outprefix = NULL,
    devpars = list(res = 100), more_formats = c(), save_code = FALSE,
    show_row_names = TRUE, show_column_names = TRUE, ...
) {
    # degs: p_val avg_log2FC pct.1 pct.2 p_val_adj gene group diff_pct
    stopifnot("[VizDEGs] Can only visualize object from RunSeuratDEAnalysis" = inherits(degs, "SeuratDEAnalysis"))
    stopifnot("[VizDEGs] 'outprefix' must be provided to save code" = !save_code || !is.null(outprefix))
    group.by <- attr(degs, "group.by")
    plot_type <- match.arg(plot_type)
    are_allmarkers <- !all(is.na(degs[[group.by]]))

    if (plot_type %in% c("volcano_pct", "volcano_log2fc")) {
        if (save_code) {
            VolcanoPlot <- gglogger::register(scplotter::VolcanoPlot, "VolcanoPlot")
        } else {
            VolcanoPlot <- scplotter::VolcanoPlot
        }

        facet_by <- if (are_allmarkers) group.by else NULL
        args <- list(data = degs, x = ifelse(plot_type == "volcano_pct", "diff_pct", "avg_log2FC"),
            y = "p_val_adj", ylab = "-log10(p_val_adj)", facet_by = facet_by, ...)
        args$y_cutoff <- args$y_cutoff %||% -log10(0.05)
        args$y_cutoff_name <- paste0("p_val_adj = ", number(10 ^ -args$y_cutoff, accuracy = 0.01))
        p <- do_call(VolcanoPlot, args)
    } else {
        object <- attr(degs, "object")
        ident.1 <- attr(degs, "ident.1")
        ident.2 <- attr(degs, "ident.2")
        if (!is.null(ident.1)) {
            if (!is.null(ident.2)) {
                object <- filter(object, !!sym(group.by) %in% c(ident.1, ident.2))
            } else {
                all_idents <- as.character(unique(object@meta.data[[group.by]]))
                ident.2 <- setdiff(all_idents, ident.1)
                if (length(ident.2) != 1) {
                    ident.2 <- ifelse(ident.1 == "Others", "Others_1", "Others")
                }
                object@meta.data[[group.by]][object@meta.data[[group.by]] != ident.1] <- ident.2
            }
            object@meta.data[[group.by]] <- factor(object@meta.data[[group.by]], levels = c(ident.1, ident.2))
        }

        if (save_code) {
            FeatureStatPlot <- gglogger::register(scplotter::FeatureStatPlot, "FeatureStatPlot")
        } else {
            FeatureStatPlot <- scplotter::FeatureStatPlot
        }

        if (is.numeric(genes)) {
            degs <- degs %>%
                dplyr::group_by(!!sym(group.by)) %>%
                arrange(!!parse_expr(order_by)) %>%
                slice_head(n = genes) %>%
                pull("gene") %>%
                unique()
        } else {
            degs <- degs %>%
                dplyr::group_by(!!sym(group.by)) %>%
                arrange(!!parse_expr(order_by)) %>%
                filter(!!parse_expr(genes)) %>%
                pull("gene") %>%
                unique()
        }

        if (plot_type == "dim") {
            p <- FeatureStatPlot(object, features = degs, plot_type = plot_type,
                facet_by = group.by, split_by = TRUE, ...)
        } else if (plot_type == "heatmap") {
            p <- FeatureStatPlot(object, features = degs, plot_type = plot_type,
                ident = group.by, show_row_names = show_row_names, show_column_names = show_column_names,
                name = "Expression Level", ...)
        } else {
            p <- FeatureStatPlot(object, features = degs, plot_type = plot_type,
                ident = group.by, ...)
        }
    }

    if (!is.null(outprefix)) {
        formats <- unique(c("png", more_formats))
        save_plot(p, outprefix, formats = formats, devpars = devpars)
        if (save_code) {
            args <- list(plot = p,
                setup = c("library(rlang)", "library(dplyr)", "library(gglogger)", "library(scplotter)", "load('data.RData')"),
                prefix = outprefix)
            args <- c(args, setdiff(ls(), c("p", "args", "formats", "devpars", "more_formats", "save_code", "outprefix", "are_allmarkers")))
            do.call(save_plotcode, args)
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
#' One of 'violin', 'box', 'scatter', 'ridge'
#' @param scatter_x Feature to use as x-axis in scatter plot.
#' If it is one of the features, it will be removed from the features
#' @param palette Color palette to use
#' @param ... Additional arguments to pass to the plot function
#' @return The plot
#' @export
#' @importFrom dplyr mutate if_else
#' @importFrom tidyr pivot_longer
#' @importFrom plotthis ViolinPlot BoxPlot ScatterPlot RidgePlot
VizSeuratCellQC <- function(
    object,
    features = c("nFeature_RNA", "nCount_RNA", "percent.mt", "percent.ribo", "percent.hb", "percent.plat"),
    plot_type = c("violin", "box", "scatter", "ridge"),
    scatter_x = "nCount_RNA", palette = "Set2",
    ...
) {
    stopifnot("[VizSeuratQC] 'object' must be a Seurat object" = inherits(object, "Seurat"))
    stopifnot("[VizSeuratQC] 'object' must be generated by [LoadSeuratAndPerformQC] or [PerformSeuratCellQC]" = is.data.frame(object@misc$cell_qc_df))
    if (!all(features %in% colnames(object@misc$cell_qc_df))) {
        stop(paste0("[VizSeuratQC] 'features' must be columns in 'object@misc$cell_qc_df'. Not found: ", paste(setdiff(features, colnames(object@misc$cell_qc_df)), collapse = ", ")))
    }
    plot_type <- match.arg(plot_type)

    if (plot_type == "scatter") {
        features <- setdiff(features, scatter_x)
    }
    df <- object@misc$cell_qc_df %>%
        pivot_longer(cols = features, names_to = "feature", values_to = "value") %>%
        mutate(QC = factor(if_else(!!sym(".QC"), "passed", "failed"), levels = c("passed", "failed")))

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
    } else {  # ridge
        RidgePlot(df, x = "value", group_by = "QC", facet_by = "feature", palette = palette,
            ...)
    }
}

#' Visualize gene QC metrics of Seurat object
#'
#' @param object A Seurat object with gene QC metrics
#' @param features Number of features/actual features to visualize
#' If actual features are given, they must be the features excluded in gene QC
#' @param ... Additional arguments to pass to the plot function [plotthis::BarPlot]
#' @return The plot
#' @export
#' @importFrom plotthis BarPlot
VizSeuratGeneQC <- function(object, features = 10, ...) {
    stopifnot("[VizSeuratQC] 'object' must be a Seurat object" = inherits(object, "Seurat"))
    stopifnot("[VizSeuratQC] 'object' must be generated by [LoadSeuratAndPerformQC]" = is.list(object@misc$gene_qc))
    stopifnot("[VizSeuratQC] No genes were filtered out, nothing to visualize" = nrow(object@misc$gene_qc$ncells) > 0)

    feats <- unique(object@misc$gene_qc$ncells$feature)
    if (is.numeric(features)) {
        features <- feats[1:features]
    } else {
        stopifnot("[VizSeuratGeneQC] 'features' must be the features excluded in gene QC" = all(features %in% feats))
    }
    # Sample = character(), feature = character(), ncells = numeric()
    df <- object@misc$gene_qc$ncells[object@misc$gene_qc$ncells$feature %in% features, , drop = FALSE]

    BarPlot(df, x = "Sample", y = "ncells", facet_by = "feature", ...)
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
