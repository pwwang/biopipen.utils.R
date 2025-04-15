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
#' @importFrom dplyr mutate if_else select all_of
#' @importFrom tidyr pivot_longer
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
