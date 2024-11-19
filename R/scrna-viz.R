#' Visualize DEGs
#'
#' Visualize differentially expressed genes
#'
#' @param degs DEGs from RunDEAnalysis
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
#' degs <- RunDEAnalysis(SeuratObject::pbmc_small, "groups", "g1", "g2")
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
    stopifnot("[VizDEGs] Can only visualize object from RunDEAnalysis" = inherits(degs, "DEAnalysis"))
    stopifnot("[VizDEGs] 'outprefix' must be provided to save code" = !save_code || !is.null(outprefix))
    plot_type <- match.arg(plot_type)
    are_allmarkers <- !all(is.na(degs$group))

    if (plot_type %in% c("volcano_pct", "volcano_log2fc")) {
        if (save_code) {
            VolcanoPlot <- gglogger::register(scplotter::VolcanoPlot, "VolcanoPlot")
        } else {
            VolcanoPlot <- scplotter::VolcanoPlot
        }

        facet_by <- if (are_allmarkers) "group" else NULL
        args <- list(data = degs, x = ifelse(plot_type == "volcano_pct", "diff_pct", "avg_log2FC"),
            y = "p_val_adj", ylab = "-log10(p_val_adj)", facet_by = facet_by, ...)
        args$y_cutoff <- args$y_cutoff %||% -log10(0.05)
        args$y_cutoff_name <- paste0("p_val_adj = ", number(10 ^ -args$y_cutoff, accuracy = 0.01))
        p <- do_call(VolcanoPlot, args)
    } else {
        object <- attr(degs, "object")
        group.by <- attr(degs, "group.by")
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
                dplyr::group_by(!!sym("group")) %>%
                arrange(!!parse_expr(order_by)) %>%
                slice_head(n = genes) %>%
                pull("gene") %>%
                unique()
        } else {
            degs <- degs %>%
                dplyr::group_by(!!sym("group")) %>%
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
