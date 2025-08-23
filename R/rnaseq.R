#' Run differential gene expression analysis
#'
#' @param exprs Expression matrix with genes as rows and samples as columns.
#' @param meta Metadata data frame with sample information. If a `Sample` column is present, it will be used as the sample identifier,
#' which is the column name in `exprs`.
#' @param group_by Column name in `meta` to group samples for differential expression analysis
#' @param ident_1 First identity to compare against
#' @param ident_2 Second identity to compare against
#' If not specified, the rest of the samples will be used as the second identity.
#' @param paired_by Column name in `meta` for paired samples.
#' For example, `Subject`, for each subject, there should be only one sample in each group (`ident_1` and `ident_2`).
#' @param tool Tool to use for differential expression analysis.
#' Currently supports "DESeq2", "edgeR", and "limma".
#' @param log Logger
#' @param ncores Number of cores to use for parallel processing.
#' If set to 1, the analysis will run in single-threaded mode.
#' If set to a value greater than 1, the analysis will run in multi-threaded mode.
#' This is only applicable for DESeq2.
#' @return A data frame with differential expression results.
#' With attributes:
#' - `object`: The input expression matrix
#' - `meta`: The metadata used for the analysis
#' - `paired_by`: The column name used for paired samples, if applicable.
#' - `group_by`: The column name used for grouping samples.
#' - `ident_1`: The first identity used for comparison.
#' - `ident_2`: The second identity used for comparison.
#' @export
RunDEGAnalysis <- function(
    exprs, group_by, ident_1, ident_2 = NULL, paired_by = NULL, meta = "@meta",
    tool = c("DESeq2", "edgeR", "deseq2", "edger"), log = get_logger(), ncores = 1
) {
    tool <- match.arg(tool)

    if (identical(meta, "@meta")) {
        meta <- attr(exprs, "meta")
    }
    check_columns <- utils::getFromNamespace("check_columns", "plotthis")
    paried_by <- check_columns(meta, paired_by, force_factor = TRUE)
    group_by <- check_columns(meta, group_by, force_factor = TRUE)
    stopifnot("[RunDEGAnalysis] 'group_by' must exist in the 'meta'." = !is.null(group_by))
    table_vec <- table(meta[[group_by]])
    stopifnot("[RunDEGAnalysis] 'ident_1' must exist in 'group_by' column of 'meta'." = ident_1 %in% names(table_vec))
    stopifnot("[RunDEGAnalysis] 'group_by' must have at least two identities." = length(table_vec) > 1)
    if (is.null(ident_2)) {
        if (length(table_vec) == 2) {
            ident_2 <- setdiff(names(table_vec), ident_1)
        } else {
            meta <- meta %>% mutate(.group = if_else(!!sym(group_by) == ident_1, ident_1, "Other"))
            meta[[group_by]] <- meta$.group
            meta$.group <- NULL
            ident_2 <- "Other"
        }
    }
    meta <- meta %>%
        filter(!!sym(group_by) %in% c(ident_1, ident_2)) %>%
        mutate(!!sym(group_by) := factor(!!sym(group_by), levels = c(ident_1, ident_2)))
    table_vec <- table(meta[[group_by]])
    stopifnot("[RunDEGAnalysis] Comparison requires at least 3 samples in each group." =
        all(table_vec >= 3))
    stopifnot("[RunDEGAnalysis] 'ident_1' and 'ident_2' must be different." = ident_1 != ident_2)
    # check if it is really paired
    if (!is.null(paired_by)) {
        # Each paired_by should have only one sample in each group
        paired_table <- table(meta[[paired_by]], meta[[group_by]])
        if (!all(paired_table == 1)) {
            stop("[RunDEGAnalysis] 'paired_by' should have only one sample in each group.")
        }
    }

    if (!"Sample" %in% colnames(meta)) {
        colnames(meta)[1] <- "Sample"
    }
    exprs <- as.matrix(exprs)
    exprs <- exprs[, as.character(meta$Sample)]

    # Run the differential expression analysis using the specified tool
    result <- switch(tool,
        DESeq2 = .run_deseq2(exprs, meta, group_by, ident_1, ident_2, paired_by, log, ncores),
        edgeR = .run_edger(exprs, meta, group_by, ident_1, ident_2, paired_by, log, ncores),
        deseq2 = .run_deseq2(exprs, meta, group_by, ident_1, ident_2, paired_by, log, ncores),
        edger = .run_edger(exprs, meta, group_by, ident_1, ident_2,paired_by,  log, ncores)
    )

    attr(result, "object") <- attr(exprs, "object") %||% exprs
    attr(result, "meta") <- meta
    attr(result, "paired_by") <- paired_by
    attr(result, "group_by") <- group_by
    attr(result, "ident_1") <- ident_1
    attr(result, "ident_2") <- ident_2

    class(result) <- c("BulkDEAnalysis", class(result))

    return(result)
}

#' Run DESeq2 differential expression analysis
#'
#' @param exprs Expression matrix with genes as rows and samples as columns.
#' @param meta Metadata data frame with sample information.
#' @param group_by Column name in `meta` to group samples for differential expression analysis
#' @param ident_1 First identity to compare against
#' @param ident_2 Second identity to compare against
#' @param paired_by Column name in `meta` for paired samples.
#' @param log Logger
#' @param ncores Number of cores to use for parallel processing.
#' @return A data frame with DESeq2 differential expression results.
#' @keywords internal
.run_deseq2 <- function(exprs, meta, group_by, ident_1, ident_2, paired_by, log, ncores) {
    log$info("Running differential gene expression analysis using DESeq2...")

    if (is.null(paired_by)) {
        design <- stats::as.formula(paste("~", bQuote(group_by)))
    } else {
        design <- stats::as.formula(paste("~", bQuote(group_by), "+", bQuote(paired_by)))
    }
    design <- stats::model.matrix(design, data = meta)
    dge <- DESeq2::DESeqDataSetFromMatrix(round(exprs), meta, design)
    if (ncores > 1) {
        dge <- DESeq2::DESeq(dge, parallel = TRUE, BPPARAM = BiocParallel::MulticoreParam(ncores))
    } else {
        dge <- DESeq2::DESeq(dge)
    }
    # d <- dge
    allgene <- DESeq2::results(dge)

    allgene <- as.data.frame(allgene[order(allgene$pvalue), , drop=FALSE])
    allgenes <- rownames(allgene)
    allgene <- data.frame(
        gene = rownames(allgene),
        log2FC = allgene$log2FoldChange,
        p_val = allgene$pvalue,
        p_val_adj = allgene$padj,
        baseMean = allgene$baseMean,
        lfcSE = allgene$lfcSE,
        stat = allgene$stat
    )
    rownames(allgene) <- allgenes

    allgene = allgene[!is.na(allgene$p_val) & !is.na(allgene$p_val_adj), , drop=FALSE]
    attr(allgene, "object") <- log2(exprs[rownames(allgene), , drop=FALSE] + 1)

    return(allgene)
}


#' Run edgeR differential expression analysis
#' @param exprs Expression matrix with genes as rows and samples as columns.
#' @param meta Metadata data frame with sample information.
#' @param group_by Column name in `meta` to group samples for differential expression analysis
#' @param ident_1 First identity to compare against
#' @param ident_2 Second identity to compare against
#' @param paired_by Column name in `meta` for paired samples.
#' @param log Logger
#' @param ncores Number of cores to use for parallel processing, not used in this function.
#' @return A data frame with edgeR differential expression results.
#' @keywords internal
.run_edger <- function(exprs, meta, group_by, ident_1, ident_2, paired_by, log, ncores) {
    log$info("Running differential gene expression analysis using edgeR...")
    if (is.null(paired_by)) {
        design <- stats::as.formula(paste("~", bQuote(group_by)))
    } else {
        design <- stats::as.formula(paste("~", bQuote(group_by), "+", bQuote(paired_by)))
    }
    design <- stats::model.matrix(design, data = meta)
    dge <- edgeR::DGEList(counts = round(exprs), group = meta[[group_by]])
    dge$samples$lib.size = colSums(dge$counts)
    dge <- edgeR::calcNormFactors(dge)
    disp <- edgeR::estimateDisp(dge, design)
    fit <- edgeR::glmFit(disp, design)
    fit = edgeR::glmLRT(fit, coef = 2)
    allgene <- edgeR::topTags(fit, n = Inf, p.value = 1)
    allgenes <- rownames(allgene)
    allgene <- data.frame(
        gene = rownames(allgene),
        log2FC = allgene$table$logFC,
        p_val = allgene$table$PValue,
        p_val_adj = allgene$table$FDR,
        logCPM = allgene$table$logCPM,
        LR = allgene$table$LR
    )
    rownames(allgene) <- allgenes
    allgene <- allgene[!is.na(allgene$p_val) & !is.na(allgene$p_val_adj), , drop=FALSE]
    attr(allgene, "object") <- log2(exprs[rownames(allgene), , drop=FALSE] + 1)

    return(allgene)
}


#' Visualize DEGs
#'
#' Visualize bulk differentially expressed genes
#'
#' @param degs DEGs from RunDEGAnalysis
#' @param plot_type Type of plot to generate
#' One of 'volcano', 'violin', 'box', 'bar', 'ridge', 'dim', 'heatmap', 'dot'
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
#' * For 'volcano', additional arguments to pass to 'scplotter::VolcanoPlot'
#' * For 'violin', 'box', 'bar', 'ridge', 'dim', 'heatmap', 'dot', additional arguments to pass to 'scplotter::FeatureStatPlot'
#' @return A ggplot object if 'outprefix' is NULL, otherwise, save the plot to the output directory
#' @export
#' @importFrom rlang sym
#' @importFrom scales number
#' @importFrom dplyr slice_head arrange pull filter rename
#' @examples
#' \donttest{
#' set.seed(8525)
#' data = matrix(rnbinom(1000, mu = 5, size = 1), nrow = 100, ncol = 10)
#' rownames(data) <- paste0("Gene", 1:100)
#' colnames(data) <- paste0("Sample", 1:10)
#' meta <- data.frame(
#'    Sample = colnames(data),
#'    Condition = rep(c("Control", "Treatment"), each = 5)
#' )
#'
#' degs <- RunDEGAnalysis(data, meta = meta,
#'  group_by = "Condition", ident_1 = "Treatment", tool = "edger")
#' VizBulkDEGs(degs, plot_type = "volcano", legend.position = "top", y_cutoff = 0.25)
#' VizBulkDEGs(degs, plot_type = "violin", genes = 2)
#' VizBulkDEGs(degs, plot_type = "violin", stack = TRUE, genes = 2)
#' VizBulkDEGs(degs, plot_type = "box", genes = 2)
#' VizBulkDEGs(degs, plot_type = "bar", genes = 2, x_text_angle = 90)
#' VizBulkDEGs(degs, plot_type = "ridge", genes = 2)
#' VizBulkDEGs(degs, plot_type = "heatmap", genes = 5)
#' }
VizBulkDEGs <- function(
    degs, plot_type = c("volcano", "violin", "box", "bar", "ridge", "heatmap", "cor"),
    order_by = 'desc(abs(log2FC))', genes = 10, outprefix = NULL,
    devpars = list(res = 100), more_formats = c(), save_code = FALSE,
    show_row_names = TRUE, show_column_names = TRUE, ...
) {
    # degs: p_val avg_log2FC pct.1 pct.2 p_val_adj gene group diff_pct
    stopifnot("[VizBulkDEGs] Can only visualize object from RunDEGAnalysis" = inherits(degs, "BulkDEAnalysis"))
    stopifnot("[VizBulkDEGs] 'outprefix' must be provided to save code" = !save_code || !is.null(outprefix))
    group_by <- attr(degs, "group_by")
    plot_type <- match.arg(plot_type)
    are_allmarkers <- !all(is.na(degs[[group_by]]))

    if (plot_type == "volcano") {
        if (save_code) {
            VolcanoPlot <- gglogger::register(scplotter::VolcanoPlot, "VolcanoPlot")
        } else {
            VolcanoPlot <- scplotter::VolcanoPlot
        }

        facet_by <- if (are_allmarkers) group_by else NULL
        args <- list(data = degs, x = "log2FC",
            y = "p_val_adj", ylab = "-log10(p_val_adj)", facet_by = facet_by, label_by = "gene", ...)
        args$y_cutoff <- args$y_cutoff %||% 0.05
        args$y_cutoff_name <- paste0("p_val_adj = ", args$y_cutoff)
        p <- do_call(VolcanoPlot, args)
    } else {
        object <- attr(degs, "object")
        ident_1 <- attr(degs, "ident_1")
        ident_2 <- attr(degs, "ident_2")
        meta <- attr(degs, "meta")

        if (is.numeric(genes)) {
            features <- degs %>%
                arrange(!!parse_expr(order_by)) %>%
                slice_head(n = genes) %>%
                pull("gene") %>%
                unique()
        } else {
            features <- degs %>%
                arrange(!!parse_expr(order_by)) %>%
                filter(!!parse_expr(genes)) %>%
                pull("gene") %>%
                unique()
        }
        object <- as.data.frame(t(object[features, , drop = FALSE]))
        object <- cbind(object, meta[match(rownames(object), meta$Sample), , drop = FALSE])

        # if (save_code) {
        #     FeatureStatPlot <- gglogger::register(scplotter::FeatureStatPlot, "FeatureStatPlot")
        # } else {
        #     FeatureStatPlot <- scplotter::FeatureStatPlot
        # }

        should_shrink <- !plot_type %in% c("heatmap", "dot")
        should_pivot <- !plot_type %in% c("heatmap", "dot", "cor")

        feature_stat_plot <- utils::getFromNamespace(".feature_stat_plot", "scplotter")
        p <- feature_stat_plot(
            data = object, features = features, plot_type = plot_type,
            should_shrink = should_shrink, should_pivot = should_pivot,
            ident = group_by, ...
        )
    }

    if (!is.null(outprefix)) {
        formats <- unique(c("png", more_formats))
        save_plot(p, outprefix, formats = formats, devpars = devpars)
        if (save_code) {
            args <- list(plot = p,
                setup = c("library(rlang)", "library(dplyr)", "library(gglogger)", "library(scplotter)", "load('data.RData')"),
                prefix = outprefix)
            args <- c(args, setdiff(ls(), c("p", "args", "formats", "devpars", "more_formats", "save_code", "outprefix", "are_allmarkers")))
            do_call(save_plotcode, args)
        }
        return(NULL)
    } else {
        return(p)
    }
}
