#' Re-exported functions from other packages
#'
#' * [enrichit::EnrichIt()] (exported as RunEnrichment) - Enrichment analysis
#' * [enrichit::ParseGMT()] - Parse GMT file
#'
#' @rdname re-exports
#' @inheritParams enrichit::EnrichIt
#' @export
#' @importFrom enrichit EnrichIt
#' @examples
#' \dontrun{
#'   # See following links for examples
#'   # https://pwwang.github.io/enrichit/reference/EnrichIt.html
#'   # https://pwwang.github.io/enrichit/reference/ParseGMT.html
#' }
RunEnrichment <- EnrichIt

#' @rdname re-exports
#' @inheritParams enrichit::ParseGMT
#' @export
#' @importFrom enrichit ParseGMT
ParseGMT <- ParseGMT

#' @rdname re-exports
#' @inheritParams scplotter::EnrichmentPlot
#' @export
#' @importFrom scplotter EnrichmentPlot
VizEnrichment <- EnrichmentPlot

#' Pre-rank genes based on expression data
#'
#' @rdname RunGSEAPreRank
#' @param exprs Expression data matrix (genes x samples)
#' @param classes A vector of class labels for each sample
#' Must be in the same order as the columns of `exprs`
#' @param case The case group name in the `classes` vector
#' @param control The control group name in the `classes` vector
#' If `NULL`, the control group will be the other groups in `classes`
#' @param method The method to use for ranking
#' One of "signal_to_noise", "abs_signal_to_noise", "t_test",
#' "ratio_of_classes", "diff_of_classes", "log2_ratio_of_classes",
#' "s2n", "abs_s2n"
#' @return A vector of the rank values with names as the gene names
#' @export
#' @importFrom dplyr arrange %>%
#' @importFrom tibble rownames_to_column
#' @importFrom Matrix rowMeans
#' @importFrom matrixStats rowSds
#' @seealso https://gseapy.readthedocs.io/en/latest/run.html#gseapy.gsea
RunGSEAPreRank <- function(
    exprs,
    classes, # must be in the order of colnames(exprdata)
    case,
    control = NULL,
    method = c(
        "signal_to_noise",
        "abs_signal_to_noise",
        "t_test",
        "ratio_of_classes",
        "diff_of_classes",
        "log2_ratio_of_classes",
        "s2n",
        "abs_s2n"
    )
) {
    set.seed(8525)
    method <- match.arg(method)

    expr_pos_mean <- rowMeans(exprs[, classes == case, drop = FALSE], na.rm = TRUE)
    if (is.null(control)) {
        expr_neg_mean <- rowMeans(exprs[, classes != case, drop = FALSE], na.rm = TRUE)
    } else {
        expr_neg_mean <- rowMeans(exprs[, classes == control, drop = FALSE], na.rm = TRUE)
    }
    expr_pos_std <- rowSds(as.matrix(exprs[, classes == case, drop = FALSE]), na.rm = TRUE, useNames = TRUE)
    if (is.null(control)) {
        expr_neg_std <- rowSds(as.matrix(exprs[, classes != case, drop = FALSE]), na.rm = TRUE, useNames = TRUE)
    } else {
        expr_neg_std <- rowSds(as.matrix(exprs[, classes == control, drop = FALSE]), na.rm = TRUE, useNames = TRUE)
    }
    # add a small random number to avoid division by zero
    rands <- stats::rnorm(length(expr_neg_std)) * 1e-6

    if (method %in% c("s2n", "signal_to_noise")) {
        out <- (expr_pos_mean - expr_neg_mean) / (expr_pos_std + expr_neg_std + rands)
    } else if (method %in% c("abs_s2n", "abs_signal_to_noise")) {
        out <- abs((expr_pos_mean - expr_neg_mean) / (expr_pos_std + expr_neg_std + rands))
    } else if (method == "t_test") {
        # ser = (df_mean[case] - df_mean[control])/ np.sqrt(df_std[case]**2/len(df_std)+df_std[control]**2/len(df_std) )
        out <- (expr_pos_mean - expr_neg_mean) / sqrt(
            expr_pos_std ^ 2 / length(expr_pos_std) +
            expr_neg_std ^ 2 / length(expr_neg_std)
        )
    } else if (method == "ratio_of_classes") {
        out <- expr_pos_mean / expr_neg_mean
    } else if (method == "diff_of_classes") {
        out <- expr_pos_mean - expr_neg_mean
    } else if (method == "log2_ratio_of_classes") {
        out <- log2(expr_pos_mean) - log2(expr_neg_mean)
    } else {
        stop(paste("Unknown method:", method))
    }
    # todo: log2fc * -log10(p)
    # see https://github.com/crazyhottommy/RNA-seq-analysis/blob/master/GSEA_explained.md#2-using-a-pre-ranked-gene-list
    names(out) <- rownames(exprs)
    out <- sort(out, decreasing = TRUE)
    return(out)
}


#' Run GSEA using fgsea
#'
#' @rdname RunGSEA
#' @param ranks A named numeric vector of gene ranks
#' @param genesets A list of gene sets
#' @param ... Additional arguments passed to [fgsea::fgsea()]
#' @return A data frame with the results of the fGSEA analysis
#' @export
#' @examples
#' \dontrun{
#' exprs <- matrix(rnorm(1000), nrow = 100, ncol = 10)
#' colnames(exprs) <- paste0("Sample", 1:10)
#' rownames(exprs) <- paste0("Gene", 1:100)
#' classes <- c(rep("A", 5), rep("B", 5))
#' ranks <- RunGSEAPreRank(exprs, case = "A", control = "B", classes = classes)
#' genesets <- list(
#'     set1 = c("Gene1", "Gene2", "Gene3"),
#'     set2 = c("Gene4", "Gene5", "Gene6")
#' )
#' RunGSEA(ranks, genesets)
#' }
RunGSEA = function(ranks, genesets, ...) {
    stopifnot("'ranks' must be a named numeric vector" = is.numeric(ranks) && !is.null(names(ranks)))

    out <- fgsea::fgsea(
        pathways = genesets,
        stats = ranks,
        ...
    )
    out <- out[order(out$pval), , drop = FALSE]
    # The leadingEdge column is list of genes, which can not be saved directly using write.table
    # so we convert it to a string by pasting the genes together
    out$leadingEdge <- sapply(out$leadingEdge, function(x) paste(x, collapse = ","))

    attr(out, "gene_ranks") <- ranks
    attr(out, "gene_sets") <- genesets

    return(out)
}


#' Visualize GSEA results
#'
#' @rdname VizGSEA
#' @param gsea_results A data frame with the results of the fgsea analysis
#' @param plot_type The type of plot to create
#' One of "summary", "gsea"
#' @param group_by The column name to group by for heatmap and dot plot.
#' They will be used as the columns in the heatmap or dot plot.
#' @param values_by The column name to use for the values in the heatmap or dot plot.
#' Default is "NES" (normalized enrichment score).
#' @param signif_by The column name to use for significance in the heatmap or dot plot.
#' Default is "padj" (adjusted p-value). It can also be "pval".
#' If NULL, no significance labels will be added to the heatmap.
#' @param signif_cutoff A numeric vector of significance cutoffs for the heatmap labels.
#' Multiple values can be provided to indicate different levels (at most 3) of significance.
#' For example, `c(0.05, 0.01, 0.001)` will label pathways with p-values less than 0.05 with "*",
#' less than 0.01 with "**", and less than 0.001 with "***".
#' @inheritParams plotthis::GSEASummaryPlot
#' @inheritParams plotthis::GSEAPlot
#' @param ... Additional arguments passed to the plotting function
#' * When `plot_type` is "summary", they are passed to [plotthis::GSEASummaryPlot()]
#' * When `plot_type` is "gsea", they are passed to [plotthis::GSEAPlot()]
#' @return A ggplot object or a list of ggplot objects
#' @importFrom rlang sym parse_expr
#' @importFrom dplyr mutate %>%
#' @importFrom plotthis GSEASummaryPlot GSEAPlot Heatmap DotPlot
#' @export
#' @examples
#' \donttest{
#' set.seed(123)
#' exprs <- matrix(rnorm(1000), nrow = 100, ncol = 10)
#' colnames(exprs) <- paste0("Sample", 1:10)
#' rownames(exprs) <- paste0("Gene", 1:100)
#' classes <- c(rep("A", 5), rep("B", 5))
#' ranks <- RunGSEAPreRank(exprs, case = "A", control = "B", classes = classes)
#' genesets <- list(
#'     set1 = c("Gene1", "Gene2", "Gene3"),
#'     set2 = c("Gene4", "Gene5", "Gene6"),
#'     set3 = c("Gene7", "Gene8", "Gene9"),
#'     set4 = c("Gene10", "Gene11", "Gene12"),
#'     set5 = c("Gene13", "Gene14", "Gene15"),
#'     set6 = c("Gene16", "Gene17", "Gene18"),
#'     set7 = c("Gene19", "Gene20", "Gene21"),
#'     set8 = c("Gene22", "Gene23", "Gene24"),
#'     set9 = c("Gene25", "Gene26", "Gene27"),
#'     set10 = c("Gene12", "Gene86", "Gene87", "Gene83", "Gene71")
#' )
#' r <- RunGSEA(ranks, genesets)
#'
#' # Visualize the GSEA results
#' VizGSEA(r, plot_type = "summary")
#' VizGSEA(r, plot_type = "gsea", gs = c("set10", "set2"))
#'
#' r$Group <- "A"
#' r2 <- r
#' r2$Group <- "B"
#' r2$NES <- sample(r2$NES)
#' VizGSEA(rbind(r, r2), plot_type = "heatmap")
#' }
VizGSEA <- function(
    gsea_results, plot_type = c("summary", "gsea", "heatmap", "dot"),
    gene_ranks = "@gene_ranks", gene_sets = "@gene_sets", gs = NULL,
    group_by = NULL, values_by = "NES", signif_by = "padj", signif_cutoff = 0.05,
    ...
) {
    plottype <- match.arg(plot_type)

    if (plot_type == "summary") {
        GSEASummaryPlot(gsea_results, gene_ranks = gene_ranks, gene_sets = gene_sets, ...)
    } else if (plot_type == "gsea") {
        GSEAPlot(gsea_results, gene_ranks = gene_ranks, gene_sets = gene_sets, gs = gs, ...)
    } else if (plot_type == "heatmap") {
        stopifnot("[VizGSEA] 'group_by' is required for heatmap" = !is.null(group_by))
        if (!is.null(signif_by)) {
            cell_type = "label"
            signif_df <- gsea_results[, c("pathway", group_by, signif_by), drop = FALSE] %>%
                tidyr::pivot_wider(
                    names_from = group_by,
                    values_from = signif_by
                ) %>%
                as.data.frame()
            rownames(signif_df) <- signif_df$pathway
            signif_df$pathway <- NULL
            signif_df <- as.matrix(signif_df)
            signif_cutoff <- sort(signif_cutoff)
            if (length(signif_cutoff) > 3) {
                stop("[VizGSEA] 'signif_cutoff' should have at most 3 values")
            }
            label <- function(x, i, j, rn, cn) {
                # don't use i, j here, use rn, cn instead, in case
                # the rownames and colnames of signif_df is different
                # from the rownames and colnames of the one passed to ComplexHeatmap::Heatmap()
                idx_i <- match(rn, rownames(signif_df))
                idx_j <- match(cn, colnames(signif_df))
                sig <- ComplexHeatmap::pindex(signif_df, idx_i, idx_j)
                if (length(signif_cutoff) == 1) {
                    ifelse(sig < signif_cutoff, "*", NA)
                } else if (length(signif_cutoff) == 2) {
                    ifelse(sig < signif_cutoff[1], "**",
                        ifelse(sig < signif_cutoff[2], "*", NA)
                    )
                } else if (length(signif_cutoff) == 3) {
                    ifelse(sig < signif_cutoff[1], "***",
                        ifelse(sig < signif_cutoff[2], "**",
                            ifelse(sig < signif_cutoff[3], "*", NA)
                        )
                    )
                }
            }
        } else {
            cell_type = "tile"
            label <- NULL
        }
        Heatmap(
            gsea_results,
            in_form = "long",
            values_by = values_by,
            rows_by = "pathway",
            columns_by = group_by,
            cell_type = cell_type,
            label = label,
            ...
        )
    } else if (plot_type == "dot") {
        stopifnot("[VizGSEA] 'group_by' is required for dot plot" = !is.null(group_by))
        if (signif_by == "padj") {
            fill_by <- "-log10(padj)"
        } else if (signif_by == "pval") {
            fill_by <- "-log10(pval)"
        } else {
            fill_by <- signif_by
        }
        gsea_results <- mutate(
            gsea_results,
            !!sym(fill_by) := !!parse_expr(fill_by)
        )
        DotPlot(
            gsea_results,
            x = group_by,
            y = "pathway",
            fill_by = values_by,
            size_by = fill_by,
            ...
        )
    }
}
