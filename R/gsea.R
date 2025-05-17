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
#' @rdname PreRank
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
PreRank <- function(
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
    rands <- rnorm(length(expr_neg_std)) * 1e-6

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
#' ranks <- PreRank(exprs, case = "A", control = "B", classes = classes)
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
    out <- out[order(out$padj), , drop = FALSE]

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
#' @inheritParams plotthis::GSEASummaryPlot
#' @inheritParams plotthis::GSEAPlot
#' @param ... Additional arguments passed to the plotting function
#' * When `plot_type` is "summary", they are passed to [plotthis::GSEASummaryPlot()]
#' * When `plot_type` is "gsea", they are passed to [plotthis::GSEAPlot()]
#' @return A ggplot object or a list of ggplot objects
#' @importFrom plotthis GSEASummaryPlot GSEAPlot
#' @export
#' @examples
#' \donttest{
#' set.seed(123)
#' exprs <- matrix(rnorm(1000), nrow = 100, ncol = 10)
#' colnames(exprs) <- paste0("Sample", 1:10)
#' rownames(exprs) <- paste0("Gene", 1:100)
#' classes <- c(rep("A", 5), rep("B", 5))
#' ranks <- PreRank(exprs, case = "A", control = "B", classes = classes)
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
#' }
VizGSEA <- function(
    gsea_results, plot_type = c("summary", "gsea"),
    gene_ranks = "@gene_ranks", gene_sets = "@gene_sets", gs = NULL, ...)
{
    plottype <- match.arg(plot_type)

    if (plot_type == "summary") {
        GSEASummaryPlot(gsea_results, gene_ranks = gene_ranks, gene_sets = gene_sets, ...)
    } else if (plot_type == "gsea") {
        GSEAPlot(gsea_results, gene_ranks = gene_ranks, gene_sets = gene_sets, gs = gs, ...)
    }
}
