#' AddCommand
#'
#' Add a command to a Seurat object `@commands` slot
#'
#' @param object Seurat object
#' @param name Name of the command
#' @param call.string Call string of the command
#' @param params Parameters of the command
#' @param assay.used Assay used in the command
#' @return The Seurat object with the command added
#' @importFrom rlang %||%
#' @importFrom methods new
#' @importFrom Seurat DefaultAssay
#' @export
#' @examples
#' object <- SeuratObject::pbmc_small
#' names(object@commands)
#'
#' object <- AddCommand(object, "RunDEAnalysis",
#'  "RunDEAnalysis(object, group.by = 'groups', ident.1 = 'g1', ident.2 = 'g2')",
#'  list(group.by = 'groups', ident.1 = 'g1', ident.2 = 'g2'))
#' object@commands$RunDEAnalysis
AddCommand <- function(
    object, name, call.string = paste0(name, "(object, ...)"),
    params = list(), assay.used = NULL
) {
    assay.used <- assay.used %||% DefaultAssay(object)
    if (is.null(object@commands)) {
        object@commands <- list()
    }
    # compose a new SeuratCommand to record it to srtobj@commands
    commands <- names(SeuratObject::pbmc_small@commands)
    scommand <- SeuratObject::pbmc_small@commands[[commands[length(commands)]]]
    scommand@name <- name
    scommand@time.stamp <- Sys.time()
    scommand@assay.used <- assay.used
    scommand@call.string <- call.string
    scommand@params <- params
    object@commands[[name]] <- scommand
    object
}

#' RunDEAnalysis
#'
#' Run differential expression analysis on a Seurat object
#'
#' @param object Seurat object
#' @param group.by Column name in meta.data to group cells by
#' @param ident.1 Identity of the first group of cells
#' @param ident.2 Identity of the second group of cells
#' @param assay Assay to use for analysis
#' @param subset Subset of cells to use for analysis
#'  It should be a string of expression to pass to `dplyr::filter` function
#' @param cache Directory to cache results
#'  The results of the analysis will be saved in this directory
#'  Anything changed in the object or arguments will trigger a re-run
#' @param error Whether to raise an error if the analysis fails
#'  Otherwise, return an empty data frame
#' @param ... Additional arguments to pass to [Seurat::FindMarkers]
#' @export
#' @import tidyseurat
#' @importFrom dplyr filter
#' @importFrom Seurat DefaultAssay PrepSCTFindMarkers FindMarkers
#' @examples
#' RunDEAnalysis(SeuratObject::pbmc_small, "groups", "g1", "g2")
RunDEAnalysis <- function(
    object, group.by, ident.1 = NULL, ident.2 = NULL, assay = NULL, subset = NULL, cache = NULL, error = TRUE, ...) {
    if (is.null(cache)) {
        cache <- tempdir()
        # file.remove(cache)
        cache <- dirname(cache)  # a better way to get the tmpdir?
    }
    cached <- get_cached(list(object, group.by, ident.1, ident.2, subset, ...), "biopipen.utils.RunDEAnalysis", cache)
    if (!is.null(cached$data)) {
        return(cached$data)
    }

    stopifnot("'group.by' is not found in the object" = group.by %in% colnames(object@meta.data))
    all_ident <- as.character(sort(unique(object@meta.data[[group.by]])))
    stopifnot("'ident.1' is not found in 'group.by'" = is.null(ident.1) || ident.1 %in% all_ident)
    stopifnot("'ident.2' is not found in 'group.by'" = is.null(ident.2) || ident.2 %in% all_ident)
    stopifnot("'ident.1' should be provided when 'ident.2' is provided" = is.null(ident.2) || !is.null(ident.1))

    assay <- assay %||% DefaultAssay(object)

    if (assay == "SCT" && !"PrepSCTFindMarkers" %in% names(object@commands)) {
        object <- PrepSCTFindMarkers(object)
        object <- AddCommand(object, "PrepSCTFindMarkers")
    }
    empty <- data.frame(
        gene = character(),
        p_val = numeric(),
        avg_log2FC = numeric(),
        pct.1 = numeric(),
        pct.2 = numeric(),
        p_val_adj = numeric(),
        diff_pct = numeric()
    )
    empty[[group.by]] <- character()
    class(empty) <- c("DEAnalysis", class(empty))

    object <- filter(object, !is.na(!!sym(group.by)))
    if (!is.null(subset)) {
        object <- filter(object, !!parse_expr(subset))
    }
    # https://satijalab.org/seurat/archive/v4.3/sctransform_v2_vignette#identify-differential-expressed-genes-across-conditions
    recorrect_umi <- is.null(subset) && assay == "SCT"

    find_markers <- function(recorrect_umi, ...) {
        if (is.null(ident.1)) {
            degs <- do.call(rbind, lapply(all_ident, function(ident) {
                m <- FindMarkers(object, group.by = group.by, ident.1 = ident, recorrect_umi = recorrect_umi, assay = assay, ...)
                m$gene <- rownames(m)
                rownames(m) <- NULL
                m[[group.by]] <- ident
                m
            }))
        } else {
            degs <- FindMarkers(object, group.by = group.by, ident.1 = ident.1, ident.2 = ident.2, recorrect_umi = recorrect_umi,
                assay = assay, ...)
            degs$gene <- rownames(degs)
            degs[[group.by]] <- NA
        }
        class(degs) <- c("DEAnalysis", class(degs))

        degs
    }

    if (!error) {
        degs <- tryCatch({
            find_markers(recorrect_umi, ...)
        }, error = function(e) {
            warning("[RunDEAnalysis] Failed to run DE analysis: ", e$message, immediate. = TRUE)
            empty
        })
    } else {
        degs <- find_markers(recorrect_umi, ...)
    }

    degs$diff_pct <- degs$pct.1 - degs$pct.2
    attr(degs, "object") <- object
    attr(degs, "group.by") <- group.by
    attr(degs, "ident.1") <- ident.1
    attr(degs, "ident.2") <- ident.2

    cached$data <- degs
    save_to_cache(cached, "RunDEAnalysis", cache)

    degs
}
