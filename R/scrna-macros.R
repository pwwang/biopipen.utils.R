#' Mutater the Seurat metadata
#'
#' @param object Seurat object
#' @param mutaters A named list of mutater expressions, where the names are the new column names
#' and the values are the expressions to mutate the columns
#' The name with the suffix `:ident` will be used as the new identity column
#' The values can be either character strings of expressions to be parsed
#' @param log Logger object to log the messages. If NULL, the default logger will be used.
#' @return A Seurat object with mutated metadata
#' @importFrom dplyr mutate
#' @importFrom rlang parse_expr %||%
#' @importFrom SeuratObject Idents
#' @export
#' @examples
#' \donttest{
#' obj <- MutateSeuratMeta(
#'     SeuratObject::pbmc_small,
#'     list(a = 1, `g:ident` = "paste0(groups, '_ident')")
#' )
#' head(obj@meta.data[, c("a", "g")])
#' GetIdentityColumn(obj)
#' }
MutateSeuratMeta <- function(object, mutaters, log = NULL) {
    if (length(mutaters) == 0 || is.null(mutaters)) {
        return(object)
    }
    ident_keys <- names(mutaters)[grepl(":ident$", names(mutaters))]
    if (length(ident_keys) > 1) {
        stop(
            "Only one ident mutater is allowed, but found multiple: ",
            paste(ident_keys, collapse = ", ")
        )
    }
    log <- log %||% get_logger()
    new_ident_col <- NULL
    if (length(ident_keys) == 1) {
        new_ident_col <- sub(":ident$", "", ident_keys[1])
        mutaters[[new_ident_col]] <- mutaters[[ident_keys[1]]]
        mutaters[[ident_keys[1]]] <- NULL
    }
    mutaters <- lapply(mutaters, as.character)
    object@meta.data <- mutate(
        object@meta.data,
        !!!lapply(mutaters, parse_expr)
    )

    if (!is.null(new_ident_col)) {
        log$info("Setting the new identity column to '{}'", new_ident_col)
        Idents(object) <- new_ident_col
    }

    object
}
#' Get the column name in meta.data that works as identity
#'
#' @param object Seurat object
#' @param return_all Whether to return all candidate columns. If FALSE, only return the best candidate column with the shortest name.
#' The best candidate column is the one that matches the Idents(object) and has the shortest name. If there are multiple columns with the same shortest name, return the one with "cluster" or "annotation" or "type" in the name. If there is still a tie, return the first one.
#' If return_all is TRUE, return all candidate columns that match the Idents(object) as a character vector. The best candidate column will be the first one in the returned vector.
#'
#' @return The column name in meta.data that works as identity
#' If there are multiple columns matching, return the shortest one.
#' @importFrom SeuratObject Idents
#' @importFrom rlang %||%
#' @export
#' @examples
#' obj <- SeuratObject::pbmc_small
#' GetIdentityColumn(obj)
#'
#' SeuratObject::Idents(obj) <- "groups"
#' GetIdentityColumn(obj)
GetIdentityColumn <- function(object, return_all = FALSE) {
    candidates <- c()
    for (name in colnames(object@meta.data)) {
        if (
            !is.character(object@meta.data[[name]]) &&
                !is.factor(object@meta.data[[name]])
        ) {
            next
        }
        if (
            isTRUE(all.equal(
                as.character(unname(Idents(object))),
                as.character(object@meta.data[[name]])
            ))
        ) {
            candidates <- c(candidates, name)
        }
    }
    if (length(candidates) == 1) {
        return(candidates[1])
    } else if (length(candidates) > 0) {
        if (isTRUE(return_all)) {
            return(candidates)
        }
        # Choose the one with "cluster" or "annotation" or "type" in the name
        outcol <- NULL
        for (keyword in c("cluster", "annotation", "type")) {
            keyword_candidates <- candidates[grepl(
                keyword,
                tolower(candidates)
            )]
            if (length(keyword_candidates) > 0) {
                outcol <- keyword_candidates[which.min(nchar(
                    keyword_candidates
                ))]
                break
            }
        }
        outcol <- outcol %||% candidates[which.min(nchar(candidates))]
        warning(
            "[GetIdentityColumn] Using '",
            outcol,
            "' as the identity column, other candidates: ",
            paste(setdiff(candidates, outcol), collapse = ", "),
            immediate. = TRUE
        )
        outcol
    } else {
        NULL
    }
}
#' Add a command to a Seurat object `@commands` slot
#'
#' @param object Seurat object
#' @param name Name of the command
#' @param call.string Call string of the command
#' @param params Parameters of the command
#' @param assay.used Assay used in the command
#' @param time.stamp Time stamp of the command
#' @return The Seurat object with the command added
#' @importFrom rlang %||%
#' @importFrom methods new
#' @importFrom Seurat DefaultAssay
#' @export
#' @examples
#' object <- SeuratObject::pbmc_small
#' names(object@commands)
#'
#' object <- AddSeuratCommand(
#'     object, "RunSeuratDEAnalysis",
#'     "RunSeuratDEAnalysis(object, group_by = 'groups', ident_1 = 'g1', ident_2 = 'g2')",
#'     list(group_by = "groups", ident_1 = "g1", ident_2 = "g2")
#' )
#' object@commands$RunSeuratDEAnalysis
AddSeuratCommand <- function(
    object,
    name,
    call.string = paste0(name, "(object, ...)"),
    params = list(),
    assay.used = NULL,
    time.stamp = Sys.time()
) {
    assay.used <- assay.used %||% DefaultAssay(object)
    if (is.null(object@commands)) {
        object@commands <- list()
    }
    # compose a new SeuratCommand to record it to srtobj@commands
    commands <- names(SeuratObject::pbmc_small@commands)
    scommand <- SeuratObject::pbmc_small@commands[[commands[length(commands)]]]
    scommand@name <- name
    scommand@time.stamp <- time.stamp
    scommand@assay.used <- assay.used
    scommand@call.string <- call.string
    scommand@params <- params
    object@commands[[name]] <- scommand
    object
}
#' RunSeuratDEAnalysis
#'
#' Run differential expression analysis on a Seurat object
#'
#' @param object Seurat object
#' @param group_by Column name in meta.data to group cells by
#' @param ident_1 Identity of the first group of cells
#' @param ident_2 Identity of the second group of cells
#' @param assay Assay to use for analysis
#' @param subset Subset of cells to use for analysis
#'  It should be a string of expression to pass to `dplyr::filter` function
#' @param cache Directory to cache the results. Set to `FALSE` to disable caching
#'  The results of the analysis will be saved in this directory
#'  Anything changed in the object or arguments will trigger a re-run
#' @param error Whether to raise an error if the analysis fails
#'  Otherwise, return an empty data frame
#' @param log Logger object to log the messages. If NULL, the default logger will be used.
#' @param log_prefix Prefix to add to the log messages
#' @param ... Additional arguments to pass to [Seurat::FindMarkers()]
#' @export
#' @import tidyseurat
#' @importFrom dplyr filter
#' @importFrom Seurat DefaultAssay PrepSCTFindMarkers FindMarkers
#' @examples
#' RunSeuratDEAnalysis(SeuratObject::pbmc_small, "groups", "g1", "g2")
RunSeuratDEAnalysis <- function(
    object,
    group_by,
    ident_1 = NULL,
    ident_2 = NULL,
    assay = NULL,
    subset = NULL,
    cache = NULL,
    error = TRUE,
    log = NULL,
    log_prefix = "",
    ...
) {
    cache <- cache %||% gettempdir()
    cached <- Cache$new(
        list(object, group_by, ident_1, ident_2, subset, assay, ...),
        prefix = "biopipen.utils.RunSeuratDEAnalysis",
        cache_dir = cache,
    )
    log <- log %||% get_logger()
    if (cached$is_cached()) {
        log$info(
            "{log_prefix}Using cached results from: {cached$get_path()}"
        )
        return(cached$restore())
    }

    stopifnot(
        "'group_by' is not found in the object" = group_by %in%
            colnames(object@meta.data)
    )
    stopifnot(
        "'ident_1' should be provided when 'ident_2' is provided" = is.null(
            ident_2
        ) ||
            !is.null(ident_1)
    )

    assay <- assay %||% DefaultAssay(object)
    is_sct <- inherits(object[[assay]], "SCTAssay")

    if (is_sct && !"PrepSCTFindMarkers" %in% names(object@commands)) {
        log$info(
            "{log_prefix}Preparing SCT for FindMarkers"
        )
        object <- PrepSCTFindMarkers(object)
        object <- AddSeuratCommand(object, "PrepSCTFindMarkers")
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
    empty[[group_by]] <- character()
    class(empty) <- c("SeuratDEAnalysis", class(empty))

    object <- filter(object, !is.na(!!sym(group_by)))
    if (!is.null(subset)) {
        log$info("{log_prefix}Subsetting object with: {subset}")
        object <- filter(object, !!parse_expr(subset))
    }
    # all_ident <- as.character(sort(unique(object@meta.data[[group_by]])))
    all_ident <- if (is.factor(object@meta.data[[group_by]])) {
        levels(object@meta.data[[group_by]])
    } else {
        as.character(sort(unique(object@meta.data[[group_by]])))
    }
    if (!is.null(ident_1) && !ident_1 %in% all_ident) {
        if (isTRUE(error)) {
            msg <- paste0(
                "'",
                ident_1,
                "' (ident_1) is not found in '",
                group_by,
                "' (group_by). "
            )
            if (!is.null(ident_2) && ident_2 %in% all_ident) {
                msg <- paste0(msg, "Do you have enough cells?")
            }
            stop(msg)
        } else {
            warning(
                "[RunSeuratDEAnalysis] '",
                ident_1,
                "' (ident_1) is not found in '",
                group_by,
                "' (group_by), returning empty result",
                immediate. = TRUE
            )
            cached$save(empty)
            return(empty)
        }
    }
    if (!is.null(ident_2) && !ident_2 %in% all_ident) {
        if (isTRUE(error)) {
            msg <- paste0(
                "'",
                ident_2,
                "' (ident_2) is not found in '",
                group_by,
                "' (group_by). "
            )
            if (ident_1 %in% all_ident) {
                msg <- paste0(msg, "Do you have enough cells?")
            }
            stop(msg)
        } else {
            warning(
                "[RunSeuratDEAnalysis] '",
                ident_2,
                "' (ident_2) is not found in '",
                group_by,
                "' (group_by), returning empty result",
                immediate. = TRUE
            )
            cached$save(empty)
            return(empty)
        }
    }
    # https://satijalab.org/seurat/archive/v4.3/sctransform_v2_vignette#identify-differential-expressed-genes-across-conditions
    recorrect_umi <- is.null(subset) && is_sct

    find_markers <- function(recorrect_umi, ...) {
        if (is.null(ident_1)) {
            logic_error <- NULL
            degs <- do_call(
                rbind,
                lapply(all_ident, function(ident) {
                    m <- tryCatch(
                        {
                            FindMarkers(
                                object,
                                group.by = group_by,
                                ident.1 = ident,
                                recorrect_umi = recorrect_umi,
                                assay = assay,
                                ...
                            )
                        },
                        error = function(e) {
                            if (
                                grepl("has fewer than", e$message) ||
                                    grepl(
                                        "Cannot find the following identities in the object",
                                        e$message
                                    )
                            ) {
                                warning(
                                    "[RunSeuratDEAnalysis] Skipping DE analysis for group_by = ",
                                    group_by,
                                    ", ident_1 = ",
                                    ident,
                                    ": ",
                                    e$message,
                                    immediate. = TRUE
                                )
                                return(empty[0, , drop = FALSE])
                            } else {
                                logic_error <<- paste0(
                                    e$message,
                                    " (group_by = ",
                                    group_by,
                                    ", ident_1 = ",
                                    ident,
                                    ")"
                                )
                                return(empty[0, , drop = FALSE])
                            }
                        }
                    )
                    if (nrow(m) == 0) {
                        return(m)
                    }
                    m$gene <- rownames(m)
                    rownames(m) <- NULL
                    m[[group_by]] <- ident
                    m
                })
            )
            degs[[group_by]] <- factor(degs[[group_by]], levels = all_ident)
            if (!is.null(logic_error)) {
                stop(logic_error)
            }
        } else {
            degs <- FindMarkers(
                object,
                group.by = group_by,
                ident.1 = ident_1,
                ident.2 = ident_2,
                recorrect_umi = recorrect_umi,
                assay = assay,
                ...
            )
            degs$gene <- rownames(degs)
            degs[[group_by]] <- ifelse(
                !is.null(ident_2),
                paste0(ident_1, ":", ident_2),
                ident_1
            )
        }
        class(degs) <- c("SeuratDEAnalysis", class(degs))

        degs
    }

    log$info(
        "{log_prefix}Running DE analysis with group_by = {group_by}, ident_1 = {ident_1}, ident_2 = {ident_2}, assay = {assay}, recorrect_umi = {recorrect_umi}"
    )
    degs <- tryCatch(
        {
            find_markers(recorrect_umi, ...)
        },
        error = function(e) {
            if (
                grepl("PrepSCTFindMarkers", e$message) && isTRUE(recorrect_umi)
            ) {
                log$warn(
                    "{log_prefix}Still failing about PrepSCTFindMarkers, try recorrect_umi = FALSE"
                )
                find_markers(recorrect_umi = FALSE, ...)
            } else if (inherits(e, "error")) {
                stop(traceback(e))
            } else {
                log$warn(
                    "{log_prefix}Failed to run DE analysis: {e$message}, returning empty result"
                )
                empty
            }
        }
    )

    if (nrow(degs) > 0) {
        # Fix +Inf/-Inf avg_log2FC to make sure visualization works
        # +Inf -> max(abs(is.finite(avg_log2FC))) + 1
        # -Inf -> -max(abs(is.finite(avg_log2FC))) - 1
        # When there is no finite value, set to 9/-9
        finite_vals <- degs$avg_log2FC[is.finite(degs$avg_log2FC)]
        if (length(finite_vals) > 0) {
            max_finite <- max(abs(finite_vals))
            degs$avg_log2FC[degs$avg_log2FC == Inf] <- max_finite + 1
            degs$avg_log2FC[degs$avg_log2FC == -Inf] <- -max_finite - 1
        } else {
            degs$avg_log2FC[degs$avg_log2FC == Inf] <- 9
            degs$avg_log2FC[degs$avg_log2FC == -Inf] <- -9
        }
    }

    cached$save(degs)
    degs
}
#' Ensure marker genes are in the scale.data layer of an assay of the Seurat object
#'
#' @param object Seurat object
#' @param features Character vector, or a list of character vectors, of feature names to ensure in the scale.data layer
#' @param assay Assay to use. If NULL, the default assay will be used.
#' @param umi_assay Assay to use for the UMI counts. Default is "RNA". This is used to get the counts for scaling.
#' @return The Seurat object with the features ensured in the scale.data layer
#' @importFrom SeuratObject DefaultAssay Cells GetAssayData SetAssayData
#' @importFrom Seurat SCTResults
#' @export
EnsureSeuratScaleData <- function(
    object,
    features,
    assay = NULL,
    umi_assay = "RNA"
) {
    assay <- assay %||% DefaultAssay(object)
    is_sct <- inherits(object[[assay]], "SCTAssay")
    # features can be a (named) list of feature groups, e.g. cell-type markers
    features <- if (is.list(features)) unlist(features, use.names = FALSE) else features
    missing <- setdiff(unique(features), rownames(suppressWarnings(GetAssayData(
        object, assay = assay, layer = "scale.data"
    ))))
    missing <- setdiff(missing, colnames(object@meta.data))
    log <- log %||% get_logger()
    # Merge new data into the existing scale.data, keeping the original rows
    # and NA-filling the cells not covered by the new data.
    merge_scale_data <- function(newdata) {
        newdata <- if (is.list(newdata)) newdata else list(newdata)
        scale <- suppressWarnings(GetAssayData(object = object, layer = "scale.data", assay = assay))
        allfeat <- union(rownames(scale), union(missing, unlist(lapply(newdata, rownames))))
        newscale <- matrix(
            NA_real_, length(allfeat), ncol(object),
            dimnames = list(allfeat, Cells(object))
        )
        if (nrow(scale) > 0) {
            newscale[rownames(scale), ] <- as.matrix(scale)
        }
        for (r in newdata) {
            newscale[rownames(r), colnames(r)] <- r
        }
        suppressWarnings(SetAssayData(object = object, assay = assay, layer = "scale.data", new.data = newscale))
    }
    if (length(missing) == 0) {
        return(object)
    }
    if (is_sct) {
        models <- levels(object[[assay]])
        # GetResidual crashes (Seurat bug) when an SCT model covers only
        # part of the requested features (na.rm=TRUE silently drops them).
        # Compute residuals per model, restricted to the features each
        # model supports, and NA-fill the cells of models lacking them.
        #
        # The residual functions also require the umi assay recorded in the
        # models (usually "RNA") to be in the object and silently add nothing
        # when it was removed. The SCT assay itself keeps the counts
        # SCTransform was run on, so they stand in for the umi assay in that
        # case.
        umi_missing <- !umi_assay %in% SeuratObject::Assays(object)
        umi <- if (umi_missing) NULL else object[[umi_assay]]
        model_residuals <- function(m) {
            mfeats <- rownames(SCTResults(
                object[[assay]], "feature.attributes", model = m
            ))
            f <- intersect(missing, mfeats)
            if (length(f) == 0) {
                return(NULL)
            }
            mcells <- Cells(slot(object[[assay]], "SCTModel.list")[[m]])
            # Seurat bug: FetchResidualSCTModel crashes when a model has
            # <= 1 cell, because `scale.data[, cells]` drops to a vector
            # and apply(..., 1, anyNA) fails. Skip such models; their
            # cells stay NA in scale.data.
            if (length(mcells) <= 1) {
                return(NULL)
            }
            if (umi_missing) {
                FetchResidualSCTModel <- utils::getFromNamespace("FetchResidualSCTModel", "Seurat")
                FetchResidualSCTModel(
                    object = object[[assay]],
                    umi.object = object[[assay]],
                    SCTModel = m,
                    layer.cells = mcells,
                    new_features = f
                )
            } else if (inherits(umi, "Assay5")) {
                FetchResidualSCTModel <- utils::getFromNamespace("FetchResidualSCTModel", "Seurat")
                FetchResidualSCTModel(
                    object = object[[assay]],
                    umi.object = umi,
                    SCTModel = m,
                    layer.cells = mcells,
                    new_features = f
                )
            } else {
                GetResidualSCTModel <- utils::getFromNamespace("GetResidualSCTModel", "Seurat")
                GetResidualSCTModel(
                    object = object,
                    assay = assay,
                    SCTModel = m,
                    new_features = f,
                    clip.range = NULL,
                    replace.value = FALSE,
                    verbose = FALSE
                )
            }
        }
        residuals <- Filter(Negate(is.null), lapply(models, model_residuals))
        if (length(residuals) > 0) {
            object <- merge_scale_data(residuals)
        }
    } else {
        # `Seurat::ScaleData()` would rebuild the whole `scale.data` layer,
        # destroying the pre-existing rows (including marker genes used by
        # downstream plots). Compute z-scores for the missing features only
        # and merge them into the existing layer.
        data <- GetAssayData(object = object, layer = "data", assay = assay)
        f <- intersect(missing, rownames(data))
        # Features without a data row (e.g. filtered out after DE) cannot be
        # scaled, and scale.data rows for them cannot be created either.
        nodata <- setdiff(missing, f)
        if (length(nodata) > 0) {
            warning(
                "Cannot scale the following features (not in the data layer): ",
                paste(nodata, collapse = ", "),
                call. = FALSE
            )
            if (length(f) == 0) {
                return(object)
            }
            missing <- f
        }
        newdata <- t(scale(t(as.matrix(data[f, , drop = FALSE]))))
        # Zero-variance features yield NaN; keep the rows, zero them out.
        newdata[!is.finite(newdata)] <- 0
        object <- merge_scale_data(newdata)
    }
    return(object)
}
