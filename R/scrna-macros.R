#' AddSeuratCommand
#' Expand a single numer into 1:number
#'
#' This is useful to expand args$dims = 10 to args$dims = 1:10
#'
#' @param x A numeric vector
#' @return A numeric vector
#' @keywords internal
.expand_number <- function(x) {
    if (length(x) == 1 && x > 0) {
        return(1:x)
    }
    x
}

#' Recode the clusters, from 0, 1, 2, ... to x1, x2, x3, ...
#' @param clusters A numeric vector of clusters
#' @return A character vector of recoded clusters
#' @keywords internal
.recode_clusters <- function(clusters, prefix = "c") {
    recode <- function(x) paste0(prefix, as.integer(as.character(x)) + 1)
    clusters <- factor(recode(clusters), levels = recode(levels(clusters)))
    clusters
}

#' Expand the resolution for `FindClusters`
#'
#' So that we can have 0.1:0.4:0.1 to be expanded to
#' 0.1, 0.2, 0.3, 0.4
#'
#' @param resolution A numeric vector or a character vector
#' @return A numeric vector with the expanded resolution
#' @keywords internal
.expand_findclusters_resolution <- function(resolution) {
    expanded_res <- c()
    for (res in resolution) {
        if (is.numeric(res)) {
            expanded_res <- c(expanded_res, res)
        } else {
            # is.character
            parts <- trimws(unlist(strsplit(res, ",")))
            for (part in parts) {
                if (grepl(":", part)) {
                    ps <- trimws(unlist(strsplit(part, ":")))
                    if (length(ps) == 2) {
                        ps <- c(ps, 0.1)
                    }
                    if (length(ps) != 3) {
                        stop("Invalid resolution format: {part}. Expected 2 or 3 parts separated by ':' for a range.")
                    }
                    ps <- as.numeric(ps)
                    expanded_res <- c(expanded_res, seq(ps[1], ps[2], by = ps[3]))
                } else {
                    expanded_res <- c(expanded_res, as.numeric(part))
                }
            }
        }
    }
    # keep the last resolution at last
    rev(unique(rev(round(expanded_res, 2))))
}

#' Get the column name in meta.data that works as identity
#'
#' @param object Seurat object
#' @return The column name in meta.data that works as identity
#' If there are multiple columns matching, return the shortest one.
#' @importFrom SeuratObject Idents
#' @export
#' @examples
#' obj <- SeuratObject::pbmc_small
#' GetIdentityColumn(obj)
#'
#' SeuratObject::Idents(obj) <- "groups"
#' GetIdentityColumn(obj)
GetIdentityColumn <- function(object) {
    candidates <- c()
    for (name in colnames(object@meta.data)) {
        if (!is.character(object@meta.data[[name]]) && !is.factor(object@meta.data[[name]])) next
        if (isTRUE(all.equal(as.character(unname(Idents(object))), as.character(object@meta.data[[name]])))) {
            candidates <- c(candidates, name)
        }
    }
    if (length(candidates) > 0) {
        candidates[which.min(nchar(candidates))]
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
    object, name, call.string = paste0(name, "(object, ...)"),
    params = list(), assay.used = NULL, time.stamp = Sys.time()) {
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
#' @param ... Additional arguments to pass to [Seurat::FindMarkers()]
#' @export
#' @import tidyseurat
#' @importFrom dplyr filter
#' @importFrom Seurat DefaultAssay PrepSCTFindMarkers FindMarkers
#' @examples
#' RunSeuratDEAnalysis(SeuratObject::pbmc_small, "groups", "g1", "g2")
RunSeuratDEAnalysis <- function(
    object, group_by,
    ident_1 = NULL, ident_2 = NULL, assay = NULL, subset = NULL, cache = NULL, error = TRUE, ...) {
    cache <- cache %||% gettempdir()
    cached <- Cache$new(
        list(object, group_by, ident_1, ident_2, subset, ...),
        prefix = "biopipen.utils.RunSeuratDEAnalysis",
        cache_dir = cache,
    )
    if (cached$is_cached()) {
        return(cached$restore())
    }

    stopifnot("'group_by' is not found in the object" = group_by %in% colnames(object@meta.data))
    stopifnot("'ident_1' should be provided when 'ident_2' is provided" = is.null(ident_2) || !is.null(ident_1))

    assay <- assay %||% DefaultAssay(object)

    if (assay == "SCT" && !"PrepSCTFindMarkers" %in% names(object@commands)) {
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
        object <- filter(object, !!parse_expr(subset))
    }
    all_ident <- as.character(sort(unique(object@meta.data[[group_by]])))
    stopifnot("'ident_1' is not found in 'group_by'" = is.null(ident_1) || ident_1 %in% all_ident)
    stopifnot("'ident_2' is not found in 'group_by'" = is.null(ident_2) || ident_2 %in% all_ident)
    # https://satijalab.org/seurat/archive/v4.3/sctransform_v2_vignette#identify-differential-expressed-genes-across-conditions
    recorrect_umi <- is.null(subset) && assay == "SCT"

    find_markers <- function(recorrect_umi, ...) {
        if (is.null(ident_1)) {
            logic_error <- NULL
            degs <- do_call(rbind, lapply(all_ident, function(ident) {
                m <- tryCatch({
                    FindMarkers(object, group.by = group_by, ident.1 = ident, recorrect_umi = recorrect_umi, assay = assay, ...)
                }, error = function(e) {
                    if (grepl("has fewer than", e$message)) {
                        warning("[RunSeuratDEAnalysis] Skipping DE analysis for group_by = ", group_by, ", ident_1 = ", ident, ": ", e$message, immediate. = TRUE)
                        return(empty[0, , drop = FALSE])
                    } else {
                        logic_error <<- paste0(e$message, " (group_by = ", group_by, ", ident_1 = ", ident, ")")
                        return(empty[0, , drop = FALSE])
                    }
                })
                if (nrow(m) == 0) {
                    return(m)
                }
                m$gene <- rownames(m)
                rownames(m) <- NULL
                m[[group_by]] <- ident
                m
            }))
            if (!is.null(logic_error)) {
                stop(logic_error)
            }
        } else {
            degs <- FindMarkers(object,
                group.by = group_by, ident.1 = ident_1, ident.2 = ident_2, recorrect_umi = recorrect_umi,
                assay = assay, ...
            )
            degs$gene <- rownames(degs)
            degs[[group_by]] <- ifelse(!is.null(ident_2), paste0(ident_2, ":", ident_1), ident_1)
        }
        class(degs) <- c("SeuratDEAnalysis", class(degs))

        degs
    }

    degs <- tryCatch(
        {
            find_markers(recorrect_umi, ...)
        },
        error = function(e) {
            if (grepl("PrepSCTFindMarkers", e$message) && isTRUE(recorrect_umi)) {
                warning("[RunSeuratDEAnalysis] Still failing about PrepSCTFindMarkers, try recorrect_umi = FALSE", immediate. = TRUE)
                find_markers(recorrect_umi = FALSE, ...)
            } else if (error) {
                stop(traceback(e))
            } else {
                warning("[RunSeuratDEAnalysis] Failed to run DE analysis: ", e$message, immediate. = TRUE)
                empty
            }
        }
    )

    cached$save(degs)
    degs
}

#' Perform cell QC
#'
#' @param object Seurat object
#' @param cell_qc Cell QC criteria.
#' It is an expression string to pass to `dplyr::filter` function to filter the cells.
#' It can also be a list of expressions, where the names of the list are sample names.
#' You can have a default expression in the list with the name "DEFAULT" for the samples
#' that are not listed.
#' @return The Seurat object with cell QC results in `@misc$cell_qc_df`
#' @importFrom dplyr mutate
#' @importFrom rlang parse_expr
#' @importFrom Seurat PercentageFeatureSet
#' @export
PerformSeuratCellQC <- function(object, cell_qc) {
    object$percent.mt <- PercentageFeatureSet(object, pattern = "^MT-|^Mt-|^mt-")
    object$percent.ribo <- PercentageFeatureSet(object, pattern = "^RP[SL]|^Rp[sl]")
    object$percent.hb <- PercentageFeatureSet(object, pattern = "^HB[^P]|^Hb[^p]")
    object$percent.plat <- PercentageFeatureSet(object, pattern = "PECAM1|PF4|Pecam1|Pf4")

    # cols <- c("Sample", "nFeature_RNA", "nCount_RNA", "percent.mt", "percent.ribo", "percent.hb", "percent.plat", ".QC")
    if (!"Sample" %in% colnames(object@meta.data)) {
        object@meta.data$Sample <- object@project.name
    }

    sam <- object@project.name
    if (is.list(cell_qc)) {
        cell_qc <- cell_qc[[sam]] %||% cell_qc$DEFAULT %||% NULL
    }
    if (is.null(cell_qc) || is.na(cell_qc) || length(cell_qc) == 0) {
        object$.QC <- TRUE
        # object@misc$cell_qc_df <- object@meta.data[, cols, drop = FALSE]
    } else {
        object@meta.data <- mutate(object@meta.data, .QC = !!parse_expr(cell_qc))
        # object@misc$cell_qc_df <- object@meta.data[, cols, drop = FALSE]
        # object <- subset(object, subset = !!sym(".QC"))
    }

    return(object)
}

#' Perform gene QC
#'
#' @param object Seurat object of a single sample.
#' @param gene_qc Gene QC criteria
#' @return A data frame with Sample (`object@project.name`),
#' Count (number of cells a gene is expressed in) and QC (whether the gene passes the QC)
#'
#' @importFrom SeuratObject GetAssayData
#' @export
PerformGeneQC <- function(object, gene_qc) {
    counts <- Matrix::rowSums(GetAssayData(object, layer = "counts") > 0)
    if (!is.null(gene_qc$min_cells) && gene_qc$min_cells > 0) {
        passes <- counts >= gene_qc$min_cells
    } else {
        passes <- rep(TRUE, length(counts))
    }

    genes <- names(counts)
    if (!is.null(gene_qc$excludes) && length(gene_qc$excludes) > 0) {
        if (length(gene_qc$excludes) == 1) {
            excludes <- trimws(unlist(strsplit(gene_qc$excludes, ",")))
        } else {
            excludes <- gene_qc$excludes
        }
        passes <- passes & !grepl(paste(excludes, collapse = "|"), genes)
    }

    data.frame(
        Sample = object@project.name,
        Feature = genes,
        Count = counts,
        QC = passes,
        stringsAsFactors = FALSE
    )
}

#' Load samples into a Seurat object
#'
#' Cell QC will be performed, either per-sample or on the whole object
#' @param meta Metadata of the samples
#' Required columns: Sample, RNAData.
#' The RNAData column should contain the path to the 10X data, either a directory or a file
#' If the path is a directory, the function will look for barcodes.tsv.gz, features.tsv.gz and matrix.mtx.gz.
#' The directory should be loaded by [Seurat::Read10X]. Sometimes, there may be prefix in the file names,
#' e.g. "'prefix'.barcodes.tsv.gz", which is also supported.
#' If the path is a file ending with ".loom", it will be loaded by [SeuratDisk::Connect()] and converted to a Seurat object.
#' Otherwise, if the path is a file, it should be a h5 file that can be loaded by [Seurat::Read10X_h5()]
#'
#' This can also be a Seurat object to perform QC on. It requires the "Sample" column in the meta.data slot
#' specifying the sample for each cell.
#' @param min_cells Include features detected in at least this many cells.
#' This will be applied to all samples and passed to the [Seurat::CreateSeuratObject()] function.
#' QCs can be further performed on the object after loading.
#' You can also provide a list of values, where the names of the list are sample names
#' and the values are the minimum number of cells for each sample to load by
#' [Seurat::CreateSeuratObject()].
#' You can have a default value in the list with the name "DEFAULT" for the samples
#' that are not listed.
#' This won't work if data is loaded from a loom file or `meta` is a Seurat object.
#' @param min_features Include cells where at least this many features are detected.
#' This will be applied to all samples and passed to the [Seurat::CreateSeuratObject()] function.
#' QCs can be further performed on the object after loading.
#' You can also provide a list of values, where the names of the list are sample names
#' and the values are the minimum number of features for each sample to load by
#' [Seurat::CreateSeuratObject()].
#' You can have a default value in the list with the name "DEFAULT" for the samples
#' that are not listed.
#' This won't work if data is loaded from a loom file or `meta` is a Seurat object.
#' @param samples Samples to load. If NULL, all samples will be loaded
#' @param cell_qc Cell QC criteria, a string of expression to pass to `dplyr::filter` function
#' to filter the cells.
#' It can also be a list of expressions, where the names of the list are sample names.
#' You can have a default expression in the list with the name "DEFAULT" for the samples
#' that are not listed.
#' @param gene_qc Gene QC criteria
#' A list containing the following fields:
#' * min_cells: Minimum number of cells a gene should be expressed in to be kept
#' * excludes: A string or strings to exclude certain genes. Regular expressions are supported.
#' Multiple strings can also be separated by commas in a single string.
#' @param LoadLoomArgs Arguments to pass to [SeuratDisk::LoadLoom()] when loading loom files.
#' @param tmpdir Temporary directory to store intermediate files when there are prefix in the file names
#' @param log Logger
#' @param cache Directory to cache the results. Set to `FALSE` to disable caching
#' @return A Seurat object.
#' In slot `misc`, there are two fields: `cell_qc_df` and `gene_qc`, a list of gene QC results
#' @importFrom bracer glob
#' @importFrom rlang sym
#' @importFrom dplyr filter group_by summarise
#' @importFrom Seurat CreateSeuratObject RenameCells Read10X Read10X_h5 SplitObject
#' @importFrom SeuratObject UpdateSeuratObject
#' @export
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
#' obj <- LoadSeuratAndPerformQC(meta, cache = FALSE, gene_qc = list(min_cells = 3))
#' head(obj@misc$cell_qc_df)
#' print(obj@misc$gene_qc)
#' }
LoadSeuratAndPerformQC <- function(
    meta,
    min_cells = 0,
    min_features = 0,
    samples = NULL,
    cell_qc = NULL,
    gene_qc = NULL,
    LoadLoomArgs = list(),
    tmpdir = NULL,
    log = NULL,
    cache = NULL) {
    log <- log %||% get_logger()

    cache <- cache %||% gettempdir()
    cached <- Cache$new(
        list(meta, min_cells, min_features, samples, cell_qc, gene_qc, LoadLoomArgs),
        prefix = "biopipen.utils.LoadSeuratAndPerformQC",
        cache_dir = cache
    )
    if (cached$is_cached()) {
        log$info("Initialized and QC'ed data loaded from cache")
        return(cached$restore())
    }
    is_seurat <- inherits(meta, "Seurat")
    if (!is_seurat) {
        meta <- as.data.frame(meta)
        samples <- samples %||% meta$Sample
    } else {
        meta <- UpdateSeuratObject(meta)
        samples <- samples %||% unique(meta@meta.data$Sample)
        # convert assays to v5 if they are not
        assay <- DefaultAssay(meta)
        if (!"Assay5" %in% class(meta[[assay]])) {
            log$debug("Converting assay '{assay}' to Assay5 ...")
            meta[[assay]] <- as(meta[[assay]], "Assay5")
        }
    }
    stopifnot("No samples found" = length(samples) > 0)

    log$info("Loading each sample ...")
    tmpdir <- tmpdir %||% gettempdir()
    dig <- digest(capture.output(utils::str(meta)), algo = "md5")
    dig <- substr(dig, 1, 8)
    tmpdir <- file.path(tmpdir, paste0("biopipen.utils.LoadSeuratSamples.", dig))

    object_list <- list()
    geneqc_df <- NULL
    for (sam in samples) {
        log$info("- Loading {sam} and performing QC ...")
        if (is_seurat) {
            obj <- filter(meta, !!sym("Sample") == sam)
            if (nrow(obj@meta.data) == 0) {
                log$warn("  No cells found for sample '{sam}', skipping ...")
                next
            }
        } else {
            mdata <- meta[meta$Sample == sam, , drop = TRUE]
            if (is.data.frame(mdata) && nrow(mdata) == 0) {
                log$warn("  No metadata found for sample '{sam}', skipping ...")
                next
            }


            path <- as.character(mdata$RNAData)
            if (is.na(path) || !is.character(path) || identical(path, "") || identical(path, "NA")) {
                log$warn("  No path found, skipping ...")
                next
            }

            if (dir.exists(path)) {
                exprs <- tryCatch(
                    {
                        Read10X(data.dir = path)
                    },
                    error = function(e) {
                        tmpdatadir <- file.path(tmpdir, slugify(sam))
                        if (dir.exists(tmpdatadir)) {
                            unlink(tmpdatadir, recursive = TRUE)
                        }
                        dir.create(tmpdatadir, recursive = TRUE, showWarnings = FALSE)
                        barcodefile <- Sys.glob(file.path(path, "*barcodes.tsv.gz"))[1]
                        file.symlink(normalizePath(barcodefile), file.path(tmpdatadir, "barcodes.tsv.gz"))
                        genefile <- glob(file.path(path, "*{genes,features}.tsv.gz"))[1]
                        file.symlink(normalizePath(genefile), file.path(tmpdatadir, "features.tsv.gz"))
                        matrixfile <- Sys.glob(file.path(path, "*matrix.mtx.gz"))[1]
                        file.symlink(normalizePath(matrixfile), file.path(tmpdatadir, "matrix.mtx.gz"))
                        Read10X(data.dir = tmpdatadir)
                    }
                )
            } else if (endsWith(path, ".loom")) {
                LoadLoomArgs$file <- path
                exprs <- do_call(SeuratDisk::LoadLoom, LoadLoomArgs)
            } else if (file.exists(path)) {
                exprs <- Read10X_h5(path)
            } else {
                stop("[LoadSeuratSample] {sam}: Path not found: {path}")
            }

            if ("Gene Expression" %in% names(exprs) && !inherits(exprs, "Seurat")) {
                exprs <- exprs[["Gene Expression"]]
            }

            minc <- if (is.list(min_cells)) {
                min_cells[[sam]] %||% min_cells$DEFAULT %||% 0
            } else {
                min_cells
            }
            minf <- if (is.list(min_features)) {
                min_features[[sam]] %||% min_features$DEFAULT %||% 0
            } else {
                min_features
            }
            if (inherits(exprs, "Seurat")) {
                obj <- exprs
            } else {
                obj <- CreateSeuratObject(exprs, project = sam, min.cells = minc, min.features = minf)
            }
            obj <- RenameCells(obj, add.cell.id = sam)
            # Attach meta data
            for (mname in names(mdata)) {
                if (mname %in% c("RNAData", "TCRData", "BCRData")) {
                    next
                }
                mdt <- mdata[[mname]]
                if (is.factor(mdt)) {
                    mdt <- levels(mdt)[mdt]
                }

                obj[[mname]] <- mdt
            }
        }

        # cell qc
        obj <- PerformSeuratCellQC(obj, cell_qc)
        if (!is.null(gene_qc) && length(gene_qc) > 0) {
            # Sample, Feature, Count, QC
            geneqc_df <- rbind(geneqc_df, PerformGeneQC(obj, gene_qc))
        }

        object_list[[sam]] <- obj
    }

    log$info("Merging samples ...")
    obj <- Reduce(merge, object_list)
    rm(object_list)
    gc()

    obj@misc$gene_qc <- geneqc_df
    obj <- AddSeuratCommand(
        obj, "LoadSeuratAndPerformQC",
        "LoadSeuratAndPerformQC(meta, samples, per_sample_qc, cell_qc, gene_qc, tmpdir, log, cache)"
    )
    cached$save(obj)
    obj
}

#' Finish the QC process including the visualization
#'
#' This will remove the cells and genes that are not passing the QC, and also remove
#' the intermediate data used for QC visualization.
#'
#' @param object Seurat object
#' @return The Seurat object with the QC process finished
#' @export
FinishSeuratQC <- function(object) {
    if (!".QC" %in% colnames(object@meta.data)) {
        stop("[FinishSeuratQC] No cell QC data found, the object must be loaded with `LoadSeuratAndPerformQC`")
    }

    features <- NULL
    if (!is.null(object@misc$gene_qc)) {
        failed_features <- unique(object@misc$gene_qc[!object@misc$gene_qc$QC, "Feature"])
        features <- setdiff(unique(object@misc$gene_qc$Feature), failed_features)
    }

    object <- subset(object, subset = !!sym(".QC"), features = features)
    object@meta.data$.QC <- NULL
    object@misc$gene_qc <- NULL
    gc()

    # Add the command to the object, using the time.stamp of LoadSeuratAndPerformQC
    # to keep the same time.stamp, so that later processes can be cached
    # and reused
    if (!is.null(object@commands) && !is.null(object@commands$LoadSeuratAndPerformQC)) {
        time.stamp <- object@commands$LoadSeuratAndPerformQC@time.stamp
    } else {
        time.stamp <- Sys.time()
    }
    AddSeuratCommand(object, "FinishSeuratQC", "FinishSeuratQC(object)", time.stamp = time.stamp)
}

#' Run transformations on a Seurat object
#'
#' @param object Seurat object
#' @param use_sct Whether to use [Seurat::SCTransform]
#' @param SCTransformArgs Arguments to pass to [Seurat::SCTransform]
#' @param NormalizeDataArgs Arguments to pass to [Seurat::NormalizeData]
#' @param FindVariableFeaturesArgs Arguments to pass to [Seurat::FindVariableFeatures]
#' @param ScaleDataArgs Arguments to pass to [Seurat::ScaleData]
#' @param RunPCAArgs Arguments to pass to [Seurat::RunPCA]
#' @param log Logger
#' @param cache Directory to cache the results. Set to `FALSE` to disable caching
#' @return The transformed Seurat object
#' @export
#' @importFrom Seurat SCTransform NormalizeData FindVariableFeatures ScaleData RunPCA
#' @examples
#' \donttest{
#' RunSeuratTransformation(SeuratObject::pbmc_small)
#' }
RunSeuratTransformation <- function(
    object,
    use_sct = FALSE,
    SCTransformArgs = list(),
    NormalizeDataArgs = list(),
    FindVariableFeaturesArgs = list(),
    ScaleDataArgs = list(),
    RunPCAArgs = list(),
    log = NULL,
    cache = NULL) {
    log <- log %||% get_logger()
    cache <- cache %||% gettempdir()
    cached <- Cache$new(
        list(
            object, use_sct,
            SCTransformArgs,
            NormalizeDataArgs,
            FindVariableFeaturesArgs,
            ScaleDataArgs,
            RunPCAArgs
        ),
        prefix = "biopipen.utils.RunSeuratTransformations",
        cache_dir = cache
    )
    if (cached$is_cached()) {
        log$info("Transformed data loaded from cache")
        return(cached$restore())
    }

    log$info("Performing data transformation and scaling ...")
    # Not joined yet
    # object[["RNA"]] <- split(object[["RNA"]], f = object$Sample)
    if (use_sct) {
        log$info("- Running SCTransform ...")
        # log to stdout but don't populate it to running log
        log$debug("  Arguments: {format_args(SCTransformArgs)}")
        SCTransformArgs$object <- object
        object <- do_call(SCTransform, SCTransformArgs)
        SCTransformArgs$object <- NULL
        gc()
    } else {
        log$info("- Running NormalizeData ...")
        log$debug("  Arguments: {format_args(NormalizeDataArgs)}")
        NormalizeDataArgs$object <- object
        object <- do_call(NormalizeData, NormalizeDataArgs)
        NormalizeDataArgs$object <- NULL
        gc()

        log$info("- Running FindVariableFeatures ...")
        log$debug("  Arguments: {format_args(FindVariableFeaturesArgs)}")
        FindVariableFeaturesArgs$object <- object
        object <- do_call(FindVariableFeatures, FindVariableFeaturesArgs)
        FindVariableFeaturesArgs$object <- NULL
        gc()

        log$info("- Running ScaleData ...")
        log_debug("  Arguments: {format_args(ScaleDataArgs)}")
        ScaleDataArgs$object <- object
        object <- do_call(ScaleData, ScaleDataArgs)
        ScaleDataArgs$object <- NULL
        gc()
    }

    log$info("- Running RunPCA ...")
    RunPCAArgs$npcs <- min(RunPCAArgs$npcs %||% 50, ncol(object) - 1, nrow(object) - 1)

    log$debug("  RunPCA: {format_args(RunPCAArgs)}")
    RunPCAArgs$object <- object
    object <- do_call(RunPCA, RunPCAArgs)
    RunPCAArgs$object <- NULL
    gc()

    object <- AddSeuratCommand(
        object,
        name = "RunSeuratTransformations",
        call.string = "RunSeuratTransformations(object, use_sct, SCTransformArgs, NormalizeDataArgs, FindVariableFeaturesArgs, ScaleDataArgs, RunPCAArgs)",
        params = list(
            use_sct = use_sct,
            SCTransformArgs = SCTransformArgs,
            NormalizeDataArgs = NormalizeDataArgs,
            FindVariableFeaturesArgs = FindVariableFeaturesArgs,
            ScaleDataArgs = ScaleDataArgs,
            RunPCAArgs = RunPCAArgs
        )
    )
    cached$save(object)
    object
}

#' Run seurat UMAP
#'
#' In additional to [Seurat::RunUMAP()], we provide an additional arguments to use
#' markers as features for UMAP, which makes the UMAP more separated for different clusters.
#'
#' @param object Seurat object
#' @param RunUMAPArgs Arguments to pass to [Seurat::RunUMAP()].
#' `RunUMAPArgs$features` can be a character vector of features directly used for UMAP,
#' or a list with the following fields:
#' * `order`: The order of the markers to use for UMAP, e.g. "desc(abs(avg_log2FC))"
#' * `n`: The number of total features to use for UMAP, e.g. 30
#' If `RunUMAPArgs$features` is a list, it will run [RunSeuratDEAnalysis()] to get the markers
#' for each group, and then select the top `n`/`ngroups` features for each group
#' based on the `order` field.
#' @param cache Directory to cache the all markers, which can be reused later if
#' DE analysis is desired.
#' @param log The logger to use. If NULL, a default logger will be used.
#' @return The Seurat object with UMAP results
#' @importFrom Seurat RunUMAP
#' @importFrom SeuratObject Idents
#' @importFrom rlang %||% parse_expr
#' @importFrom dplyr arrange group_by slice_head pull
RunSeuratUMAP <- function(object, RunUMAPArgs = list(), cache = NULL, log = NULL) {
    log <- log %||% get_logger()
    if (is.list(RunUMAPArgs$features)) {
        log$info("  Running RunSeuratDEAnalysis to get markers for UMAP ...")
        object$Identity <- Idents(object)
        markers <- RunSeuratDEAnalysis(object, group_by = "Identity", cache = cache)
        attr(markers, "object") <- NULL
        gc()

        RunUMAPArgs$features$order <- RunUMAPArgs$features$order %||% "desc(abs(avg_log2FC))"
        RunUMAPArgs$features$n <- RunUMAPArgs$features$n %||% 30

        ngroups <- length(unique(markers$Identity))
        each_n <- ceiling(RunUMAPArgs$features$n / ngroups)
        RunUMAPArgs$features <- markers %>%
            arrange(!!parse_expr(RunUMAPArgs$features$order)) %>%
            group_by(!!sym("Identity")) %>%
            slice_head(n = each_n) %>%
            pull("gene") %>%
            unique()

        rm(markers)
        gc()
    }
    ncells <- ncol(object)
    if (!is.null(RunUMAPArgs$features) && !is.null(RunUMAPArgs$dims)) {
        log$warn("  'RunUMAPArgs$features' and 'RunUMAPArgs$dims' are both set, 'RunUMAPArgs$dims' will be ignored")
    } else if (is.null(RunUMAPArgs$features)) {
        RunUMAPArgs$dims <- RunUMAPArgs$dims %||% 1:min(30, ceiling(ncells / 3))
        RunUMAPArgs$dims <- .expand_number(RunUMAPArgs$dims)
    }
    RunUMAPArgs$umap.method <- RunUMAPArgs$umap.method %||% "uwot"
    if (RunUMAPArgs$umap.method == "uwot") {
        # https://github.com/satijalab/seurat/issues/4312
        RunUMAPArgs$n.neighbors <- RunUMAPArgs$n.neighbors %||% min(ncells - 1, 30)
    }
    RunUMAPArgs$object <- object
    object <- do_call(RunUMAP, RunUMAPArgs)
    RunUMAPArgs$object <- NULL
    gc()

    object <- AddSeuratCommand(
        object,
        name = "RunSeuratUMAP",
        call.string = "RunSeuratUMAP(object, RunUMAPArgs)",
        params = list(RunUMAPArgs = RunUMAPArgs)
    )
    object
}

#' Run seurat unsupervised clustering
#'
#' @param object Seurat object
#' @param RunPCAArgs Arguments to pass to [Seurat::RunPCA()]
#' @param RunUMAPArgs Arguments to pass to [Seurat::RunUMAP()].
#' `RunUMAPArgs$features` can be a character vector of features directly used for UMAP,
#' or a list with the following fields:
#' * `order`: The order of the markers to use for UMAP, e.g. "desc(abs(avg_log2FC))"
#' * `n`: The number of total features to use for UMAP, e.g. 30
#' If `RunUMAPArgs$features` is a list, it will run [RunSeuratDEAnalysis()] to get the markers
#' for each group, and then select the top `n`/`ngroups` features for each group
#' based on the `order` field.
#' @param FindNeighborsArgs Arguments to pass to [Seurat::FindNeighbors()]
#' @param FindClustersArgs Arguments to pass to [Seurat::FindClusters()]
#' @param log Logger
#' @param cache Directory to cache the results. Set to `FALSE` to disable caching
#' @return The Seurat object with clustering results
#' @export
#' @importFrom Seurat RunUMAP FindNeighbors FindClusters SCTransform NormalizeData
#' @importFrom Seurat FindVariableFeatures ScaleData RunPCA
#' @importFrom SeuratObject DefaultAssay
#' @importFrom rlang %||%
RunSeuratClustering <- function(
    object,
    RunPCAArgs = list(),
    RunUMAPArgs = list(),
    FindNeighborsArgs = list(),
    FindClustersArgs = list(),
    log = NULL,
    cache = NULL) {
    log <- log %||% get_logger()
    cache <- cache %||% gettempdir()

    log$info("Running RunPCA ...")
    caching <- Cache$new(
        list(object, RunPCAArgs),
        prefix = "biopipen.utils.RunSeuratClustering.RunPCA",
        cache_dir = cache
    )

    ncells <- ncol(object)
    if (caching$is_cached()) {
        log$info("PCA results loaded from cache")
        object <- caching$restore()
    } else {
        log$debug("  Arguments: {format_args(RunPCAArgs)}")
        RunPCAArgs$dims <- RunPCAArgs$dims %||% 1:min(30, ncells - 1)
        RunPCAArgs$dims <- .expand_number(RunPCAArgs$dims)
        # https://github.com/satijalab/seurat/issues/1914#issuecomment-1008728797
        RunPCAArgs$npcs <- min(RunPCAArgs$npcs %||% 50, ncol(object) - 1, nrow(object) - 1)
        RunPCAArgs$object <- object
        object <- do_call(RunPCA, RunPCAArgs)
        RunPCAArgs$object <- NULL
        gc()

        caching$save(object)
    }

    log$info("Running FindNeighbors ...")
    caching <- Cache$new(
        list(object, FindNeighborsArgs),
        prefix = "biopipen.utils.RunSeuratClustering.FindNeighbors",
        cache_dir = cache
    )
    if (caching$is_cached()) {
        log$info("FindNeighbors results loaded from cache")
        object <- caching$restore()
    } else {
        log$debug("  Arguments: {format_args(FindNeighborsArgs)}")
        FindNeighborsArgs$reduction <- FindNeighborsArgs$reduction %||% object@misc$integrated_new_reduction %||% "pca"
        if (!is.null(FindNeighborsArgs$dims)) {
            FindNeighborsArgs$dims <- .expand_number(FindNeighborsArgs$dims)
        }
        FindNeighborsArgs$object <- object
        object <- do_call(FindNeighbors, FindNeighborsArgs)
        FindNeighborsArgs$object <- NULL
        gc()

        caching$save(object)
    }

    log$info("Running FindClusters ...")
    caching <- Cache$new(
        list(object, FindClustersArgs),
        prefix = "biopipen.utils.RunSeuratClustering.FindClusters",
        cache_dir = cache
    )
    if (caching$is_cached()) {
        log$info("FindClusters results loaded from cache")
        object <- caching$restore()
    } else {
        log$debug("  Arguments: {format_args(FindClustersArgs)}")

        # FindClustersArgs$graph.name <- FindClustersArgs$graph.name %||% "RNA_snn"
        FindClustersArgs$object <- object
        FindClustersArgs$random.seed <- FindClustersArgs$random.seed %||% 8525
        FindClustersArgs$resolution <- .expand_findclusters_resolution(FindClustersArgs$resolution %||% 0.8)
        cluster_name <- FindClustersArgs$cluster.name %||% "seurat_clusters"
        if (length(cluster_name) > 1) {
            stop("[RunSeuratClustering] 'FindClustersArgs$cluster.name' only supports a single name")
        }
        FindClustersArgs$cluster.name <- paste0(cluster_name, ".", FindClustersArgs$resolution)
        # FindClustersArgs$cluster.name[length(FindClustersArgs$cluster.name)] <- cluster_name
        log$info("  Using resolution(s): {paste(FindClustersArgs$resolution, collapse = ', ')}")
        object <- do_call(FindClusters, FindClustersArgs)
        FindClustersArgs$object <- NULL
        gc()

        for (clname in FindClustersArgs$cluster.name) {
            object@meta.data[[clname]] <- .recode_clusters(object@meta.data[[clname]])
        }
        object@meta.data[[cluster_name]] <- object@meta.data[[FindClustersArgs$cluster.name[length(FindClustersArgs$cluster.name)]]]
        # object@meta.data$seurat_clusters <- .recode_clusters(object@meta.data$seurat_clusters)
        Idents(object) <- cluster_name

        ident_table <- table(object@meta.data[[cluster_name]])
        ident_table <- paste0(names(ident_table), "(", ident_table, ")")
        log$info("  Found clusters (with resolution {FindClustersArgs$resolution[length(FindClustersArgs$resolution)]}):")
        # log every 5 clusters
        for (i in seq(1, length(ident_table), by = 5)) {
            log$info("   | {paste(ident_table[i:min(i + 4, length(ident_table))], collapse = ', ')}")
        }

        caching$save(object)
    }

    log$info("Running RunUMAP ...")
    caching <- Cache$new(
        list(object, RunUMAPArgs),
        prefix = "biopipen.utils.RunSeuratClustering.RunSeuratUMAP",
        cache_dir = cache
    )
    if (caching$is_cached()) {
        log$info("UMAP results loaded from cache")
        object <- caching$restore()
    } else {
        log$debug("  Arguments: {format_args(RunUMAPArgs)}")
        object <- RunSeuratUMAP(
            object,
            RunUMAPArgs = RunUMAPArgs,
            cache = cache,
            log = log
        )

        caching$save(object)
    }

    object
}

#' Run subset clustering on a Seurat object
#'
#' It's unlike [`Seurat::FindSubCluster`], which only finds subclusters of a single
#' cluster. Instead, it will perform the whole clustering procedure on the subset of
#' cells. One can use metadata to specify the subset of cells to perform clustering on.
#'
#' @param object Seurat object
#' @param subset A string of expression to pass to `dplyr::filter` function to filter the cells.
#' @param name Name of the run.
#' It will be used as the prefix for the reduction name, keys and cluster names.
#' For reduction keys, it will be `toupper(<name>)` + "PC_" and `toupper(<name>)` + "UMAP_".
#' For cluster names, it will be `<name>` + "." + resolution.
#' And the final cluster name will be `<name>`.
#' Default is "subcluster".
#' @param RunPCAArgs Arguments to pass to [Seurat::RunPCA()]
#' @param RunUMAPArgs Arguments to pass to [Seurat::RunUMAP()].
#' `RunUMAPArgs$features` can be a character vector of features directly used for UMAP,
#' or a list with the following fields:
#' * `order`: The order of the markers to use for UMAP, e.g. "desc(abs(avg_log2FC))"
#' * `n`: The number of total features to use for UMAP, e.g. 30
#' If `RunUMAPArgs$features` is a list, it will run [RunSeuratDEAnalysis()] to get the markers
#' for each group, and then select the top `n`/`ngroups` features for each group
#' based on the `order` field.
#' @param FindNeighborsArgs Arguments to pass to [Seurat::FindNeighbors()]
#' @param FindClustersArgs Arguments to pass to [Seurat::FindClusters()]
#' @param log Logger
#' @param cache Directory to cache the results. Set to `FALSE` to disable caching
#' @return The original Seurat object (not the subsetted one) with the subclusters
#' results in `@meta.data` and `@reductions`.
#' @export
#' @importFrom Seurat RunUMAP FindNeighbors FindClusters SCTransform NormalizeData
#' @importFrom Seurat FindVariableFeatures ScaleData RunPCA
#' @importFrom SeuratObject DefaultAssay Reductions
#' @importFrom rlang %||%
#' @importFrom dplyr filter
#' @examples
#' \donttest{
#' obj <- SeuratObject::pbmc_small
#' # Just run UMAP to compare with the subclusters
#' obj <- suppressMessages(Seurat::RunUMAP(obj, dims = 1:10))
#' obj <- suppressWarnings(suppressMessages(RunSeuratSubClustering(
#'    obj, subset = "groups == 'g1'", name = "g1subcluster"
#' )))
#'
#' scplotter::CellDimPlot(
#'     obj, reduction = "umap", group_by = "groups",
#'     title = "UMAP of all cells",
#'     subtitle = "groups = g1 will be subclustered"
#' )
#' scplotter::CellDimPlot(
#'     obj, reduction = "umap", group_by = "g1subcluster",
#'     title = "Subclusters on the original UMAP"
#' )
#' scplotter::CellDimPlot(obj,
#'     reduction = "g1subcluster.umap", group_by = "g1subcluster"
#' )
#' }
RunSeuratSubClustering <- function(
    object,
    subset,
    name = "subcluster",
    RunPCAArgs = list(),
    RunUMAPArgs = list(),
    FindNeighborsArgs = list(),
    FindClustersArgs = list(),
    log = NULL,
    cache = NULL) {
    log <- log %||% get_logger()
    cache <- cache %||% gettempdir()
    cached <- Cache$new(
        list(object, subset, name, RunPCAArgs, RunUMAPArgs, FindNeighborsArgs, FindClustersArgs),
        prefix = "biopipen.utils.RunSeuratSubClustering",
        cache_dir = cache
    )
    if (cached$is_cached()) {
        log$info("Subset clustering results loaded from cache")
        return(cached$restore())
    }
    log$info("Subsetting seurat object ...")
    subobj <- filter(object, !!rlang::parse_expr(subset))
    if (ncol(subobj) < 10) {
        stop("[RunSeuratSubClustering] Not enough (< 10) cells to perform clustering")
    }

    if (name %in% colnames(object@meta.data)) {
        stop(paste0("[RunSeuratSubClustering] Name '", name, "' already exists in the metadata. Please choose a different name."))
    }

    log$info("- Running RunPCA ...")
    RunPCAArgs$object <- subobj
    RunPCAArgs$reduction.key <- RunPCAArgs$reduction.key %||% paste0(toupper(name), "PC_")
    RunPCAArgs$dims <- RunPCAArgs$dims %||% 1:min(30, ncol(subobj) - 1)
    RunPCAArgs$dims <- .expand_number(RunPCAArgs$dims)
    # https://github.com/satijalab/seurat/issues/1914#issuecomment-1008728797
    RunPCAArgs$npcs <- min(RunPCAArgs$npcs %||% 50, min(ncol(subobj), nrow(subobj)) - 1)
    log$debug("  Arguments: {format_args(RunPCAArgs)}")
    subobj <- do_call(RunPCA, RunPCAArgs)
    RunPCAArgs$object <- NULL
    gc()

    log$info("- Running FindNeighbors ...")
    FindNeighborsArgs$object <- subobj
    FindNeighborsArgs$reduction <- FindNeighborsArgs$reduction %||% subobj@misc$integrated_new_reduction %||% "pca"
    if (!is.null(FindNeighborsArgs$dims)) {
        FindNeighborsArgs$dims <- .expand_number(FindNeighborsArgs$dims)
    }
    log$debug("  Arguments: {format_args(FindNeighborsArgs)}")
    subobj <- do_call(FindNeighbors, FindNeighborsArgs)
    FindNeighborsArgs$object <- NULL
    gc()

    log$info("- Running FindClusters ...")
    FindClustersArgs$object <- subobj
    FindClustersArgs$random.seed <- FindClustersArgs$random.seed %||% 8525
    FindClustersArgs$resolution <- .expand_findclusters_resolution(FindClustersArgs$resolution %||% 0.8)
    FindClustersArgs$cluster.name <- paste0(name, ".", FindClustersArgs$resolution)
    # FindClustersArgs$cluster.name[length(FindClustersArgs$cluster.name)] <- name
    log$info("  Using resolution(s): {paste(FindClustersArgs$resolution, collapse = ', ')}")
    log$debug("  Arguments: {format_args(FindClustersArgs)}")
    subobj <- do_call(FindClusters, FindClustersArgs)
    FindClustersArgs$object <- NULL
    gc()

    for (clname in FindClustersArgs$cluster.name) {
        subobj@meta.data[[clname]] <- .recode_clusters(subobj@meta.data[[clname]], prefix = "s")
    }
    subobj@meta.data[[name]] <- subobj@meta.data[[FindClustersArgs$cluster.name[length(FindClustersArgs$cluster.name)]]]
    # subobj@meta.data[[name]] <- .recode_clusters(subobj@meta.data$seurat_clusters, prefix = "s")
    Idents(subobj) <- name

    ident_table <- table(subobj@meta.data[[name]])
    ident_table <- paste0(names(ident_table), "(", ident_table, ")")
    log$info("  Found subclusters (with resolution {FindClustersArgs$resolution[length(FindClustersArgs$resolution)]}):")
    # log every 5 clusters
    for (i in seq(1, length(ident_table), by = 5)) {
        log$info("   | {paste(ident_table[i:min(i + 4, length(ident_table))], collapse = ', ')}")
    }

    log$info("- Running RunUMAP ...")
    RunUMAPArgs$reduction.key <- RunUMAPArgs$reduction.key %||% paste0(toupper(name), "UMAP_")
    log$debug("  Arguments: {format_args(RunUMAPArgs)}")
    subobj <- RunSeuratUMAP(
        subobj,
        RunUMAPArgs = RunUMAPArgs,
        cache = cache,
        log = log
    )

    object <- AddSeuratCommand(
        object,
        name = paste0("RunSeuratSubClustering.", name),
        call.string = "RunSeuratSubClustering(object, subset, name, RunPCAArgs, RunUMAPArgs, FindNeighborsArgs, FindClustersArgs)",
        params = list(
            subset = subset,
            name = name,
            RunPCAArgs = RunPCAArgs,
            RunUMAPArgs = RunUMAPArgs,
            FindNeighborsArgs = FindNeighborsArgs,
            FindClustersArgs = FindClustersArgs
        )
    )

    # Add the subclusters.xxx and subclusters metadata to the original object
    for (clname in FindClustersArgs$cluster.name) {
        object@meta.data[[clname]] <- subobj@meta.data[colnames(object), clname, drop = TRUE]
    }
    object@meta.data[[name]] <- subobj@meta.data[colnames(object), name, drop = TRUE]

    # Add the subclusters reduction to the original object
    object@reductions[[paste0(name, ".pca")]] <- subobj@reductions[["pca"]]
    object@reductions[[paste0(name, ".umap")]] <- subobj@reductions[["umap"]]

    cached$save(object)
    object
}


#' Run data integration on Seurat object
#'
#' @param object Seurat object
#' @param no_integration Whether to skip integration, just join layers
#' @param IntegrateLayersArgs Arguments to pass to [Seurat::IntegrateLayers]
#' @param log Logger
#' @param cache Directory to cache the results. Set to `FALSE` to disable caching
#' @return The Seurat object with integrated data
#' @export
#' @importFrom utils getFromNamespace
#' @importFrom Seurat IntegrateLayers
#' @importFrom rlang %||%
#' @importFrom SeuratObject JoinLayers
RunSeuratIntegration <- function(
    object, no_integration = FALSE, IntegrateLayersArgs = list(),
    log = NULL, cache = NULL) {
    log <- log %||% get_logger()
    cache <- cache %||% gettempdir()
    cached <- Cache$new(
        list(object, no_integration, IntegrateLayersArgs),
        prefix = "biopipen.utils.RunSeuratIntegration",
        cache_dir = cache
    )
    if (cached$is_cached()) {
        log$info("Integrated data loaded from cache")
        return(cached$restore())
    }

    log$info("Performing data integration ...")
    if (!no_integration) {
        method <- IntegrateLayersArgs$method %||% "rpca"
        if (!is.null(IntegrateLayersArgs$reference) && is.character(IntegrateLayersArgs$reference)) {
            log$info("  Using reference samples: {paste(IntegrateLayersArgs$reference, collapse = ', ')}")
            samples <- if (is.factor(object$Sample)) levels(object$Sample) else unique(object$Sample)
            IntegrateLayersArgs$reference <- match(IntegrateLayersArgs$reference, samples)
            log$debug("  Transferred to indices: {paste(IntegrateLayersArgs$reference, collapse = ', ')}")
        }
        log$info("- Running IntegrateLayers (method = {method}) ...")
        method <- switch(method,
            "CCA" = "CCAIntegration",
            "cca" = "CCAIntegration",
            "RPCA" = "RPCAIntegration",
            "rpca" = "RPCAIntegration",
            "Harmony" = "HarmonyIntegration",
            "harmony" = "HarmonyIntegration",
            "FastMNN" = "FastMNNIntegration",
            "fastmnn" = "FastMNNIntegration",
            "scVI" = "scVIIntegration",
            "scvi" = "scVIIntegration",
            stop(paste0("Unknown integration method: ", method))
        )
        IntegrateLayersArgs$method <- getFromNamespace(method, "Seurat")
        IntegrateLayersArgs$assay <- IntegrateLayersArgs$assay %||% DefaultAssay(object)
        if (IntegrateLayersArgs$assay == "SCT") {
            IntegrateLayersArgs$normalization.method <- IntegrateLayersArgs$normalization.method %||% "SCT"
        }

        new_reductions <- list(
            "CCAIntegration" = "integrated.cca",
            "RPCAIntegration" = "integrated.rpca",
            "HarmonyIntegration" = "harmony",
            "FastMNNIntegration" = "integration.mnn",
            "scVIIntegration" = "integrated.scvi"
        )
        IntegrateLayersArgs$new.reduction <- IntegrateLayersArgs$new.reduction %||% new_reductions[[method]]

        log$debug("  Arguments: {format_args(IntegrateLayersArgs)}")
        IntegrateLayersArgs$object <- object
        object <- do_call(IntegrateLayers, IntegrateLayersArgs)
        IntegrateLayersArgs$object <- NULL
        gc()

        # Save it for dimension reduction plots
        object@misc$integrated_new_reduction <- IntegrateLayersArgs$new.reduction
    }

    log$info("- Joining layers ...")
    object <- JoinLayers(object, assay = "RNA")
    object <- AddSeuratCommand(
        object, "RunSeuratIntegration",
        "RunSeuratIntegration(object, no_integration, IntegrateLayersArgs)",
        params = list(
            no_integration = no_integration,
            IntegrateLayersArgs = IntegrateLayersArgs
        )
    )

    cached$save(object)
    object
}

#' Run DoubletFinder on a Seurat object
#'
#' @param object Seurat object
#' @param ncores Number of cores to use
#' @param ident Cluster identity to use for homotypic doublet proportion estimation.
#' If NULL or not exists, will run clustering first to get cluster identities.
#' @param PCs Number of statistically-significant principal components
#' @param pN The number of generated artificial doublets, expressed as a proportion of the merged
#' real-artificial data. Default is set to 0.25, based on observation that DoubletFinder
#' performance is largely pN-invariant (see McGinnis, Murrow and Gartner 2019, Cell Systems).
#' @param doublets The expected proportion of doublets in the dataset. Default is set to 0.075
#' @keywords internal
#' @return The Seurat object with doublet detection results in `@misc$doubletFinder`
#' @importFrom Seurat FindNeighbors FindClusters DefaultAssay AddMetaData
#' @importFrom rlang %||%
#' @importFrom SeuratObject Idents<- Idents
#' @importFrom grDevices pdf dev.off
RunSeuratDoubletFinder <- function(object, ident = NULL, ncores = 1, PCs = 30, pN = 0.25, doublets = 0.075, log = NULL, allow_warnings = FALSE) {
    log <- log %||% get_logger()

    log$info("- Preparing Seurat object ...")
    # More controls from args?
    if (is.null(ident) || !ident %in% colnames(object@meta.data)) {
        if (!is.null(ident)) {
            log$warn("  ident '{ident}' not found in metadata, running clustering to get identities")
        } else {
            log$info("  No ident provided, running clustering to get identities")
        }
        object <- FindNeighbors(object, dims = 1:PCs)
        object <- FindClusters(object)
    } else {
        Idents(object) <- ident
    }

    log$info("- pK Indentification ...")
    if (allow_warnings) {
        sweep.res.list <- DoubletFinder::paramSweep(
            object,
            PCs = 1:PCs,
            sct = identical(DefaultAssay(object), "SCT"),
            num.cores = ncores
        )
    } else {
        sweep.res.list <- suppressWarnings(DoubletFinder::paramSweep(
            object,
            PCs = 1:PCs,
            sct = identical(DefaultAssay(object), "SCT"),
            num.cores = ncores
        ))
    }
    sweep.stats <- DoubletFinder::summarizeSweep(sweep.res.list, GT = FALSE)
    pdf(NULL)
    bcmvn <- DoubletFinder::find.pK(sweep.stats)
    dev.off()
    bcmvn$Selected <- bcmvn$pK == bcmvn$pK[which.max(bcmvn$BCmetric)[1]]

    pK <- bcmvn$pK[which.max(bcmvn$BCmetric)[1]]
    pK <- as.numeric(as.character(pK))
    log$info("- Homotypic Doublet Proportion Estimate ...")

    homotypic.prop <- DoubletFinder::modelHomotypic(Idents(object))
    nExp_poi <- round(nrow(object@meta.data) * doublets)
    nExp_poi.adj <- round(nExp_poi * (1 - homotypic.prop))

    log$info("- Running DoubletFinder ...")
    if (allow_warnings) {
        object <- DoubletFinder::doubletFinder(
            object,
            PCs = 1:PCs,
            pN = pN,
            pK = pK,
            nExp = nExp_poi.adj,
            reuse.pANN = FALSE,
            sct = identical(DefaultAssay(object), "SCT")
        )
    } else {
        object <- suppressWarnings(DoubletFinder::doubletFinder(
            object,
            PCs = 1:PCs,
            pN = pN,
            pK = pK,
            nExp = nExp_poi.adj,
            reuse.pANN = FALSE,
            sct = identical(DefaultAssay(object), "SCT")
        ))
    }
    pANN_col <- paste0("pANN_", pN, "_", pK)
    pANN_col <- colnames(object@meta.data)[grepl(pANN_col, colnames(object@meta.data))]
    DF_col <- paste0("DF.classifications_", pN, "_", pK)
    DF_col <- colnames(object@meta.data)[grepl(DF_col, colnames(object@meta.data))]
    doublets <- object@meta.data[, c(pANN_col, DF_col), drop = FALSE]
    colnames(doublets) <- c("DoubletFinder_score", "DoubletFinder_DropletType")
    doublets$DoubletFinder_DropletType <- tolower(doublets$DoubletFinder_DropletType)
    doublets$DoubletFinder_DropletType <- factor(doublets$DoubletFinder_DropletType, levels = c("singlet", "doublet"))

    bcmvn$pK <- as.numeric(as.character(bcmvn$pK))
    bcmvn$BCmetric <- as.numeric(bcmvn$BCmetric)

    object <- AddMetaData(object, doublets)
    object@misc$doublets <- list(
        bcmvn = bcmvn,
        tool = "DoubletFinder"
    )

    object
}

#' Run SCDblFinder on a Seurat object
#'
#' @param object Seurat object
#' @param ncores Number of cores to use
#' @param ... Additional arguments to pass to [scDblFinder::scDblFinder]
#' @return The Seurat object with doublet detection results in `@misc$scDblFinder`
#' @keywords internal
#' @importFrom Seurat GetAssayData AddMetaData
RunSeuratScDblFinder <- function(object, ncores = 1, ...) {
    args <- list(returnType = "table", ...)
    args$sce <- GetAssayData(object, layer = "counts")
    if (ncores > 1) {
        args$BPPARAM <- BiocParallel::MulticoreParam(ncores, RNGseed = 8525)
    }

    doublets <- do_call(scDblFinder::scDblFinder, args)
    doublets <- doublets[doublets$type == "real", , drop = FALSE]
    doublets <- doublets[, c("score", "class"), drop = FALSE]
    colnames(doublets) <- c("scDblFinder_score", "scDblFinder_DropletType")
    doublets$scDblFinder_DropletType <- factor(doublets$scDblFinder_DropletType, levels = c("singlet", "doublet"))

    object@misc$doublets <- list(tool = "scDblFinder")
    object <- AddMetaData(object, as.data.frame(doublets))
    object
}

#' Run doublet detection on a Seurat object
#'
#' @param object Seurat object
#' @param tool Doublet detection tool.
#' Either "DoubletFinder" or "scDblFinder"
#' @param DoubletFinderArgs Arguments to pass to [RunSeuratDoubletFinder]
#' @param scDblFinderArgs Arguments to pass to [RunSeuratScDblFinder]
#' @param filter Whether to filter out doublets. Default is TRUE.
#' If you want to visualize the singlets and doublets, set it to FALSE so that the doublets are not filtered out.
#' Then you will need to filter them out manually. For example: `object <- subset(object, subset = DoubletFinder_DropletType != "doublet")`
#' @param log Logger
#' @param cache Directory to cache the results. Set to `FALSE` to disable caching
#' @return The Seurat object with doublet detection results in `@misc$doublets`
#' @export
RunSeuratDoubletDetection <- function(
    object,
    tool = "DoubletFinder",
    DoubletFinderArgs = list(),
    scDblFinderArgs = list(),
    filter = TRUE,
    log = NULL,
    cache = NULL) {
    log <- log %||% get_logger()
    cache <- cache %||% gettempdir()
    cached <- Cache$new(
        list(object, tool, DoubletFinderArgs, scDblFinderArgs, filter),
        prefix = "biopipen.utils.RunSeuratDoubletDetection",
        cache_dir = cache
    )
    if (cached$is_cached()) {
        log$info("Doublet detection results loaded from cache")
        return(cached$restore())
    }

    if (tolower(tool) == "doubletfinder") {
        tool <- "DoubletFinder"
    } else if (tolower(tool) == "scdblfinder") {
        tool <- "scDblFinder"
    } else {
        stop(paste0("Unknown doublet detection tool: ", tool))
    }

    log$info("Running doublet detection using {tool} ...")
    if (tool == "DoubletFinder") {
        DoubletFinderArgs$object <- object
        object <- do_call(RunSeuratDoubletFinder, DoubletFinderArgs)
        DoubletFinderArgs$object <- NULL
        gc()
    } else {
        scDblFinderArgs$object <- object
        object <- do_call(RunSeuratScDblFinder, scDblFinderArgs)
        scDblFinderArgs$object <- NULL
        gc()
    }
    object@misc$doublets$filtered <- filter

    if (isTRUE(filter)) {
        log$info("Filtering out doublets ...")
        if (tool == "DoubletFinder") {
            object <- subset(object, subset = !!sym("DoubletFinder_DropletType") != "doublet")
        } else {
            object <- subset(object, subset = !!sym("scDblFinder_DropletType") != "doublet")
        }
    }

    object <- AddSeuratCommand(
        object, "RunSeuratDoubletDetection",
        "RunSeuratDoubletDetection(object, tool, DoubletFinderArgs, scDblFinderArgs, filter)",
        params = list(
            tool = tool,
            DoubletFinderArgs = DoubletFinderArgs,
            scDblFinderArgs = scDblFinderArgs,
            filter = filter
        )
    )
    cached$save(object)
    object
}

#' Run Seurat MapQuery to reference
#'
#' This will normalize the query object, find transfer anchors, and map the query to the reference.
#' @seealso https://satijalab.org/seurat/articles/integration_mapping.html
#'
#' @param object Seurat object
#' @param ref Reference Seurat object or a file path to a Seurat object with .rds or .h5seurat extension
#' @param use The name in the metadata of the reference to use as identity for the query after mapping
#' @param ident The name of the identity in metadata after mapping
#' @param refnorm The normalization method used by the reference.
#' The same normalization method will be used for the query.
#' * "auto": automatically detect the normalization method used by the reference
#' * "LogNormalize": LogNormalize
#' * "SCT": SCTransform
#' * "SCTransform": SCTransform (alias for SCT)
#' @param skip_if_normalized Skip normalization if the query is already normalized with the same method as the reference
#' @param split_by The name of the metadata to split the query object by and do the mapping separately
#' @param ncores Number of cores to use for parallel processing for the split query objects
#' @param MapQueryArgs Arguments to pass to [Seurat::MapQuery()].
#' The `use` argument will be added to the `refdata` list.
#' @param FindTransferAnchorsArgs Arguments to pass to [Seurat::FindTransferAnchors()].
#' @param SCTransformArgs Arguments to pass to [Seurat::SCTransform()].
#' Will be used to normalize the query object if `refnorm` is set to "SCT"
#' @param NormalizeDataArgs Arguments to pass to [Seurat::NormalizeData()].
#' Will be used to normalize the query object if `refnorm` is set to "LogNormalize"
#' @param log Logger
#' @param cache Directory to cache the results. Set to `FALSE` to disable caching
#' @return The Seurat object with the mapped data
#' @export
#' @importFrom methods slot
#' @importFrom rlang %||%
#' @importFrom SeuratObject UpdateSeuratObject DefaultAssay Idents Misc
#' @importFrom Seurat GetAssay SplitObject MapQuery UpdateSCTAssays Reductions FindTransferAnchors
#' @importFrom Seurat SCTransform NormalizeData MappingScore AddMetaData RunUMAP
#' @importFrom parallel mclapply
RunSeuratMap2Ref <- function(
    object,
    ref,
    use,
    ident = "seurat_clusters",
    refnorm = c("auto", "LogNormalize", "SCT", "SCTransform"),
    skip_if_normalized = TRUE,
    split_by = NULL,
    ncores = 1,
    MapQueryArgs = list(
        # reference.reduction = NULL,
        # reduction.model = NULL,
        refdata = list()
    ),
    FindTransferAnchorsArgs = list(),
    SCTransformArgs = list(),
    NormalizeDataArgs = list(),
    log = NULL,
    cache = NULL) {
    refnorm <- match.arg(refnorm)

    log <- log %||% get_logger()
    cache <- cache %||% gettempdir()
    cached <- Cache$new(
        list(
            object, ref, use, ident, refnorm, skip_if_normalized,
            split_by, ncores, MapQueryArgs, FindTransferAnchorsArgs,
            SCTransformArgs, NormalizeDataArgs
        ),
        prefix = "biopipen.utils.RunSeuratMap2Ref",
        cache_dir = cache
    )
    if (cached$is_cached()) {
        log$info("Mapping-to-reference results loaded from cache")
        return(cached$restore())
    }

    if (is.character(ref) && (endsWith(ref, ".rds") || endsWith(ref, ".RDS") || endsWith(ref, "qs") || endsWith(ref, ".qs2"))) {
        log$info("Loading reference ...")
        reference <- read_obj(ref)
    } else if (is.character(ref) && (endsWith(ref, ".h5seurat") || endsWith(ref, ".H5Seurat"))) {
        log$info("Loading reference ...")
        reference <- SeuratDisk::LoadH5Seurat(ref)
    } else if (inherits(ref, "Seurat")) {
        reference <- ref
    } else {
        stop("[RunSeuratMap2Ref] 'reference' should be a Seurat object or a file path with extension '.qs', 'qs2', '.rds', '.RDS' or '.h5seurat'")
    }
    reference <- UpdateSeuratObject(reference)
    is_sct <- getFromNamespace("IsSCT", "Seurat")
    ref_is_sct <- is_sct(GetAssay(reference))
    if (ref_is_sct) {
        reference <- UpdateSCTAssays(reference)
    }

    MapQueryArgs$refdata <- MapQueryArgs$refdata %||% list()
    MapQueryArgs$refdata[[use]] <- use

    log$info("Checking if given refdata for MapQuery is in the reference ...")
    for (name in names(MapQueryArgs$refdata)) {
        if (!name %in% colnames(reference@meta.data)) {
            stop(paste0(
                "[RunSeuratMap2Ref] items of 'refdata' should be in the reference: ", name, "\n",
                "Available items: ", paste0(colnames(reference@meta.data), collapse = ", ")
            ))
        }
        if (startsWith(name, "predicted.")) {
            stop(paste0(
                "[RunSeuratMap2Ref] items 'refdata' should not start with 'predicted.': ", name,
                " please use 'x' instead of 'predicted.x'"
            ))
        }
    }
    if (is.null(MapQueryArgs$reference.reduction)) {
        MapQueryArgs$reference.reduction <- Reductions(reference)[1]
        log$warn("- `MapQueryArgs$reference.reduction` is not set, using the first reduction in the reference: {MapQueryArgs$reference.reduction}")
    }
    if (!MapQueryArgs$reference.reduction %in% Reductions(reference)) {
        stop(paste0(
            "[RunSeuratMap2Ref] `MapQueryArgs$reference.reduction` is not in the reference: ", MapQueryArgs$reference.reduction, "\n",
            "Available reductions: ", paste0(Reductions(reference), collapse = ", ")
        ))
    }
    MapQueryArgs$reduction.model <- MapQueryArgs$reduction.model %||% "umap"
    # Check if reference has the same reduction model
    if (
        is.null(reference@reductions[[MapQueryArgs$reduction.model]]) ||
            length(x = Misc(object = reference@reductions[[MapQueryArgs$reduction.model]], slot = "model")) == 0
    ) {
        models <- sapply(names(reference@reductions), function(x) {
            ifelse(length(x = Misc(object = reference@reductions[[x]], slot = "model")) > 0, x, NA)
        })
        models <- models[!is.na(models)]
        msg <- paste0(
            "[RunSeuratMap2Ref] `MapQueryArgs$reduction.model` is not in the reference: ", MapQueryArgs$reduction.model, "\n",
            "Try to run the reference with ",
            "RunUMAP(reference, reduction = '", MapQueryArgs$reference.reduction, "', reduction.name = '", MapQueryArgs$reduction.model, "', dims = 1:N, return.model = TRUE)\n",
            "to create the model.\n"
        )
        if (length(models) == 0) {
            stop(msg)
        } else {
            msg <- paste0(msg, "Or choose a different `MapQueryArgs$reduction.model`. Available models: ", paste0(models, collapse = ", "), "\n")
            stop(msg)
        }
    }

    if (refnorm == "auto") {
        refnorm <- ifelse(ref_is_sct, "SCT", "LogNormalize")
    }
    if (refnorm == "SCTransform") {
        refnorm <- "SCT"
    }

    if (refnorm == "SCT") {
        # Check if the reference is SCTransform'ed
        stopifnot(
            "[RunSeuratMap2Ref] reference is not SCTransform'ed, but refnorm is set to SCT" =
                ref_is_sct
        )

        n_models <- length(x = slot(object = reference[[DefaultAssay(reference)]], name = "SCTModel.list"))
        if (n_models == 0) {
            stop("[RunSeuratMap2Ref] reference doesn't contain any SCTModel.")
        }
    }
    FindTransferAnchorsArgs$normalization.method <- refnorm

    defassay <- DefaultAssay(object)

    if (!is.null(split_by)) {
        # check if each split has more than 100 cells
        cellno <- table(object@meta.data[[split_by]])
        cellno <- cellno[cellno < 100]
        if (length(cellno) > 0) {
            # stop and print the splits with # cells
            stop(paste0(
                "[RunSeuratMap2Ref] The following splits have less than 100 cells: \n",
                paste0("- ", names(cellno), ": ", cellno, collapse = "\n"),
                "\n\n",
                "You may want to set `split_by = NULL`, remove the splits with less than 100 cells, ",
                "or choose a different `split_by`."
            ))
        }
        object <- SplitObject(object, split.by = split_by)
    }

    if (refnorm == "SCT" && defassay == "SCT" && skip_if_normalized) {
        log$info("Skipping query normalization, already normalized with SCT")
    } else if (refnorm == "LogNormalize" && defassay == "RNA" && skip_if_normalized) {
        log$info("Skipping query normalization, already normalized with LogNormalize")
    } else {
        if (refnorm == "SCT") {
            log$info("Normalizing query with SCT ...")
            if (!is.null(split_by)) {
                object <- mclapply(
                    X = object,
                    FUN = function(x) {
                        SCTransformArgs1 <- SCTransformArgs
                        SCTransformArgs1$object <- x
                        do_call(SCTransform, SCTransformArgs1)
                    },
                    mc.cores = ncores
                )
                if (any(unlist(lapply(object, class)) == "try-error")) {
                    stop(paste0("[RunSeuratMap2Ref] mclapply (SCTransform) error:", object))
                }
            } else {
                SCTransformArgs$object <- object
                object <- do_call(SCTransform, SCTransformArgs)
                SCTransformArgs$object <- NULL
                gc()
            }
        } else { # LogNormalize
            log$info("Normalizing query with LogNormalize ...")
            if (!is.null(split_by)) {
                object <- mclapply(
                    X = object,
                    FUN = function(x) {
                        NormalizeDataArgs1 <- NormalizeDataArgs
                        NormalizeDataArgs1$object <- x
                        do_call(NormalizeData, NormalizeDataArgs1)
                    },
                    mc.cores = ncores
                )
                if (any(unlist(lapply(object, class)) == "try-error")) {
                    stop(paste0("[RunSeuratMap2Ref] mclapply (NormalizeData) error:", object))
                }
            } else {
                NormalizeDataArgs$object <- object
                object <- do_call(NormalizeData, NormalizeDataArgs)
                NormalizeDataArgs$object <- NULL
                gc()
            }
        }
    }

    log$info("Running FindTransferAnchors ...")
    FindTransferAnchorsArgs$reference.reduction <- FindTransferAnchorsArgs$reference.reduction %||%
        MapQueryArgs$reference.reduction %||% Reductions(reference)[1]
    if (!is.null(FindTransferAnchorsArgs$dims)) {
        FindTransferAnchorsArgs$dims <- .expand_number(FindTransferAnchorsArgs$dims)
    }
    if (!is.null(split_by)) {
        anchors <- mclapply(
            X = object,
            FUN = function(x) {
                FindTransferAnchorsArgs1 <- FindTransferAnchorsArgs
                FindTransferAnchorsArgs1$query <- x
                FindTransferAnchorsArgs1$reference <- reference
                do_call(FindTransferAnchors, FindTransferAnchorsArgs1)
            },
            mc.cores = ncores
        )
        if (any(unlist(lapply(anchors, class)) == "try-error")) {
            stop(paste0("[RunSeuratMap2Ref] mclapply (FindTransferAnchors) error:", anchors))
        }
    } else {
        FindTransferAnchorsArgs$query <- object
        FindTransferAnchorsArgs$reference <- reference
        anchors <- do_call(FindTransferAnchors, FindTransferAnchorsArgs)
        FindTransferAnchorsArgs$query <- NULL
        FindTransferAnchorsArgs$reference <- NULL
        gc()
    }
    # a <<- anchors
    log$info("Running MapQuery ...")
    if (!is.null(split_by)) {
        object <- mclapply(
            X = seq_along(object),
            FUN = function(i) {
                MapQueryArgs1 <- MapQueryArgs
                MapQueryArgs1$query <- object[[i]]
                MapQueryArgs1$reference <- reference
                MapQueryArgs1$anchorset <- anchors[[i]]
                do_call(MapQuery, MapQueryArgs1)
            },
            mc.cores = ncores
        )
        if (any(unlist(lapply(object, class)) == "try-error")) {
            stop(paste0("[RunSeuratMap2Ref] mclapply (MapQuery) error:", object))
        }

        log$info("Joining split objects ...")
        gc()

        object <- merge(object[[1]], object[2:length(object)], merge.dr = MapQueryArgs$reference.reduction)
    } else {
        MapQueryArgs$query <- object
        MapQueryArgs$reference <- reference
        MapQueryArgs$anchorset <- anchors
        object <- do_call(MapQuery, MapQueryArgs)
        MapQueryArgs$query <- NULL
        MapQueryArgs$reference <- NULL
        MapQueryArgs$anchorset <- NULL
        gc()
    }

    log$info("Adding ident to metadata and set as identity ...")
    object <- AddMetaData(
        object,
        metadata = object@meta.data[[paste0("predicted.", use)]],
        col.name = ident
    )

    object@meta.data[[paste0("predicted.", use)]] <- NULL
    object@meta.data[[ident]] <- factor(object@meta.data[[ident]], levels = unique(object@meta.data[[ident]]))
    Idents(object) <- ident

    object <- AddMetaData(
        object,
        metadata = object@meta.data[[paste0("predicted.", use, ".score")]],
        col.name = paste0(ident, ".score")
    )
    object@meta.data[[paste0("predicted.", use, ".score")]] <- NULL

    if (is_sct(GetAssay(object)) && is.null(object@commands$PrepSCTFindMarkers)) {
        log$info("Running PrepSCTFindMarkers ...")
        object <- PrepSCTFindMarkers(object)
        object <- AddSeuratCommand(
            object,
            "PrepSCTFindMarkers",
            "PrepSCTFindMarkers(object)",
            assay.used = defassay
        )
    }

    cached$save(object)

    object
}

#' Convert a Seurat object (or RDS/H5Seurat/qs2 file) to an AnnData object file
#' @param object_or_file The Seurat object or the path to the RDS or H5Seurat file
#' @param outfile Output file
#' @param assay Assay to be used
#' @param subset Subset of cells to be kept in the AnnData object
#' @param log Logger
#' @return No return value
#' @importFrom rlang %||%
#' @importFrom Seurat DefaultAssay DefaultAssay<-
#' @importFrom SeuratObject RenameAssays
#' @importFrom digest digest
#' @importFrom methods as
#' @examples
#' \donttest{
#' ConvertSeuratToAnnData(SeuratObject::pbmc_small, "/tmp/pbmc_small.h5ad")
#' ConvertSeuratToAnnData(SeuratObject::pbmc_small, "/tmp/pbmc_small.g1.h5ad",
#'     subset = 'groups == "g1"'
#' )
#'
#' saveRDS(SeuratObject::pbmc_small, "/tmp/pbmc_small.rds")
#' ConvertSeuratToAnnData("/tmp/pbmc_small.rds", "/tmp/pbmc_small.h5ad")
#' }
#' @export
ConvertSeuratToAnnData <- function(object_or_file, outfile, assay = NULL, subset = NULL, log = NULL) {
    stopifnot(
        "[ConvertSeuratToAnnData] 'object_or_file' should be a Seurat object or a file path" =
            is.character(object_or_file) || inherits(object_or_file, "Seurat")
    )
    monkey_patch("SeuratDisk", "H5SeuratToH5AD", .H5SeuratToH5AD)

    dig <- digest(capture.output(utils::str(list(object_or_file, assay, subset))), algo = "sha256")
    dig <- substr(dig, 1, 8)
    outdir <- dirname(outfile)
    dir.create(outdir, showWarnings = FALSE)
    h5seurat_file <- file.path(
        outdir,
        paste0(tools::file_path_sans_ext(basename(outfile)), ".", dig, ".h5seurat")
    )
    if (file.exists(h5seurat_file) &&
        !inherits(object_or_file, "Seurat") &&
        (file.mtime(h5seurat_file) < file.mtime(object_or_file))) {
        file.remove(h5seurat_file)
    }

    log <- log %||% get_logger()
    if (inherits(object_or_file, "Seurat") || endsWith(object_or_file, ".rds") || endsWith(object_or_file, ".RDS") || endsWith(object_or_file, ".qs") || endsWith(object_or_file, ".qs2")) {
        if (!file.exists(h5seurat_file)) {
            if (is.character(object_or_file)) {
                log$debug("[ConvertSeuratToAnnData] Reading Seurat object from file ...")
                object_or_file <- read_obj(object_or_file)
            }
            if (!is.null(subset)) {
                log$debug("[ConvertSeuratToAnnData] Subsetting cells ...")
                object_or_file <- filter(object_or_file, !!!rlang::parse_expr(subset))
            }

            assay <- assay %||% DefaultAssay(object_or_file)
            # In order to convert to h5ad
            # https://github.com/satijalab/seurat/issues/8220#issuecomment-1871874649
            if (assay == "RNA") {
                object_or_file$RNAv3 <- as(object = object_or_file[[assay]], Class = "Assay")
                DefaultAssay(object_or_file) <- "RNAv3"
                object_or_file$RNA <- NULL
                object_or_file <- RenameAssays(object_or_file, RNAv3 = "RNA")
            }

            log$debug("[ConvertSeuratToAnnData] Saving Seurat object to H5Seurat file ...")
            object_or_file@commands <- list()
            SeuratDisk::SaveH5Seurat(object_or_file, h5seurat_file)

            rm(object_or_file)
            gc()
        } else if (is.null(assay)) {
            if (inherits(object_or_file, "character")) {
                log$debug("[ConvertSeuratToAnnData] Reading Seurat object from file to get default assay ...")
                object <- read_obj(object_or_file)
            }
            assay <- DefaultAssay(object)
            rm(object)
            gc()
        }
        object_or_file <- h5seurat_file
    } else if (!endsWith(object_or_file, ".h5seurat")) {
        stop(
            "[ConvertSeuratToAnnData] 'object_or_file' should be a Seurat object or ",
            "a file path with extension '.rds', '.RDS' or '.h5seurat'"
        )
    } else {
        log$debug("[ConvertSeuratToAnnData] Using existing H5Seurat file ...")
        if (!is.null(subset)) {
            if (!file.exists(h5seurat_file)) {
                log$debug("[ConvertSeuratToAnnData] Reading H5Seurat file for subsetting ...")
                object_or_file <- SeuratDisk::LoadH5Seurat(object_or_file)
                assay <- assay %||% DefaultAssay(object_or_file)
                object_or_file <- eval(parse(text = paste0("base::subset(object_or_file, subset = ", subset, ")")))

                log$debug("[ConvertSeuratToAnnData] Saving Seurat object to H5Seurat file ...")
                SeuratDisk::SaveH5Seurat(object_or_file, h5seurat_file)
            }
            object_or_file <- h5seurat_file
        }
    }
    if (is.null(assay)) {
        object <- SeuratDisk::LoadH5Seurat(object_or_file)
        assay <- DefaultAssay(object)
        rm(object)
        gc()
    }

    log$debug("[ConvertSeuratToAnnData] Converting to AnnData ...")
    SeuratDisk::Convert(object_or_file, dest = outfile, assay = assay, overwrite = TRUE)

    log$debug("[ConvertSeuratToAnnData] Fixing categorical data ...")
    # See: https://github.com/mojaveazure/seurat-disk/issues/183

    H5.create_reference <- function(self, ...) {
        space <- self$get_space()
        do_call("[", c(list(space), list(...)))
        ref_type <- hdf5r::h5const$H5R_OBJECT
        ref_obj <- hdf5r::H5R_OBJECT$new(1, self)
        res <- .Call(
            "R_H5Rcreate", ref_obj$ref, self$id, ".", ref_type,
            space$id, FALSE,
            PACKAGE = "hdf5r"
        )
        if (res$return_val < 0) {
            stop("Error creating object reference")
        }
        ref_obj$ref <- res$ref
        return(ref_obj)
    }

    h5ad <- hdf5r::H5File$new(outfile, "r+")
    cats <- names(h5ad[["obs/__categories"]])
    for (cat in cats) {
        catname <- paste0("obs/__categories/", cat)
        obsname <- paste0("obs/", cat)
        # Handle NA values in obs/cat
        # by creating levels with <NA> in obs/__categories/cat
        codes <- h5ad[[obsname]]$read()
        if (any(is.na(codes) | codes < 0)) {
            # add <NA> to levels (obs/__categories/cat)
            levels <- h5ad[[catname]]$read()
            levels <- c(levels, "<NA>")
            h5ad[[catname]]$write(args = list(1:length(levels)), value = levels)
            # update codes in obs/cat
            codes <- ifelse(is.na(codes) | codes < 0, length(levels) - 1, codes)
            h5ad[[obsname]]$write(args = list(1:length(codes)), value = codes)
        }
        ref <- H5.create_reference(h5ad[[catname]])
        h5ad[[obsname]]$create_attr(
            attr_name = "categories",
            robj = ref,
            space = hdf5r::H5S$new(type = "scalar")
        )
    }
    h5ad$close_all()
    rm(h5ad)
}


#' Convert an AnnData file (h5ad) to a Seurat object or an RDS/qs2 file
#' @param infile Input file
#' @param outfile Output file. If "NULL" (default) is given, a temporary file will be created
#' and saved and the Seurat object will be read from it.
#' @param assay Assay naem to save in the Seurat object
#' @param ident The name of the identity in metadata after conversion
#' @param log Logger
#' @importFrom rlang %||%
#' @export
#' @return The Seurat object if `outfile` is "<object>", otherwise NULL.
ConvertAnnDataToSeurat <- function(infile, outfile = NULL, assay = "RNA", ident = NULL, log = NULL) {
    stopifnot(
        "[ConvertAnnDataToSeurat] 'infile' should be a file path with extension '.h5ad'" =
            is.character(infile) && endsWith(infile, ".h5ad")
    )
    monkey_patch("SeuratDisk", "H5ADToH5Seurat", .H5ADToH5Seurat)
    monkey_patch("SeuratDisk", "AssembleAssay", .AssembleAssay)

    log <- log %||% get_logger()
    return_object <- is.null(outfile)

    if (return_object) {
        destfile <- tempfile(fileext = ".h5seurat")
    } else {
        if (
            !endsWith(outfile, ".h5seurat") &&
                !endsWith(outfile, ".rds") && !endsWith(outfile, ".RDS") &&
                !endsWith(outfile, ".qs") && !endsWith(outfile, ".qs2")
        ) {
            stop(
                "[ConvertAnnDataToSeurat] 'outfile' should be either NULL or ",
                "a file path with extension '.h5seurat', '.rds', '.RDS', '.qs' or '.qs2'"
            )
        }
        if (endsWith(outfile, ".h5seurat")) {
            destfile <- outfile
        } else {
            destfile <- tempfile(fileext = ".h5seurat")
        }
        outdir <- dirname(outfile)
        dir.create(outdir, showWarnings = FALSE)
    }

    log$debug("[ConvertAnnDataToSeurat] Converting h5ad file to h5seurat file ...")
    SeuratDisk::Convert(infile, destfile, assay = assay %||% "RNA", overwrite = TRUE)

    # Fixing categorical data
    # See: https://github.com/mojaveazure/seurat-disk/issues/109#issuecomment-1722394184
    f <- hdf5r::H5File$new(destfile, "r+")
    groups <- f$ls(recursive = TRUE)

    for (name in groups$name[grepl("categories", groups$name)]) {
        names <- strsplit(name, "/")[[1]]
        lvl_names <- c(names[1:length(names) - 1], "levels")
        lvl_names <- paste(lvl_names, collapse = "/")

        # check codes
        code_names <- c(names[1:length(names) - 1], "codes")
        code_names <- paste(code_names, collapse = "/")
        codes <- f[[code_names]]$read()
        # print(codes)
        if (any(codes < 0)) {
            f$create_dataset(
                name = lvl_names,
                robj = c("<NA>", as.character(f[[name]]$read())),
                dtype = hdf5r::H5T_STRING$new(size = Inf)
            )
        } else {
            f[[lvl_names]] <- f[[name]]
        }
    }

    for (name in groups$name[grepl("codes", groups$name)]) {
        names <- strsplit(name, "/")[[1]]
        names <- c(names[1:length(names) - 1], "values")
        new_name <- paste(names, collapse = "/")
        f[[new_name]] <- f[[name]]
        grp <- f[[new_name]]
        codes <- grp$read()
        if (any(codes < 0)) {
            codes <- ifelse(codes < 0, 1, codes + 2)
        } else {
            codes <- codes + 1
        }
        grp$write(args = list(1:grp$dims), value = codes)
    }

    # make as.list() works on misc and tools
    if (!is.null(f[["misc"]])) {
        lst <- h5group_to_list(f[["misc"]])
        list_to_h5group(f, "misc", lst)
    }
    if (!is.null(f[["tools"]])) {
        lst <- h5group_to_list(f[["tools"]])
        list_to_h5group(f, "tools", lst)
    }

    f$close_all()
    # Done fixing categorical data

    if (identical(destfile, outfile)) {
        # already converted to the desired output file
        return(NULL)
    }

    log$debug("[ConvertAnnDataToSeurat] Loading h5seurat file ...")
    object <- SeuratDisk::LoadH5Seurat(destfile, misc = FALSE, tools = FALSE)
    if (!is.null(ident)) {
        if (ident %in% colnames(object@meta.data)) {
            SeuratObject::Idents(object) <- ident
        } else {
            stop("[ConvertAnnDataToSeurat] 'ident' column '{ident}' not found in metadata/obs")
        }
    }

    if (return_object) {
        return(object)
    }

    log$debug("[ConvertAnnDataToSeurat] Saved Seurat object to {outfile}")
    save_obj(object, outfile)
}


#' Aggregate expression of single cells into psedobulk expression matrix
#'
#' @param object Seurat object
#' @param aggregate_by The metadata column to aggregate by. Default is "Sample".
#' You can add other columns to aggregate by, such as "Condition", "Batch", etc.
#' @param assay The assay to use for aggregation. Default is "RNA".
#' @param layer The layer to use for aggregation. Default is "counts".
#' @param subset A string to filter the cells before aggregation.
#' For example, `subset = "Condition == 'Control'"` will only aggregate cells
#' from the "Control" condition.
#' @param log Logger
#'
#' @return The expression matrix aggregated by the specified metadata columns.
#' With the metadata as the metadata at `meta` attribute.
#' @export
#' @importFrom SeuratObject GetAssayData
#' @importFrom dplyr %>% count distinct
#' @importFrom rlang %||% sym syms
#' @examples
#' \donttest{
#' obj <- SeuratObject::pbmc_small
#' obj$Sample <- rep(paste0("S", 1:10), each = ncol(obj) / 10)
#' obj$Condition <- rep(c("Control", "Treatment"), each = ncol(obj) / 2)
#' result <- AggregateExpressionPseudobulk(obj, aggregate_by = c("Sample", "Condition"))
#' head(result)
#' head(attr(result, "meta"))
#' }
AggregateExpressionPseudobulk <- function(
    object,
    aggregate_by = "Sample",
    assay = "RNA",
    layer = "counts",
    subset = NULL,
    log = NULL) {
    log <- log %||% get_logger()

    # Validate inputs
    stopifnot("'aggregate_by' columns not found in metadata" = all(aggregate_by %in% colnames(object@meta.data)))
    stopifnot("'assay' not found in object" = assay %in% names(object@assays))

    if (!is.null(subset)) {
        log$info("Subsetting cells based on: {subset}")
        object <- filter(object, !!rlang::parse_expr(subset))
    }

    log$info("Aggregating expression by: {paste(aggregate_by, collapse = ', ')}")

    # Get expression data
    expr_data <- GetAssayData(object, assay = assay, layer = layer)

    # Get metadata for aggregation
    metadata <- object@meta.data[, aggregate_by, drop = FALSE]
    metadata <- tidyr::unite(metadata, "Sample", !!!syms(aggregate_by), remove = FALSE)

    # Aggregate expression data
    log$info("Aggregating expression matrix ...")
    unique_groups <- unique(metadata$Sample)
    aggregated_matrix <- sapply(unique_groups, function(group) {
        group_cells <- rownames(metadata)[metadata$Sample == group]
        if (length(group_cells) == 1) {
            as.numeric(expr_data[, group_cells])
        } else {
            Matrix::rowSums(expr_data[, group_cells])
        }
    })

    # Set row and column names
    rownames(aggregated_matrix) <- rownames(expr_data)
    colnames(aggregated_matrix) <- unique_groups

    # Create metadata for aggregated samples
    log$info("Creating metadata for aggregated samples ...")
    meta_df <- metadata %>% distinct(!!!syms(aggregate_by))
    rownames(meta_df) <- NULL

    log$info("Aggregation complete. Matrix dimensions: {nrow(aggregated_matrix)} x {ncol(aggregated_matrix)}")

    attr(aggregated_matrix, "meta") <- meta_df
    return(aggregated_matrix)
}
