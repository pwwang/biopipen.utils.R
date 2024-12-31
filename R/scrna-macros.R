#' AddSeuratCommand
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
#' object <- AddSeuratCommand(object, "RunSeuratDEAnalysis",
#'  "RunSeuratDEAnalysis(object, group.by = 'groups', ident.1 = 'g1', ident.2 = 'g2')",
#'  list(group.by = 'groups', ident.1 = 'g1', ident.2 = 'g2'))
#' object@commands$RunSeuratDEAnalysis
AddSeuratCommand <- function(
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

#' RunSeuratDEAnalysis
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
#' @param cache Directory to cache the results. Set to `FALSE` to disable caching
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
#' RunSeuratDEAnalysis(SeuratObject::pbmc_small, "groups", "g1", "g2")
RunSeuratDEAnalysis <- function(
    object, group.by, ident.1 = NULL, ident.2 = NULL, assay = NULL, subset = NULL, cache = NULL, error = TRUE, ...) {
    cache <- cache %||% gettempdir()
    cached <- get_cached(list(object, group.by, ident.1, ident.2, subset, ...), "biopipen.utils.RunSeuratDEAnalysis", cache)
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
    empty[[group.by]] <- character()
    class(empty) <- c("SeuratDEAnalysis", class(empty))

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
        class(degs) <- c("SeuratDEAnalysis", class(degs))

        degs
    }

    if (!error) {
        degs <- tryCatch({
            find_markers(recorrect_umi, ...)
        }, error = function(e) {
            warning("[RunSeuratDEAnalysis] Failed to run DE analysis: ", e$message, immediate. = TRUE)
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
    save_to_cache(cached, "biopipen.utils.RunSeuratDEAnalysis", cache)

    degs
}

#' Perform cell QC
#' @param object Seurat object
#' @param cell_qc Cell QC criteria
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

    cols <- c("Sample", "nFeature_RNA", "nCount_RNA", "percent.mt", "percent.ribo", "percent.hb", "percent.plat", ".QC")
    if (!"Sample" %in% colnames(object@meta.data)) {
        object@meta.data$Sample <- object@project.name
    }
    if (is.null(cell_qc) || is.na(cell_qc) || length(cell_qc) == 0) {
        object$.QC <- TRUE
        object@misc$cell_qc_df <- object@meta.data[, cols, drop = FALSE]
    } else {
        object@meta.data <- mutate(object@meta.data, .QC = !!parse_expr(cell_qc))
        object@misc$cell_qc_df <- object@meta.data[, cols, drop = FALSE]
        object <- subset(object, subset = !!sym(".QC"))
    }

    return(object)
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
#' If the path is a file, it should be a h5 file that can be loaded by [Seurat::Read10X_h5]
#' @param samples Samples to load. If NULL, all samples will be loaded
#' @param per_sample_qc Whether to perform per-sample cell QC
#' @param cell_qc Cell QC criteria
#' @param gene_qc Gene QC criteria
#' A list containing the following fields:
#' * min_cells: Minimum number of cells a gene should be expressed in to be kept
#' * excludes: A string or strings to exclude certain genes. Regular expressions are supported.
#' Multiple strings can also be separated by commas in a single string.
#' @param tmpdir Temporary directory to store intermediate files when there are prefix in the file names
#' @param log Logger
#' @param cache Directory to cache the results. Set to `FALSE` to disable caching
#' @return A Seurat object.
#' In slot `misc`, there are two fields: `cell_qc_df` and `gene_qc`, a list of gene QC results
#' @importFrom bracer glob
#' @importFrom rlang sym
#' @importFrom dplyr filter group_by summarise
#' @importFrom Seurat CreateSeuratObject RenameCells Read10X Read10X_h5
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
    samples = NULL,
    per_sample_qc = FALSE,
    cell_qc = NULL,
    gene_qc = NULL,
    tmpdir = NULL,
    log = NULL,
    cache = NULL
) {
    log <- log %||% get_logger()
    meta <- as.data.frame(meta)
    samples <- samples %||% meta$Sample
    stopifnot("No samples found" = length(samples) > 0)

    cache <- cache %||% gettempdir()
    cached <- get_cached(list(meta, samples, per_sample_qc, cell_qc, gene_qc), "biopipen.utils.LoadSeuratAndPerformQC", cache)
    if (!is.null(cached$data)) {
        log$info("Initialized and QC'ed data loaded from cache")
        return(cached$data)
    }

    log$info("Loading each sample ...")
    tmpdir <- tmpdir %||% gettempdir()
    dig <- digest(capture.output(str(meta)), algo = "md5")
    dig <- substr(dig, 1, 8)
    tmpdir <- file.path(tmpdir, paste0("biopipen.utils.LoadSeuratSamples.", dig))

    object_list <- list()
    cell_qc_df <- NULL
    for (sam in samples) {
        if (per_sample_qc) {
            log$info("- Loading sample {sam} and performing per-sample QC ...")
        } else {
            log$info("- Loading sample {sam} ...")
        }

        mdata <- meta[meta$Sample == sam, , drop = TRUE]
        if (is.data.frame(mdata) && nrow(mdata) == 0) {
            log$warn("  No metadata found, skipping ...")
            next
        }

        path <- as.character(mdata$RNAData)
        if (is.na(path) || !is.character(path) || identical(path, "") || identical(path, "NA")) {
            log$warn("  No path found, skipping ...")
            next
        }

        if (dir.exists(path)) {
            exprs <- tryCatch(
                { Read10X(data.dir = path) },
                error = function(e) {
                    tmpdatadir <- file.path(tmpdir, slugify(sam))
                    if (dir.exists(tmpdatadir)) { unlink(tmpdatadir, recursive = TRUE) }
                    dir.create(tmpdatadir, recursive = TRUE, showWarnings = FALSE)
                    barcodefile = Sys.glob(file.path(path, "*barcodes.tsv.gz"))[1]
                    file.symlink(normalizePath(barcodefile), file.path(tmpdatadir, "barcodes.tsv.gz"))
                    genefile = glob(file.path(path, "*{genes,features}.tsv.gz"))[1]
                    file.symlink(normalizePath(genefile), file.path(tmpdatadir, "features.tsv.gz"))
                    matrixfile = Sys.glob(file.path(path, "*matrix.mtx.gz"))[1]
                    file.symlink(normalizePath(matrixfile), file.path(tmpdatadir, "matrix.mtx.gz"))
                    Read10X(data.dir = tmpdatadir)
                }
            )
        } else if (file.exists(path)) {
            exprs <- Read10X_h5(path)
        } else {
            stop("[LoadSeuratSample] {sample}: Path not found: {path}")
        }

        if ("Gene Expression" %in% names(exprs)) {
            exprs <- exprs[["Gene Expression"]]
        }

        obj <- CreateSeuratObject(exprs, project = sam)
        obj <- RenameCells(obj, add.cell.id = sam)
        # Attach meta data
        for (mname in names(mdata)) {
            if (mname %in% c("RNAData", "TCRData", "BCRData")) { next }
            mdt <- mdata[[mname]]
            if (is.factor(mdt)) { mdt <- levels(mdt)[mdt] }

            obj[[mname]] <- mdt
        }

        if (isTRUE(per_sample_qc)) {
            obj <- PerformSeuratCellQC(obj, cell_qc)
            cell_qc_df <- rbind(cell_qc_df, obj@misc$cell_qc_df)
        }

        object_list[[sam]] <- obj
    }

    log$info("Merging samples ...")
    obj = Reduce(merge, object_list)
    rm(object_list)
    gc()

    if (!per_sample_qc) {
        log$info("Performing cell QC ...")
        obj = PerformSeuratCellQC(obj, cell_qc = cell_qc)
    } else {
        obj@misc$cell_qc_df <- cell_qc_df
    }

    log$info("Performing gene QC ...")
    genes <- rownames(obj)
    obj@misc$gene_qc <- list(
        before = length(genes),
        criteria = gene_qc,
        ncells = data.frame(Sample = character(), feature = character(), ncells = numeric())
    )
    ncells_df <- data.frame(Sample = character(), feature = character(), ncells = numeric())
    if (!is.null(gene_qc)) {
        for (l in names(obj@assays$RNA@layers)) {
            if (startsWith(l, "counts.")) {
                lname <- substring(l, 8)
            } else {
                lname <- l
            }
            ncells_df <- rbind(
                ncells_df,
                data.frame(
                    Sample = rep(lname, length(genes)),
                    feature = genes,
                    ncells = Matrix::rowSums(obj@assays$RNA@layers[[l]] > 0)
                )
            )
        }
        if (!is.null(gene_qc$min_cells) && gene_qc$min_cells > 0) {
            obj@misc$gene_qc$ncells <- ncells_df[
                ncells_df$feature %in% (
                    ncells_df %>%
                    group_by(!!sym("feature")) %>%
                    summarise(.i = any(!!sym("ncells") < gene_qc$min_cells), .groups = "drop") %>%
                    filter(!!sym(".i")) %>%
                    pull("feature")
                ), , drop = FALSE
            ]
        }
        excludes <- gene_qc$excludes
        if (!is.null(excludes)) {
            if (length(excludes) == 1) {
                excludes <- trimws(unlist(strsplit(excludes, ",")))
            }
            exgenes <- unique(unlist(lapply(excludes, function(ex) { genes[grepl(ex, genes)] })))
            exgenes <- setdiff(exgenes, unique(obj@misc$gene_qc$ncells$feature))
            obj@misc$gene_gc$ncells <- rbind(
                obj@misc$gene_qc$ncells,
                ncells_df[ncells_df$feature %in% exgenes, , drop = FALSE]
            )
        }
        if (nrow(obj@misc$gene_qc$ncells) > 0) {
            obj <- subset(obj, features = setdiff(genes, unique(obj@misc$gene_qc$ncells$feature)))
        }
    }
    obj@misc$gene_qc$after <- nrow(obj)

    cached$data <- obj
    save_to_cache(cached, "biopipen.utils.LoadSeuratAndPerformQC", cache)

    AddSeuratCommand(
        obj, "LoadSeuratAndPerformQC",
        "LoadSeuratAndPerformQC(meta, samples, per_sample_qc, cell_qc, gene_qc, tmpdir, log, cache)"
    )
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
    cache = NULL
) {
    log <- log %||% get_logger()
    cache <- cache %||% gettempdir()
    cached <- get_cached(
        list(
            object, use_sct,
            SCTransformArgs,
            NormalizeDataArgs,
            FindVariableFeaturesArgs,
            ScaleDataArgs,
            RunPCAArgs
        ),
        "biopipen.utils.RunSeuratTransformations",
        cache
    )
    if (!is.null(cached$data)) {
        log$info("Transformed data loaded from cache")
        return(cached$data)
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
        # Default is to use the SCT assay
        # Cleanup memory
        SCTransformArgs$object <- NULL
        rm(SCTransformArgs)
        gc()
    } else {
        log_info("- Running NormalizeData ...")
        log$debug("  Arguments: {format_args(NormalizeDataArgs)}")
        NormalizeDataArgs$object <- object
        object <- do_call(NormalizeData, NormalizeDataArgs)
        # Cleanup memory
        NormalizeDataArgs$object <- NULL
        rm(NormalizeDataArgs)
        gc()

        log_info("- Running FindVariableFeatures ...")
        log$debug("  Arguments: {format_args(FindVariableFeaturesArgs)}")
        FindVariableFeaturesArgs$object <- object
        object <- do_call(FindVariableFeatures, FindVariableFeaturesArgs)
        # Cleanup memory
        FindVariableFeaturesArgs$object <- NULL
        rm(FindVariableFeaturesArgs)
        gc()

        log_info("- Running ScaleData ...")
        log_debug("  Arguments: {format_args(ScaleDataArgs)}")
        ScaleDataArgs$object <- object
        object <- do_call(ScaleData, ScaleDataArgs)
        # Cleanup memory
        ScaleDataArgs$object <- NULL
        rm(ScaleDataArgs)
        gc()
    }

    log_info("- Running RunPCA ...")
    RunPCAArgs$npcs <- if (is.null(RunPCAArgs$npcs)) { 50 } else { min(RunPCAArgs$npcs, ncol(object) - 1) }
    log$debug("  RunPCA: {format_args(RunPCAArgs)}")
    RunPCAArgs$object <- object
    object <- do_call(RunPCA, RunPCAArgs)
    # Cleanup memory
    RunPCAArgs$object <- NULL
    rm(RunPCAArgs)
    gc()

    cached$data <- object
    save_to_cache(cached, "biopipen.utils.RunSeuratTransformations", cache)

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
#' @importFrom SeuratObject JoinLayers
RunSeuratIntegration <- function(
    object, no_integration = FALSE, IntegrateLayersArgs = list(),
    log = NULL, cache = NULL
) {
    log <- log %||% get_logger()
    cache <- cache %||% gettempdir()
    cached <- get_cached(
        list(object, no_integration, IntegrateLayersArgs),
        "biopipen.utils.RunSeuratIntegration",
        cache
    )
    if (!is.null(cached$data)) {
        log$info("Integrated data loaded from cache")
        return(cached$data)
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
        method <- switch(
            method,
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
        if (DefaultAssay(object) == "SCT") {
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
        # Save it for dimension reduction plots
        object@misc$integrated_new_reduction <- IntegrateLayersArgs$new.reduction

        # Cleanup memory
        IntegrateLayersArgs$object <- NULL
        rm(IntegrateLayersArgs)
        gc()
    }

    log$info("- Joining layers ...")
    object <- JoinLayers(object, assay = "RNA")

    cached$data <- object
    save_to_cache(cached, "biopipen.utils.RunSeuratIntegration", cache)

    object
}

#' Run DoubletFinder on a Seurat object
#'
#' @param object Seurat object
#' @param ncores Number of cores to use
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
RunSeuratDoubletFinder <- function(object, ncores = 1, PCs = 30, pN = 0.25, doublets = 0.075, log = NULL, allow_warnings = FALSE) {
    log <- log %||% get_logger()

    log$info("- Preparing Seurat object ...")
    # More controls from args?
    if (!"seurat_clusters" %in% colnames(object@meta.data)) {
        object <- FindNeighbors(object, dims = 1:PCs)
        object <- FindClusters(object)
    } else {
        Idents(object) <- "seurat_clusters"
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
    colnames(doublets) <-  c("DoubletFinder_score","DoubletFinder_DropletType")
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
    cache = NULL
) {
    log <- log %||% get_logger()
    cache <- cache %||% gettempdir()
    cached <- get_cached(
        list(object, tool, DoubletFinderArgs, scDblFinderArgs, filter),
        "biopipen.utils.RunSeuratDoubletDetection",
        cache
    )
    if (!is.null(cached$data)) {
        log$info("Doublet detection results loaded from cache")
        return(cached$data)
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
        rm(DoubletFinderArgs)
        gc()
    } else {
        scDblFinderArgs$object <- object
        object <- do_call(RunSeuratScDblFinder, scDblFinderArgs)
        scDblFinderArgs$object <- NULL
        rm(scDblFinderArgs)
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

    cached$data <- object
    save_to_cache(cached, "biopipen.utils.RunSeuratDoubletDetection", cache)

    object
}
