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
    object$percent.mt <- PercentageFeatureSet(
        object,
        pattern = "^MT-|^Mt-|^mt-"
    )
    object$percent.ribo <- PercentageFeatureSet(
        object,
        pattern = "^RP[SL]|^Rp[sl]"
    )
    object$percent.hb <- PercentageFeatureSet(
        object,
        pattern = "^HB[^P]|^Hb[^p]"
    )
    object$percent.plat <- PercentageFeatureSet(
        object,
        pattern = "PECAM1|PF4|Pecam1|Pf4"
    )

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
        object@meta.data <- mutate(
            object@meta.data,
            .QC = !!parse_expr(cell_qc)
        )
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

#' Perform cell and gene QC on a Seurat object
#'
#' This is a convenience wrapper of [PerformSeuratCellQC()] and [PerformGeneQC()]
#' for a single sample, as done by [LoadSeuratAndPerformQC()] per sample before
#' the samples are merged.
#' @param object Seurat object of a single sample
#' @param cell_qc Cell QC criteria.
#' It is an expression string to pass to `dplyr::filter` function to filter the cells.
#' It can also be a list of expressions, where the names of the list are sample names.
#' You can have a default expression in the list with the name "DEFAULT" for the samples
#' that are not listed.
#' @param gene_qc Gene QC criteria
#' A list containing the following fields:
#' * min_cells: Minimum number of cells a gene should be expressed in to be kept
#' * excludes: A string or strings to exclude certain genes. Regular expressions are supported.
#' Multiple strings can also be separated by commas in a single string.
#' @return The Seurat object with the `.QC` column in `meta.data` and, if `gene_qc` is
#' given, the gene QC results in `@misc$gene_qc`
#' @export
#' @examples
#' \donttest{
#' obj <- PerformSeuratQC(
#'     SeuratObject::pbmc_small,
#'     cell_qc = "nFeature_RNA > 40",
#'     gene_qc = list(min_cells = 3)
#' )
#' table(obj$.QC)
#' head(obj@misc$gene_qc)
#' }
PerformSeuratQC <- function(object, cell_qc = NULL, gene_qc = NULL) {
    object <- PerformSeuratCellQC(object, cell_qc)
    if (!is.null(gene_qc) && length(gene_qc) > 0) {
        object@misc$gene_qc <- PerformGeneQC(object, gene_qc)
    }

    return(object)
}

#' Run contaminant RNA correction on a Seurat object
#'
#' @param object Seurat object
#' @param method Method to use for contaminant RNA correction. Must be one of "decontx"
#' (using decontX function from the celda package) and "sccdc" (using scCDC package),
#' case-insensitively.
#' @param decontXArgs Arguments to pass to decontX function from the celda package. See `?celda::decontX` for details.
#' @param scCDCArgs Arguments to pass to scCDC function from the scCDC package.
#' It is a list with 3 elements: Detection, Quantification and Correction, which are lists of arguments to pass to the corresponding functions
#' from the scCDC package: `scCDC::ContaminationDetection`, `scCDC::ContaminationQuantification` and `scCDC::ContaminationCorrection`.
#' @param keep_contam_assay Whether to keep the `Contaminated` assay (the original counts
#' before contamination correction) in the object. If `FALSE` (default), the assay is dropped
#' right after the correction to save memory.
#' @param log Logger
#' @return A Seurat object with contaminant RNA corrected counts in the "RNA" assay, the original
#' counts in the "Contaminated" assay (unless `keep_contam_assay` is `FALSE`), and the tool used
#' in `@misc$contamination$tool`. For scCDC, the detected GCGs and contamination ratios are
#' recorded in `@misc$contamination` as well.
#' @export
RunContamCorrection <- function(
    object,
    method,
    decontXArgs = list(),
    scCDCArgs = list(
        Detection = list(),
        Quantification = list(),
        Correction = list()
    ),
    keep_contam_assay = FALSE,
    log = NULL
) {
    log <- log %||% get_logger()
    method <- match.arg(tolower(method), c("decontx", "sccdc"))

    log$info(
        "Performing contaminant RNA correction with method '{method}' on '{object@project.name}' ..."
    )
    if (method == "decontx") {
        if (!requireNamespace("celda", quietly = TRUE)) {
            stop(
                "[RunContamCorrection] The 'celda' package is required for decontX contaminant RNA correction. Please install it first."
            )
        }
        # Run decontX
        decontXArgs$x <- SeuratObject::GetAssayData(
            object,
            assay = "RNA",
            layer = "counts"
        )
        decontx_res <- do_call(celda::decontX, decontXArgs)
        # Update the counts with the decontaminated counts
        object <- SeuratObject::RenameAssays(
            object,
            assay.name = "RNA",
            new.assay.name = "Contaminated"
        )
        object[["RNA"]] <- as(
            SeuratObject::CreateAssayObject(counts = decontx_res$decontXcounts),
            "Assay5"
        )
        SeuratObject::DefaultAssay(object) <- "RNA"
        # Store the contamination fraction in meta.data
        object$decontX_contamination <- decontx_res$contamination
    } else {
        if (!requireNamespace("scCDC", quietly = TRUE)) {
            stop(
                "[RunContamCorrection] The 'scCDC' package is required for scCDC contaminant RNA correction. Please install it first."
            )
        }
        # Quick clustering pass (needed if active.ident not already set)
        object <- Seurat::NormalizeData(
            object,
            normalization.method = "LogNormalize",
            scale.factor = 10000
        )
        nfeatures <- min(ceiling(nrow(object) / 2), 2000)
        object <- Seurat::FindVariableFeatures(
            object,
            selection.method = "vst",
            nfeatures = nfeatures
        )
        vfeats <- Seurat::VariableFeatures(object)
        object <- Seurat::ScaleData(object, features = vfeats)
        object <- Seurat::RunPCA(object, features = vfeats)
        max_dims <- min(30, ncol(object[["pca"]]))
        object <- Seurat::FindNeighbors(object, dims = 1:max_dims)
        object <- Seurat::FindClusters(object, resolution = 0.5)

        # Detection + correction
        scCDCArgs$Detection <- scCDCArgs$Detection %||% list()
        scCDCArgs$Detection$seuratobject <- object
        gcgs <- do_call(scCDC::ContaminationDetection, scCDCArgs$Detection)
        scCDCArgs$Detection$seuratobject <- NULL
        gc()

        scCDCArgs$Quantification <- scCDCArgs$Quantification %||% list()
        scCDCArgs$Quantification$object <- object
        scCDCArgs$Quantification$cont_genes <- rownames(gcgs)
        contamination_ratio <- do_call(
            scCDC::ContaminationQuantification,
            scCDCArgs$Quantification
        )
        scCDCArgs$Quantification$object <- NULL
        gc()

        object@misc$contamination <- list(
            tool = "scCDC",
            GCGs = gcgs,
            contamination_ratio = contamination_ratio
        )

        scCDCArgs$Correction <- scCDCArgs$Correction %||% list()
        scCDCArgs$Correction$object <- object
        scCDCArgs$Correction$cont_genes <- rownames(gcgs)
        object <- do_call(.scCDC.ContaminationCorrection, scCDCArgs$Correction)
        scCDCArgs$Correction$object <- NULL
        gc()
    }

    # scCDC records its GCGs and contamination ratios in @misc$contamination itself
    contamination <- object@misc$contamination %||% list()
    contamination$tool <- method
    object@misc$contamination <- contamination

    # The `Contaminated` assay (original counts) is only used by
    # contamination-expression visualizations, which are done after merging
    # and require `keep_contam_assay = TRUE`. Drop it here to avoid keeping
    # ~2x the count data through merging and the cell/gene filtering.
    if (!keep_contam_assay && "Contaminated" %in% names(object@assays)) {
        object@assays$Contaminated <- NULL
        invisible(gc())
    }

    return(object)
}

#' Load samples into a list of Seurat objects
#'
#' This loads each sample without performing any QC, so that QC can be done per sample by
#' [PerformSeuratQC()] before the samples are merged by [LoadSeuratAndPerformQC()].
#' @param meta Metadata of the samples
#' Required columns: Sample, RNAData.
#' The RNAData column should contain the path to the 10X or ParseBio data, either a directory or a file
#' If the path is a directory, the function will look for barcodes.tsv.gz, features.tsv.gz and matrix.mtx.gz.
#' The directory should be loaded by [Seurat::Read10X], [Seurat::ReadParseBio] or the HIVE data. Sometimes, there may be prefix in the file names,
#' e.g. "'prefix'.barcodes.tsv.gz", which is also supported.
#' If the path is a file ending with ".loom", it will be loaded by [SeuratDisk::Connect()] and converted to a Seurat object.
#' Otherwise, if the path is a file, it should be a h5 file that can be loaded by [Seurat::Read10X_h5()]
#'
#' This can also be a Seurat object to split into samples. It requires the "Sample" column in the meta.data slot
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
#' @param features A named character vector/list or a file path to rename features.
#' If a named vector/list is given, the names are the original feature names and the values are
#' the new names.
#' If a file path is given, it should be a TAB-delimited file with two columns (no header);
#' lines beginning with '#' are ignored. The first column contains the original feature names
#' and the second column the new names.
#' @param samples Samples to load. If NULL, all samples will be loaded
#' @param LoadLoomArgs Arguments to pass to [SeuratDisk::LoadLoom()] when loading loom files.
#' @param tmpdir Temporary directory to store intermediate files when there are prefix in the file names
#' @param log Logger
#' @return A named list of Seurat objects, one per sample. Samples that have no data or no
#' cells are skipped with a warning.
#' @importFrom dplyr filter
#' @importFrom rlang sym %||%
#' @importFrom Seurat CreateSeuratObject RenameCells
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
#'
#' objs <- LoadSeuratSamples(meta)
#' names(objs)
#' }
LoadSeuratSamples <- function(
    meta,
    min_cells = 0,
    min_features = 0,
    features = NULL,
    samples = NULL,
    LoadLoomArgs = list(),
    tmpdir = NULL,
    log = NULL
) {
    log <- log %||% get_logger()

    features_map <- .parse_features_map(features)

    is_seurat <- inherits(meta, "Seurat")
    if (!is_seurat) {
        meta <- as.data.frame(meta)
        samples <- samples %||% meta$Sample
    } else {
        meta <- UpdateSeuratObject(meta)
        samples <- samples %||% unique(meta@meta.data$Sample)
        assay <- DefaultAssay(meta)
        if (identical(assay, "integrated")) {
            log$warn(
                "The default assay is 'integrated', which is not suitable for QC. Please set the default assay to the original assay (e.g. 'RNA') before calling this function."
            )
        }
        if (!"Assay5" %in% class(meta[[assay]])) {
            log$debug("Converting assay '{assay}' to Assay5 ...")
            meta[[assay]] <- as(meta[[assay]], "Assay5")
        }
    }
    stopifnot("No samples found" = length(samples) > 0)

    tmpdir <- tmpdir %||% gettempdir()
    dig <- digest::digest(.sig_str(meta), algo = "md5")
    dig <- substr(dig, 1, 8)
    tmpdir <- file.path(
        tmpdir,
        paste0("biopipen.utils.LoadSeuratSamples.", dig)
    )

    object_list <- list()
    for (sam in samples) {
        log$info("- Loading {sam} ...")
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
            if (
                is.na(path) ||
                    !is.character(path) ||
                    identical(path, "") ||
                    identical(path, "NA")
            ) {
                log$warn("  No path found, skipping ...")
                next
            }

            loaded <- .load_expression_data(
                path,
                sam,
                tmpdir,
                LoadLoomArgs,
                log
            )
            exprs <- loaded$exprs
            cell_meta <- loaded$cell_meta

            if (
                "Gene Expression" %in%
                    names(exprs) &&
                    !inherits(exprs, "Seurat")
            ) {
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
                if (!is.null(features_map)) {
                    obj <- .rename_seurat_features(obj, features_map)
                }
            } else {
                if (!is.null(features_map)) {
                    cur_names <- rownames(exprs)
                    mask <- cur_names %in% names(features_map)
                    if (any(mask)) {
                        cur_names[mask] <- features_map[cur_names[mask]]
                        rownames(exprs) <- cur_names
                    }
                }
                obj <- CreateSeuratObject(
                    exprs,
                    project = sam,
                    min.cells = minc,
                    min.features = minf,
                    meta.data = cell_meta
                )
            }
            obj <- RenameCells(obj, add.cell.id = sam)

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

        object_list[[sam]] <- obj
    }

    return(object_list)
}

#' Load samples into a Seurat object
#'
#' Cell QC will be performed, either per-sample or on the whole object
#' @param meta Metadata of the samples
#' Required columns: Sample, RNAData.
#' The RNAData column should contain the path to the 10X or ParseBio data, either a directory or a file
#' If the path is a directory, the function will look for barcodes.tsv.gz, features.tsv.gz and matrix.mtx.gz.
#' The directory should be loaded by [Seurat::Read10X], [Seurat::ReadParseBio] or the HIVE data. Sometimes, there may be prefix in the file names,
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
#' that are not listed.
#' This won't work if data is loaded from a loom file or `meta` is a Seurat object.
#' @param samples Samples to load. If NULL, all samples will be loaded
#' @param cell_qc Cell QC criteria, a string of expression to pass to `dplyr::filter` function
#' to filter the cells.
#' It can also be a list of expressions, where the names of the list are sample names.
#' You can have a default expression in the list with the name "DEFAULT" for the samples
#' that are not listed.
#' @param features A named character vector/list or a file path to rename features.
#' If a named vector/list is given, the names are the original feature names and the values are
#' the new names.
#' If a file path is given, it should be a TAB-delimited file with two columns (no header);
#' lines beginning with '#' are ignored. The first column contains the original feature names
#' and the second column the new names.
#' @param gene_qc Gene QC criteria
#' A list containing the following fields:
#' * min_cells: Minimum number of cells a gene should be expressed in to be kept
#' * excludes: A string or strings to exclude certain genes. Regular expressions are supported.
#' Multiple strings can also be separated by commas in a single string.
#' @param ccs_args Arguments to pass to [RunSeuratCellCycleScoring()] to perform cell cycle scoring after the data
#' has been QC'ed.
#' @param LoadLoomArgs Arguments to pass to [SeuratDisk::LoadLoom()] when loading loom files.
#' @param contam_correction Whether to perform contaminant RNA correction. Supported methods: "decontx" (using decontX function from the celda package) and "sccdc" (using scCDC package).
#' @param decontXArgs Arguments to pass to decontX function from the celda package when `contam_correction` is "decontx". See `?celda::decontX` for details.
#' @param scCDCArgs Arguments to pass to scCDC package when `contam_correction` is "sccdc". It should be a list containing three sub-lists: Detection, Quantification and Correction, which are the arguments for `scCDC::ContaminationDetection`, `scCDC::ContaminationQuantification` and `scCDC::ContaminationCorrection` functions, respectively. See `?scCDC::ContaminationDetection`, `?scCDC::ContaminationQuantification` and `?scCDC::ContaminationCorrection` for details.
#' @param keep_contam_assay Whether to keep the `Contaminated` assay (the original counts before
#' contamination correction) in the object. If `FALSE` (default), the assay is dropped per-sample
#' right after the correction (before samples are merged) to save memory. Note the `Contaminated`
#' assay is not used by any downstream steps of [FinishSeuratQC()] unless `keep_contam_assay` is `TRUE`.
#' @param tmpdir Temporary directory to store intermediate files when there are prefix in the file names
#' @param log Logger
#' @param cache Directory to cache the results. Set to `FALSE` to disable caching
#' @return A Seurat object.
#' In slot `misc`, there are two fields: `cell_qc_df` and `gene_qc`, a list of gene QC results
#' @importFrom bracer glob
#' @importFrom rlang sym
#' @importFrom dplyr filter group_by summarise
#' @importFrom Seurat CreateSeuratObject RenameCells Read10X Read10X_h5 SplitObject ReadParseBio
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
#'
#' obj <- LoadSeuratAndPerformQC(
#'    meta, cache = FALSE, cell_qc = "nFeature_RNA > 1000",
#'    gene_qc = list(min_cells = 3)
#' )
#' print(table(obj$.QC))
#' print(obj@misc$gene_qc)
#' }
LoadSeuratAndPerformQC <- function(
    meta,
    min_cells = 0,
    min_features = 0,
    features = NULL,
    samples = NULL,
    cell_qc = NULL,
    gene_qc = NULL,
    ccs_args = NULL,
    LoadLoomArgs = list(),
    contam_correction = NULL,
    decontXArgs = list(),
    scCDCArgs = list(
        Detection = list(),
        Quantification = list(),
        Correction = list()
    ),
    keep_contam_assay = FALSE,
    tmpdir = NULL,
    log = NULL,
    cache = NULL
) {
    log <- log %||% get_logger()

    features_map <- .parse_features_map(features)

    cache <- cache %||% gettempdir()
    cached <- Cache$new(
        list(
            meta,
            min_cells,
            min_features,
            features_map,
            samples,
            cell_qc,
            gene_qc,
            LoadLoomArgs,
            ccs_args,
            contam_correction,
            decontXArgs,
            scCDCArgs,
            keep_contam_assay
        ),
        prefix = "biopipen.utils.LoadSeuratAndPerformQC",
        cache_dir = cache
    )

    if (cached$is_cached()) {
        log$info("Initialized and QC'ed data loaded from cache: {cached$get_path()}")
        return(cached$restore())
    }

    # Captured before loading, so that the levels survive the merging of the samples
    factor_levels <- lapply(
        if (inherits(meta, "Seurat")) meta@meta.data else as.data.frame(meta),
        function(x) if (is.factor(x)) levels(x) else NULL
    )

    log$info("Loading each sample ...")
    object_list <- LoadSeuratSamples(
        meta,
        min_cells = min_cells,
        min_features = min_features,
        features = features,
        samples = samples,
        LoadLoomArgs = LoadLoomArgs,
        tmpdir = tmpdir,
        log = log
    )

    geneqc_df <- NULL
    contamination_info <- list()
    for (sam in names(object_list)) {
        obj <- PerformSeuratQC(
            object_list[[sam]],
            cell_qc = cell_qc,
            gene_qc = gene_qc
        )
        if (!is.null(gene_qc) && length(gene_qc) > 0) {
            geneqc_df <- rbind(geneqc_df, obj@misc$gene_qc)
        }

        if (!is.null(contam_correction) && !isFALSE(contam_correction)) {
            if (isTRUE(contam_correction)) {
                contam_correction <- "decontx"
            }
            obj <- RunContamCorrection(
                obj,
                method = contam_correction,
                decontXArgs = decontXArgs,
                scCDCArgs = scCDCArgs,
                keep_contam_assay = keep_contam_assay,
                log = log
            )
            contam_info <- obj@misc$contamination
            contamination_info[["tool"]] <- contam_info$tool
            if (identical(contam_info$tool, "sccdc")) {
                contamination_info[["GCGs"]][[sam]] <- contam_info$GCGs
                contamination_info[["contamination_ratio"]][[sam]] <-
                    contam_info$contamination_ratio
            }
        }

        if (length(ccs_args) > 0) {
            ccs_args$object <- obj
            ccs_args$log <- log
            ccs_args$cache <- cache
            obj <- do_call(RunSeuratCellCycleScoring, ccs_args)
            ccs_args$object <- NULL
            gc()
        }

        object_list[[sam]] <- obj
    }

    log$info("Merging samples ...")
    obj <- object_list[[1]]
    for (sam in names(object_list)[-1]) {
        obj <- merge(obj, object_list[[sam]])
        # release the sample object as soon as it is merged to reduce the peak memory
        object_list[[sam]] <- NULL
        gc()
    }
    rm(object_list)
    gc()

    for (col in names(factor_levels)) {
        if (!is.null(factor_levels[[col]])) {
            obj@meta.data[[col]] <- factor(obj@meta.data[[col]], levels = factor_levels[[col]])
        }
    }

    obj@misc$gene_qc <- geneqc_df
    obj@misc$contamination <- contamination_info
    if (!is.null(contamination_info$contamination_ratio)) {
        obj@misc$contamination$contamination_ratio <- data.frame(
            Sample = names(contamination_info$contamination_ratio),
            ContaminationRatio = unlist(contamination_info$contamination_ratio)
        )
    }
    obj <- AddSeuratCommand(
        obj,
        "LoadSeuratAndPerformQC",
        "LoadSeuratAndPerformQC(meta, samples, per_sample_qc, cell_qc, gene_qc, tmpdir, log, cache)"
    )
    # gc before serialization: qs2 allocations don't trigger R's gc,
    # so collect first to lower the peak memory of the cache save
    invisible(gc())
    cached$save(obj)
    obj
}
#' Finish the QC process including the visualization
#'
#' This will remove the cells and genes that are not passing the QC, and also remove
#' the intermediate data used for QC visualization.
#'
#' @param object Seurat object
#' @param keep_contam_assay A logic value to control where to keep the "Contaminated" assay
#' @return The Seurat object with the QC process finished
#' @export
FinishSeuratQC <- function(object, keep_contam_assay = FALSE) {
    if (!".QC" %in% colnames(object@meta.data)) {
        stop(
            "[FinishSeuratQC] No cell QC data found, the object must be loaded with `LoadSeuratAndPerformQC`"
        )
    }

    features <- NULL
    if (!is.null(object@misc$gene_qc)) {
        failed_features <- unique(object@misc$gene_qc[
            !object@misc$gene_qc$QC,
            "Feature"
        ])
        features <- setdiff(
            unique(object@misc$gene_qc$Feature),
            failed_features
        )
    }

    object <- subset(object, subset = !!sym(".QC"), features = features)
    object@meta.data$.QC <- NULL
    object@misc$gene_qc <- NULL
    if (!is.null(object@misc$contamination)) {
        object@misc$contamination <- NULL
    }
    if ("Contaminated" %in% names(object@assays) && !keep_contam_assay) {
        object@assays$Contaminated <- NULL
    }
    gc()

    # Add the command to the object, using the time.stamp of LoadSeuratAndPerformQC
    # to keep the same time.stamp, so that later processes can be cached
    # and reused
    if (
        !is.null(object@commands) &&
            !is.null(object@commands$LoadSeuratAndPerformQC)
    ) {
        time.stamp <- object@commands$LoadSeuratAndPerformQC@time.stamp
    } else {
        time.stamp <- Sys.time()
    }
    AddSeuratCommand(
        object,
        "FinishSeuratQC",
        "FinishSeuratQC(object)",
        time.stamp = time.stamp
    )
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
#' @param reuse.pANN Metadata column name for previously-generated pANN results.
#' If NULL (default), an existing pANN column matching the current pN/pK is
#' reused to skip pANN computation, if available. Set to FALSE or an invalid
#' column name is treated as NULL with a warning.
#' @keywords internal
#' @return The Seurat object with doublet detection results in `@misc$doubletFinder`
#' @importFrom Seurat FindNeighbors FindClusters DefaultAssay AddMetaData
#' @importFrom rlang %||%
#' @importFrom SeuratObject Idents<- Idents
#' @importFrom grDevices pdf dev.off
RunSeuratDoubletFinder <- function(
    object,
    ident = NULL,
    ncores = 1,
    PCs = 30,
    pN = 0.25,
    doublets = 0.075,
    reuse.pANN = NULL,
    log = NULL,
    allow_warnings = FALSE
) {
    log <- log %||% get_logger()

    log$info("- Preparing Seurat object ...")
    # More controls from args?
    if (is.null(ident) || !ident %in% colnames(object@meta.data)) {
        if (!is.null(ident)) {
            log$warn(
                "  ident '{ident}' not found in metadata, running clustering to get identities"
            )
        } else {
            log$info(
                "  No ident provided, running clustering to get identities"
            )
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

    # Release the pK sweep artifacts, they are no longer needed (bcmvn is kept below)
    rm(sweep.res.list, sweep.stats)
    invisible(gc())

    log$info("- Running DoubletFinder ...")
    if (allow_warnings) {
        object <- DoubletFinder::doubletFinder(
            object,
            PCs = 1:PCs,
            pN = pN,
            pK = pK,
            nExp = nExp_poi.adj,
            reuse.pANN = reuse.pANN,
            sct = identical(DefaultAssay(object), "SCT")
        )
    } else {
        object <- suppressWarnings(DoubletFinder::doubletFinder(
            object,
            PCs = 1:PCs,
            pN = pN,
            pK = pK,
            nExp = nExp_poi.adj,
            reuse.pANN = reuse.pANN,
            sct = identical(DefaultAssay(object), "SCT")
        ))
    }
    pANN_col <- paste0("pANN_", pN, "_", pK)
    pANN_col <- colnames(object@meta.data)[grepl(
        pANN_col,
        colnames(object@meta.data)
    )]
    DF_col <- paste0("DF.classifications_", pN, "_", pK)
    DF_col <- colnames(object@meta.data)[grepl(
        DF_col,
        colnames(object@meta.data)
    )]
    doublets <- object@meta.data[, c(pANN_col, DF_col), drop = FALSE]
    colnames(doublets) <- c("DoubletFinder_score", "DoubletFinder_DropletType")
    doublets$DoubletFinder_DropletType <- tolower(
        doublets$DoubletFinder_DropletType
    )
    doublets$DoubletFinder_DropletType <- factor(
        doublets$DoubletFinder_DropletType,
        levels = c("singlet", "doublet")
    )

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
    doublets$scDblFinder_DropletType <- factor(
        doublets$scDblFinder_DropletType,
        levels = c("singlet", "doublet")
    )

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
    cached <- Cache$new(
        list(object, tool, DoubletFinderArgs, scDblFinderArgs, filter),
        prefix = "biopipen.utils.RunSeuratDoubletDetection",
        cache_dir = cache
    )
    if (cached$is_cached()) {
        log$info("Doublet detection results loaded from cache: {cached$get_path()}")
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
            object <- subset(
                object,
                subset = !!sym("DoubletFinder_DropletType") != "doublet"
            )
        } else {
            object <- subset(
                object,
                subset = !!sym("scDblFinder_DropletType") != "doublet"
            )
        }
    }

    object <- AddSeuratCommand(
        object,
        "RunSeuratDoubletDetection",
        "RunSeuratDoubletDetection(object, tool, DoubletFinderArgs, scDblFinderArgs, filter)",
        params = list(
            tool = tool,
            DoubletFinderArgs = DoubletFinderArgs,
            scDblFinderArgs = scDblFinderArgs,
            filter = filter
        )
    )
    invisible(gc())
    # gc before serialization: qs2 allocations don't trigger R's gc
    cached$save(object)
    object
}
