#' Run Seurat MapQuery to reference
#'
#' This will find transfer anchors between query and reference, and map the query to the reference.
#' For SCT-normalized references, [Seurat::FindTransferAnchors()] handles query normalization internally
#' by recomputing residuals using the reference's SCT model (`recompute.residuals = TRUE` by default),
#' so a manual call to [Seurat::SCTransform()] on the query is NOT needed.
#' For LogNormalize references, the query is normalized with [Seurat::NormalizeData()] prior to anchor
#' finding.
#' @seealso https://satijalab.org/seurat/articles/integration_mapping.html
#' @seealso https://satijalab.org/seurat/articles/multimodal_reference_mapping.html
#' @seealso https://satijalab.org/seurat/articles/covid_sctmapping
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
#' @param skip_if_normalized For LogNormalize, skip [Seurat::NormalizeData()] if the query's default
#' assay is already 'RNA' (i.e., already log-normalized).
#' For SCT with `FindTransferAnchorsArgs$recompute.residuals = FALSE`, skip [Seurat::SCTransform()] if
#' the default assay is already 'SCT'.
#' This parameter has no effect in the default SCT case (`recompute.residuals = TRUE`), where
#' [Seurat::FindTransferAnchors()] handles normalization internally using the reference model.
#' @param split_by The name of the metadata to split the query object by and do the mapping separately
#' @param ncores Number of cores to use for parallel processing for the split query objects
#' @param MapQueryArgs Arguments to pass to [Seurat::MapQuery()].
#' The `use` argument will be added to the `refdata` list.
#' @param FindTransferAnchorsArgs Arguments to pass to [Seurat::FindTransferAnchors()].
#' @param SCTransformArgs Arguments to pass to [Seurat::SCTransform()].
#' Only used when `refnorm` is set to "SCT" AND `FindTransferAnchorsArgs$recompute.residuals` is
#' explicitly set to `FALSE`. In the default case (`recompute.residuals = TRUE`),
#' [Seurat::FindTransferAnchors()] recomputes query residuals from raw counts using the reference
#' SCT model, making manual [Seurat::SCTransform()] on the query unnecessary.
#' @param NormalizeDataArgs Arguments to pass to [Seurat::NormalizeData()].
#' Will be used to normalize the query object if `refnorm` is set to "LogNormalize"
#' @param log Logger
#' @param cache Directory to cache the results. Set to `FALSE` to disable caching
#' @return The Seurat object with the mapped data
#' @export
#' @importFrom methods slot
#' @importFrom rlang %||%
#' @importFrom SeuratObject UpdateSeuratObject DefaultAssay Idents Misc JoinLayers
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
    cache = NULL
) {
    refnorm <- match.arg(refnorm)

    log <- log %||% get_logger()
    cache <- cache %||% gettempdir()
    cached <- Cache$new(
        list(
            object,
            ref,
            use,
            ident,
            refnorm,
            skip_if_normalized,
            split_by,
            ncores,
            MapQueryArgs,
            FindTransferAnchorsArgs,
            SCTransformArgs,
            NormalizeDataArgs
        ),
        prefix = "biopipen.utils.RunSeuratMap2Ref",
        cache_dir = cache
    )
    if (cached$is_cached()) {
        log$info("Mapping-to-reference results loaded from cache: {cached$get_path()}")
        return(cached$restore())
    }

    if (
        is.character(ref) &&
            (endsWith(ref, ".rds") ||
                endsWith(ref, ".Rds") ||
                endsWith(ref, ".RDS") ||
                endsWith(ref, "qs") ||
                endsWith(ref, ".qs2"))
    ) {
        log$info("Loading reference ...")
        reference <- read_obj(ref)
    } else if (
        is.character(ref) &&
            (endsWith(ref, ".h5seurat") || endsWith(ref, ".H5Seurat"))
    ) {
        log$info("Loading reference ...")
        reference <- SeuratDisk::LoadH5Seurat(ref)
    } else if (inherits(ref, "Seurat")) {
        reference <- ref
    } else {
        stop(
            "[RunSeuratMap2Ref] 'reference' should be a Seurat object or a file path with extension '.qs', 'qs2', '.rds', '.RDS' or '.h5seurat'"
        )
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
        if (
            !(MapQueryArgs$refdata[[name]] %in% colnames(reference@meta.data))
        ) {
            stop(paste0(
                "[RunSeuratMap2Ref] items of 'refdata' should be in the reference: ",
                MapQueryArgs$refdata[[name]],
                "\n",
                "Available items: ",
                paste0(colnames(reference@meta.data), collapse = ", ")
            ))
        }
        if (startsWith(name, "predicted.")) {
            stop(paste0(
                "[RunSeuratMap2Ref] items 'refdata' should not start with 'predicted.': ",
                name,
                " please use 'x' instead of 'predicted.x'"
            ))
        }
    }
    if (is.null(MapQueryArgs$reference.reduction)) {
        MapQueryArgs$reference.reduction <- Reductions(reference)[1]
        log$warn(
            "- `MapQueryArgs$reference.reduction` is not set, using the first reduction in the reference: {MapQueryArgs$reference.reduction}"
        )
    }
    if (!MapQueryArgs$reference.reduction %in% Reductions(reference)) {
        stop(paste0(
            "[RunSeuratMap2Ref] `MapQueryArgs$reference.reduction` is not in the reference: ",
            MapQueryArgs$reference.reduction,
            "\n",
            "Available reductions: ",
            paste0(Reductions(reference), collapse = ", ")
        ))
    }
    MapQueryArgs$reduction.model <- MapQueryArgs$reduction.model %||% "umap"
    # Check if reference has the same reduction model
    if (
        is.null(reference@reductions[[MapQueryArgs$reduction.model]]) ||
            length(
                x = Misc(
                    object = reference@reductions[[
                        MapQueryArgs$reduction.model
                    ]],
                    slot = "model"
                )
            ) ==
                0
    ) {
        models <- sapply(names(reference@reductions), function(x) {
            ifelse(
                length(
                    x = Misc(object = reference@reductions[[x]], slot = "model")
                ) >
                    0,
                x,
                NA
            )
        })
        models <- models[!is.na(models)]
        msg <- paste0(
            "[RunSeuratMap2Ref] `MapQueryArgs$reduction.model` is not in the reference: ",
            MapQueryArgs$reduction.model,
            "\n",
            "Try to run the reference with ",
            "RunUMAP(reference, reduction = '",
            MapQueryArgs$reference.reduction,
            "', reduction.name = '",
            MapQueryArgs$reduction.model,
            "', dims = 1:N, return.model = TRUE)\n",
            "to create the model.\n"
        )
        if (length(models) == 0) {
            stop(msg)
        } else {
            msg <- paste0(
                msg,
                "Or choose a different `MapQueryArgs$reduction.model`. Available models: ",
                paste0(models, collapse = ", "),
                "\n"
            )
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
            "[RunSeuratMap2Ref] reference is not SCTransform'ed, but refnorm is set to SCT" = ref_is_sct
        )

        n_models <- length(
            x = slot(
                object = reference[[DefaultAssay(reference)]],
                name = "SCTModel.list"
            )
        )
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

    # When normalization.method = "SCT" with recompute.residuals = TRUE (FindTransferAnchors default),
    # the function internally computes query residuals using the reference's SCT model from raw counts.
    # Manual SCTransform on the query is therefore NOT needed and would be redundant, since those
    # residuals are overridden internally anyway. Manual SCTransform is only needed when
    # recompute.residuals = FALSE is explicitly set by the user.
    sct_needs_precompute <- isFALSE(
        FindTransferAnchorsArgs$recompute.residuals %||% TRUE
    )

    if (refnorm == "SCT" && !sct_needs_precompute) {
        log$info(paste0(
            "Skipping manual SCT normalization of query: ",
            "FindTransferAnchors will compute query residuals using the reference ",
            "SCT model internally (recompute.residuals = TRUE)."
        ))
    } else if (refnorm == "SCT" && defassay == "SCT" && skip_if_normalized) {
        log$info("Skipping query normalization, already normalized with SCT")
    } else if (
        refnorm == "LogNormalize" && defassay == "RNA" && skip_if_normalized
    ) {
        log$info(
            "Skipping query normalization, already normalized with LogNormalize"
        )
    } else {
        if (refnorm == "SCT") {
            # Only reached when recompute.residuals = FALSE is explicitly set
            log$info(
                "Normalizing query with SCT (recompute.residuals = FALSE) ..."
            )
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
                    stop(paste0(
                        "[RunSeuratMap2Ref] mclapply (SCTransform) error:",
                        object
                    ))
                }
            } else {
                SCTransformArgs$object <- object
                object <- do_call(SCTransform, SCTransformArgs)
                SCTransformArgs$object <- NULL
                gc()
            }
        } else {
            # LogNormalize
            log$info("Normalizing query with LogNormalize ...")
            # If the query's default assay is SCT, explicitly target the RNA assay so that
            # NormalizeData operates on RNA rather than the SCT assay (which would be incorrect
            # and leave the RNA assay un-normalized, causing CCA to crash on bad data).
            if (is.null(NormalizeDataArgs$assay) && defassay != "RNA") {
                NormalizeDataArgs$assay <- "RNA"
            }
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
                    stop(paste0(
                        "[RunSeuratMap2Ref] mclapply (NormalizeData) error:",
                        object
                    ))
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
    # If the reference is LogNormalize but the query default assay is SCT, explicitly
    # direct FindTransferAnchors to use the RNA assay to avoid the error:
    # "An SCT assay was provided for query.assay but normalization.method was set as LogNormalize"
    if (
        refnorm == "LogNormalize" &&
            is.null(FindTransferAnchorsArgs$query.assay)
    ) {
        query_defassay <- if (is.list(object)) {
            DefaultAssay(object[[1]])
        } else {
            DefaultAssay(object)
        }
        if (query_defassay != "RNA") {
            log$warn(paste0(
                "Query default assay is '",
                query_defassay,
                "' but refnorm is 'LogNormalize'. ",
                "Setting query.assay = 'RNA' for FindTransferAnchors."
            ))
            FindTransferAnchorsArgs$query.assay <- "RNA"
        }
    }
    FindTransferAnchorsArgs$reduction <- FindTransferAnchorsArgs$reduction %||%
        "pcaproject"
    if (!identical(FindTransferAnchorsArgs$reduction, "cca")) {
        FindTransferAnchorsArgs$reference.reduction <- FindTransferAnchorsArgs$reference.reduction %||%
            MapQueryArgs$reference.reduction %||%
            Reductions(reference)[1]
    }
    if (!is.null(FindTransferAnchorsArgs$dims)) {
        FindTransferAnchorsArgs$dims <- .expand_number(
            FindTransferAnchorsArgs$dims
        )
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
            stop(paste0(
                "[RunSeuratMap2Ref] mclapply (FindTransferAnchors) error:",
                anchors
            ))
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
            stop(paste0(
                "[RunSeuratMap2Ref] mclapply (MapQuery) error:",
                object
            ))
        }

        log$info("Joining split objects ...")
        gc()

        object <- merge(
            object[[1]],
            object[2:length(object)],
            merge.dr = MapQueryArgs$reference.reduction
        )
        object <- JoinLayers(object)
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
    object@meta.data[[ident]] <- factor(
        object@meta.data[[ident]],
        levels = unique(object@meta.data[[ident]])
    )
    Idents(object) <- ident
    # TODO: Set default dim reduction to `ref.<MapQueryArgs$reduction.model>` if exists.
    # Wait for SeuratObject v5.4.0
    # See: https://github.com/satijalab/seurat-object/pull/268
    ProjectUMAPArgs <- MapQueryArgs$projectumap.args %||% list()
    new_reduc <- ProjectUMAPArgs$reduction.name %||% "ref.umap"
    if (!new_reduc %in% Reductions(object)) {
        log$warn(
            "Can't find reduction '{new_reduc}' in the mapped object. Check the logs to make sure everything is correct."
        )
        log$warn("Give up setting default reduction to '{new_reduc}'.")
    } else {
        log$info("Setting default reduction to '{new_reduc}' ...")
        `default_dimreduc<-` <- utils::getFromNamespace(
            "default_dimreduc<-",
            "scplotter"
        )
        default_dimreduc(object) <- new_reduc
    }

    object <- AddMetaData(
        object,
        metadata = object@meta.data[[paste0("predicted.", use, ".score")]],
        col.name = paste0(ident, ".score")
    )
    object@meta.data[[paste0("predicted.", use, ".score")]] <- NULL

    if (
        is_sct(GetAssay(object)) && is.null(object@commands$PrepSCTFindMarkers)
    ) {
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
ConvertSeuratToAnnData <- function(
    object_or_file,
    outfile,
    assay = NULL,
    subset = NULL,
    log = NULL
) {
    stopifnot(
        "[ConvertSeuratToAnnData] 'object_or_file' should be a Seurat object or a file path" = is.character(
            object_or_file
        ) ||
            inherits(object_or_file, "Seurat")
    )
    monkey_patch("SeuratDisk", "H5SeuratToH5AD", .H5SeuratToH5AD)
    # methods::setMethod(
    #     f = utils::getFromNamespace("WriteH5Group", "SeuratDisk"),
    #     signature = c('x' = 'Assay'),
    #     definition = WriteH5Group.Seurat,
    #     where = .GlobalEnv
    # )

    dig <- digest::digest(
        .sig_str(list(object_or_file, assay, subset)),
        algo = "md5"
    )
    dig <- substr(dig, 1, 8)
    outdir <- dirname(outfile)
    dir.create(outdir, showWarnings = FALSE)
    h5seurat_file <- file.path(
        outdir,
        paste0(
            tools::file_path_sans_ext(basename(outfile)),
            ".",
            dig,
            ".h5seurat"
        )
    )
    if (
        file.exists(h5seurat_file) &&
            !inherits(object_or_file, "Seurat") &&
            (file.mtime(h5seurat_file) < file.mtime(object_or_file))
    ) {
        file.remove(h5seurat_file)
    }

    log <- log %||% get_logger()
    if (
        inherits(object_or_file, "Seurat") ||
            endsWith(object_or_file, ".rds") ||
            endsWith(object_or_file, ".RDS") ||
            endsWith(object_or_file, ".Rds") ||
            endsWith(object_or_file, ".qs") ||
            endsWith(object_or_file, ".qs2")
    ) {
        if (!file.exists(h5seurat_file)) {
            if (is.character(object_or_file)) {
                log$debug(
                    "[ConvertSeuratToAnnData] Reading Seurat object from file ..."
                )
                object_or_file <- read_obj(object_or_file)
            }
            if (!is.null(subset)) {
                log$debug("[ConvertSeuratToAnnData] Subsetting cells ...")
                object_or_file <- filter(
                    object_or_file,
                    !!!rlang::parse_expr(subset)
                )
            }

            assay <- assay %||% DefaultAssay(object_or_file)
            active_ident <- GetIdentityColumn(object_or_file)
            # In order to convert to h5ad
            # https://github.com/satijalab/seurat/issues/8220#issuecomment-1871874649
            if ("RNA" %in% names(object_or_file@assays)) {
                rna_assay <- object_or_file$RNA
                if (inherits(rna_assay, "Assay5")) {
                    # SeuratObject's coerce method (Assay5 -> Assay) fails when
                    # meta.features has 0 columns due to `1:ncol(value)` giving
                    # `c(1, 0)` instead of `integer(0)` in the `[[<-.Assay` method.
                    # Work around by temporarily adding a placeholder column.
                    empty_meta <- ncol(rna_assay[[]]) == 0L
                    if (empty_meta) {
                        rna_assay[["__biopp_placeholder__"]] <- stats::setNames(
                            rep(NA, nrow(rna_assay)),
                            rownames(rna_assay)
                        )
                    }
                    object_or_file$RNA <- as(
                        object = rna_assay,
                        Class = "Assay"
                    )
                    if (empty_meta) {
                        object_or_file$RNA[["__biopp_placeholder__"]] <- NULL
                    }
                } else {
                    object_or_file$RNA <- as(
                        object = object_or_file$RNA,
                        Class = "Assay"
                    )
                }
            }

            log$debug(
                "[ConvertSeuratToAnnData] Saving Seurat object to H5Seurat file ..."
            )
            object_or_file@commands <- list()
            SeuratDisk::SaveH5Seurat(object_or_file, h5seurat_file)

            rm(object_or_file)
            gc()
        } else if (is.null(assay)) {
            if (inherits(object_or_file, "character")) {
                log$debug(
                    "[ConvertSeuratToAnnData] Reading Seurat object from file to get default assay ..."
                )
                object <- read_obj(object_or_file)
            } else {
                object <- object_or_file
            }
            assay <- DefaultAssay(object)
            active_ident <- GetIdentityColumn(object)
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
                log$debug(
                    "[ConvertSeuratToAnnData] Reading H5Seurat file for subsetting ..."
                )
                object_or_file <- SeuratDisk::LoadH5Seurat(object_or_file)
                assay <- assay %||% DefaultAssay(object_or_file)
                active_ident <- GetIdentityColumn(object_or_file)
                object_or_file <- eval(parse(
                    text = paste0(
                        "base::subset(object_or_file, subset = ",
                        subset,
                        ")"
                    )
                ))

                log$debug(
                    "[ConvertSeuratToAnnData] Saving Seurat object to H5Seurat file ..."
                )
                SeuratDisk::SaveH5Seurat(object_or_file, h5seurat_file)
            }
            object_or_file <- h5seurat_file
        }
    }
    if (is.null(assay)) {
        object <- SeuratDisk::LoadH5Seurat(object_or_file)
        assay <- DefaultAssay(object)
        active_ident <- GetIdentityColumn(object)
        rm(object)
        gc()
    }

    log$debug("[ConvertSeuratToAnnData] Converting to AnnData ...")
    SeuratDisk::Convert(
        object_or_file,
        dest = outfile,
        assay = assay,
        overwrite = TRUE
    )

    log$debug("[ConvertSeuratToAnnData] Fixing categorical data ...")
    # See: https://github.com/mojaveazure/seurat-disk/issues/183

    H5.create_reference <- function(self, ...) {
        space <- self$get_space()
        do_call("[", c(list(space), list(...)))
        ref_type <- hdf5r::h5const$H5R_OBJECT
        ref_obj <- hdf5r::H5R_OBJECT$new(1, self)
        res <- .Call(
            "R_H5Rcreate",
            ref_obj$ref,
            self$id,
            ".",
            ref_type,
            space$id,
            FALSE,
            PACKAGE = "hdf5r"
        )
        if (res$return_val < 0) {
            stop("Error creating object reference")
        }
        ref_obj$ref <- res$ref
        return(ref_obj)
    }

    h5ad <- hdf5r::H5File$new(outfile, "r+")
    # Save active assay and identity
    h5ad$create_attr(
        attr_name = "active_assay",
        robj = assay,
        space = hdf5r::H5S$new(type = "scalar")
    )
    if (!is.null(active_ident)) {
        h5ad$create_attr(
            attr_name = "active_ident",
            robj = active_ident,
            space = hdf5r::H5S$new(type = "scalar")
        )
    }
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
#' @param assay Assay name to save in the Seurat object
#' @param ident The name of the identity in metadata after conversion
#' @param log Logger
#' @importFrom rlang %||%
#' @export
#' @return The Seurat object if `outfile` is "<object>", otherwise NULL.
ConvertAnnDataToSeurat <- function(
    infile,
    outfile = NULL,
    assay = NULL,
    ident = NULL,
    log = NULL
) {
    stopifnot(
        "[ConvertAnnDataToSeurat] 'infile' should be a file path with extension '.h5ad'" = is.character(
            infile
        ) &&
            endsWith(infile, ".h5ad")
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
                !endsWith(outfile, ".rds") &&
                !endsWith(outfile, ".RDS") &&
                !endsWith(outfile, ".Rds") &&
                !endsWith(outfile, ".qs") &&
                !endsWith(outfile, ".qs2")
        ) {
            stop(
                "[ConvertAnnDataToSeurat] 'outfile' should be either NULL or ",
                "a file path with extension '.h5seurat', '.rds', '.RDS', '.Rds', '.qs' or '.qs2'"
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

    log$debug(
        "[ConvertAnnDataToSeurat] Converting h5ad file to h5seurat file ..."
    )
    fin <- hdf5r::H5File$new(infile, "r")
    if (is.null(assay)) {
        if (fin$attr_exists("active_assay")) {
            assay <- hdf5r::h5attr(fin, "active_assay")
        } else {
            assay <- "RNA"
        }
    }
    if (is.null(ident)) {
        if (fin$attr_exists("active_ident")) {
            ident <- hdf5r::h5attr(fin, "active_ident")
        } else {
            ident <- NULL
        }
    }
    fin$close_all()
    SeuratDisk::Convert(infile, destfile, assay = assay, overwrite = TRUE)

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
    if (!is.null(assay)) {
        DefaultAssay(object) <- assay
    }
    if (!is.null(ident)) {
        if (ident %in% colnames(object@meta.data)) {
            SeuratObject::Idents(object) <- ident
        } else {
            stop(
                "[ConvertAnnDataToSeurat] 'ident' column '{ident}' not found in metadata/obs"
            )
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
    log = NULL
) {
    log <- log %||% get_logger()

    # Validate inputs
    stopifnot(
        "'aggregate_by' columns not found in metadata" = all(
            aggregate_by %in% colnames(object@meta.data)
        )
    )
    stopifnot("'assay' not found in object" = assay %in% names(object@assays))

    if (!is.null(subset)) {
        log$info("Subsetting cells based on: {subset}")
        object <- filter(object, !!rlang::parse_expr(subset))
    }

    log$info(
        "Aggregating expression by: {paste(aggregate_by, collapse = ', ')}"
    )

    # Get expression data
    expr_data <- GetAssayData(object, assay = assay, layer = layer)

    # Get metadata for aggregation
    metadata <- object@meta.data[, aggregate_by, drop = FALSE]
    metadata <- tidyr::unite(
        metadata,
        "Sample",
        !!!syms(aggregate_by),
        remove = FALSE
    )

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

    log$info(
        "Aggregation complete. Matrix dimensions: {nrow(aggregated_matrix)} x {ncol(aggregated_matrix)}"
    )

    attr(aggregated_matrix, "meta") <- meta_df
    return(aggregated_matrix)
}
