#' Run transformations on a Seurat object
#'
#' @param object Seurat object
#' @param use_sct Whether to use [Seurat::SCTransform]
#' @param SCTransformArgs Arguments to pass to [Seurat::SCTransform]
#' @param NormalizeDataArgs Arguments to pass to [Seurat::NormalizeData]
#' @param FindVariableFeaturesArgs Arguments to pass to [Seurat::FindVariableFeatures]
#' @param ScaleDataArgs Arguments to pass to [Seurat::ScaleData]
#' You can use `features = "__all__"` to scale all features, which will be expanded to all features in the object.
#' @param RunPCAArgs Arguments to pass to [Seurat::RunPCA]
#' @param from_ccs Whether this is called from [RunSeuratCellCycleScoring].
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
    from_ccs = FALSE,
    cache = NULL
) {
    log <- log %||% get_logger()
    cache <- cache %||% gettempdir()
    cached <- Cache$new(
        list(
            object,
            use_sct,
            SCTransformArgs,
            NormalizeDataArgs,
            FindVariableFeaturesArgs,
            ScaleDataArgs,
            RunPCAArgs,
            from_ccs
        ),
        prefix = "biopipen.utils.RunSeuratTransformations",
        cache_dir = cache
    )
    if (cached$is_cached()) {
        if (from_ccs) {
            log$info("  Transformed data loaded from cache: {cached$get_path()}")
        } else {
            log$info("Transformed data loaded from cache: {cached$get_path()}")
        }
        return(cached$restore())
    }

    if (from_ccs) {
        run_pca <- FALSE
        log_prefix <- "  "
        log$info("  Performing transformations for cell cycle scoring ...")
    } else {
        run_pca <- TRUE
        log_prefix <- "- "
        log$info("Performing data transformation and scaling ...")
    }
    # Not joined yet
    # object[["RNA"]] <- split(object[["RNA"]], f = object$Sample)
    if (use_sct) {
        log$info("{log_prefix}Running SCTransform ...")
        # log to stdout but don't populate it to running log
        log$debug("  Arguments: {format_args(SCTransformArgs)}")
        SCTransformArgs$object <- object
        object <- do_call(SCTransform, SCTransformArgs)
        SCTransformArgs$object <- NULL
        gc()
    } else {
        log$info("{log_prefix}Running NormalizeData ...")
        log$debug("  Arguments: {format_args(NormalizeDataArgs)}")
        NormalizeDataArgs$object <- object
        object <- do_call(NormalizeData, NormalizeDataArgs)
        NormalizeDataArgs$object <- NULL
        gc()

        log$info("{log_prefix}Running FindVariableFeatures ...")
        log$debug("  Arguments: {format_args(FindVariableFeaturesArgs)}")
        FindVariableFeaturesArgs$object <- object
        object <- do_call(FindVariableFeatures, FindVariableFeaturesArgs)
        FindVariableFeaturesArgs$object <- NULL
        gc()

        log$info("{log_prefix}Running ScaleData ...")
        log$debug("  Arguments: {format_args(ScaleDataArgs)}")
        ScaleDataArgs$object <- object
        if (identical(ScaleDataArgs$features, "__all__")) {
            ScaleDataArgs$features <- rownames(object)
        }
        object <- do_call(ScaleData, ScaleDataArgs)
        ScaleDataArgs$object <- NULL
        gc()
    }

    if (run_pca) {
        RunPCAArgs$dims <- RunPCAArgs$dims %||% 1:min(30, ncol(object) - 1)
        RunPCAArgs$dims <- .expand_number(RunPCAArgs$dims)
        # https://github.com/satijalab/seurat/issues/1914#issuecomment-1008728797
        RunPCAArgs$npcs <- min(
            RunPCAArgs$npcs %||% 50,
            ncol(object) - 1,
            nrow(object) - 1
        )
        log$info("{log_prefix}Running RunPCA (npcs={RunPCAArgs$npcs}) ...")
        log$debug("  RunPCA: {format_args(RunPCAArgs)}")
        RunPCAArgs$object <- object
        object <- do_call(RunPCA, RunPCAArgs)
        RunPCAArgs$object <- NULL
        gc()
    }

    object <- AddSeuratCommand(
        object,
        name = "RunSeuratTransformations",
        call.string = "RunSeuratTransformations(object, use_sct, SCTransformArgs, NormalizeDataArgs, FindVariableFeaturesArgs, ScaleDataArgs, RunPCAArgs, run_pca)",
        params = list(
            use_sct = use_sct,
            SCTransformArgs = SCTransformArgs,
            NormalizeDataArgs = NormalizeDataArgs,
            FindVariableFeaturesArgs = FindVariableFeaturesArgs,
            ScaleDataArgs = ScaleDataArgs,
            RunPCAArgs = RunPCAArgs,
            from_ccs = from_ccs
        )
    )
    invisible(gc())
    # gc before serialization: qs2 allocations don't trigger R's gc
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
#' If `RunUMAPArgs$features` is a numeric value, it will be treated as the `n` field
#' in the list above, with the default `order` being "desc(abs(avg_log2FC))".
#' `RunUMAPArgs$from` can be a path to a TSV file containing pre-computed UMAP
#' coordinates to overwrite the results of [Seurat::RunUMAP()]. The path should be
#' in the format `"file:///path/to/file#id_col,coord_col1,coord_col2"`, where
#' `id_col` is the column with cell barcodes and the remaining columns are the UMAP
#' coordinate columns (e.g., `UMAP_1,UMAP_2`). The cell barcodes must match those
#' in the Seurat object, and the coordinate column names must match the reduction's
#' `cell.embeddings` column names. You can also use 1-based column indices instead of names, e.g., `"file:///path/to/file#1,2,3"`.
#' @details
#' When both `RunUMAPArgs$features` and `RunUMAPArgs$dims` are provided,
#' `RunUMAPArgs$dims` will be ignored.
#' If neither `RunUMAPArgs$features` nor `RunUMAPArgs$dims` is provided,
#' `RunUMAPArgs$dims` will be set to `1:min(30, ceiling(ncells/3), ncol(object@reductions[[reduction]]))`,
#' where `ncells` is the number of cells in the object, and `reduction` is `RunUMAPArgs$reduction` (default: "pca").
#' @export
#' @param cache Directory to cache the all markers, which can be reused later if
#' DE analysis is desired.
#' @param log The logger to use. If NULL, a default logger will be used.
#' @return The Seurat object with UMAP results
#' @importFrom Seurat RunUMAP
#' @importFrom SeuratObject Idents
#' @importFrom rlang %||% parse_expr
#' @importFrom dplyr arrange group_by slice_head pull
RunSeuratUMAP <- function(
    object,
    RunUMAPArgs = list(),
    cache = NULL,
    log = NULL
) {
    log <- log %||% get_logger()
    if (length(RunUMAPArgs$features) == 1 && is.numeric(RunUMAPArgs$features)) {
        RunUMAPArgs$features <- list(
            order = "desc(abs(avg_log2FC))",
            n = RunUMAPArgs$features
        )
    }
    if (is.list(RunUMAPArgs$features)) {
        log$info("  Running RunSeuratDEAnalysis to get markers for UMAP ...")
        ident <- GetIdentityColumn(object)
        markers <- RunSeuratDEAnalysis(object, group_by = ident, cache = cache)
        attr(markers, "object") <- NULL
        gc()

        RunUMAPArgs$features$order <- RunUMAPArgs$features$order %||%
            "desc(abs(avg_log2FC))"
        RunUMAPArgs$features$n <- RunUMAPArgs$features$n %||% 30

        ngroups <- length(unique(markers[[ident]]))
        each_n <- ceiling(RunUMAPArgs$features$n / ngroups)
        RunUMAPArgs$features <- markers %>%
            arrange(!!parse_expr(RunUMAPArgs$features$order)) %>%
            group_by(!!sym(ident)) %>%
            slice_head(n = each_n) %>%
            pull("gene") %>%
            unique()

        rm(markers)
        gc()
    }
    ncells <- ncol(object)
    reduction <- RunUMAPArgs[['reduction']] %||%
        object@misc$integrated_new_reduction %||%
        "pca"
    log_piece <- paste0("reduction=", reduction)
    if (!is.null(RunUMAPArgs$features) && !is.null(RunUMAPArgs$dims)) {
        log$warn(
            "  'RunUMAPArgs$features' and 'RunUMAPArgs$dims' are both set, 'RunUMAPArgs$dims' will be ignored"
        )
        log_piece <- c(
            log_piece,
            paste0("features=", length(RunUMAPArgs$features))
        )
    } else if (is.null(RunUMAPArgs$features)) {
        RunUMAPArgs$dims <- RunUMAPArgs$dims %||%
            1:min(30, ceiling(ncells / 3), ncol(object@reductions[[reduction]]))
        RunUMAPArgs$dims <- .expand_number(RunUMAPArgs$dims)
        log_piece <- c(log_piece, paste0("dims=1:", max(RunUMAPArgs$dims)))
    }
    RunUMAPArgs$umap.method <- RunUMAPArgs$umap.method %||% "uwot"
    if (identical(RunUMAPArgs$umap.method, "umap-learn")) {
        RunUMAPArgs$metric <- RunUMAPArgs$metric %||% "correlation"
    }
    log_piece <- c(log_piece, paste0("umap.method=", RunUMAPArgs$umap.method))
    if (RunUMAPArgs$umap.method == "uwot") {
        # https://github.com/satijalab/seurat/issues/4312
        RunUMAPArgs$n.neighbors <- RunUMAPArgs$n.neighbors %||%
            min(ncells - 1, 30)
        log_piece <- c(
            log_piece,
            paste0("n.neighbors=", RunUMAPArgs$n.neighbors)
        )
    }
    log$info("  {paste(log_piece, collapse=', ')}")
    RunUMAPArgs$object <- object
    if (!is.null(RunUMAPArgs$from)) {
        umap_from <- RunUMAPArgs$from
        RunUMAPArgs$from <- NULL
    } else {
        umap_from <- NULL
    }
    object <- do_call(RunUMAP, RunUMAPArgs)
    RunUMAPArgs$object <- NULL
    gc()

    # Use umap_from to overwrite the UMAP results in the original reduction if specified
    if (!is.null(umap_from)) {
        if (startsWith(umap_from, "file://")) {
            umap_from <- sub("^file://", "", umap_from)
        }
        if (!file.exists(umap_from)) {
            stop(
                "[RunSeuratUMAP] The specified 'from' file does not exist: ",
                umap_from
            )
        }
        log$info("  Overwriting UMAP results from given 'from' file ...")
        if (!grepl("#", umap_from)) {
            stop(
                "[RunSeuratUMAP] The specified 'from' file must contain a '#' to specify the column name of the UMAP results, e.g. 'file://path/to/file#cell_id,UMAP_1,UMAP_2'"
            )
        }
        umap_file_parts <- strsplit(umap_from, "#")[[1]]
        umap_file_path <- umap_file_parts[1]
        umap_col_names <- trimws(strsplit(umap_file_parts[2], ",")[[1]])
        if (length(umap_col_names) < 2) {
            stop(
                "[RunSeuratUMAP] At least two column names must be specified after '#': the cell identifier column and at least one UMAP coordinate column, e.g. 'file://path/to/file#cell_id,UMAP_1,UMAP_2'"
            )
        }
        umap_data <- utils::read.table(
            umap_file_path,
            header = TRUE,
            sep = "\t",
            stringsAsFactors = FALSE
        )
        umap_col_names <- vapply(
            umap_col_names,
            function(col) {
                if (grepl("^[+-]?\\d+$", col)) {
                    col <- as.integer(col)
                    if (col < 0) {
                        col <- ncol(umap_data) + col + 1
                    }
                    if (col < 1 || col > ncol(umap_data)) {
                        stop(
                            "[RunSeuratUMAP] The specified column index is out of bounds: ",
                            col
                        )
                    }
                    col <- colnames(umap_data)[col]
                }
                col
            },
            character(1)
        )
        if (!all(umap_col_names %in% colnames(umap_data))) {
            stop(
                "[RunSeuratUMAP] The specified column names do not exist in the 'from' file: ",
                paste(
                    umap_col_names[!umap_col_names %in% colnames(umap_data)],
                    collapse = ", "
                )
            )
        }
        umap_data <- umap_data[, umap_col_names, drop = FALSE]
        if (anyDuplicated(umap_data[[umap_col_names[1]]])) {
            stop(
                "[RunSeuratUMAP] Duplicate cell barcodes found in the 'from' file column '",
                umap_col_names[1],
                "'"
            )
        }
        rownames(umap_data) <- umap_data[[umap_col_names[1]]]
        umap_data <- umap_data[, -1, drop = FALSE]
        umap_name <- RunUMAPArgs$reduction.name %||% "umap"
        if (
            !all(
                rownames(object@reductions[[umap_name]]) %in%
                    rownames(umap_data)
            )
        ) {
            stop(
                "[RunSeuratUMAP] The cell names in the 'from' file do not match the cell names in the Seurat object."
            )
        }
        if (
            ncol(umap_data) >
                ncol(object@reductions[[umap_name]]@cell.embeddings)
        ) {
            stop(
                "[RunSeuratUMAP] The number of UMAP dimensions in the 'from' file is greater than the number of dimensions in the Seurat object."
            )
        }
        object@reductions[[umap_name]]@cell.embeddings[,
            1:ncol(umap_data)
        ] <- umap_data[
            rownames(object@reductions[[umap_name]]@cell.embeddings),
            ,
            drop = FALSE
        ]
    }

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
#' @examples
#' \donttest{
#' obj <- RunSeuratClustering(SeuratObject::pbmc_small)
#' GetIdentityColumn(obj)
#' table(obj$seurat_clusters)
#' }
RunSeuratClustering <- function(
    object,
    RunPCAArgs = list(),
    RunUMAPArgs = list(),
    FindNeighborsArgs = list(),
    FindClustersArgs = list(),
    log = NULL,
    cache = NULL
) {
    log <- log %||% get_logger()
    cache <- cache %||% gettempdir()

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
        RunPCAArgs$dims <- RunPCAArgs$dims %||% 1:min(30, ncells - 1)
        RunPCAArgs$dims <- .expand_number(RunPCAArgs$dims)
        # https://github.com/satijalab/seurat/issues/1914#issuecomment-1008728797
        RunPCAArgs$npcs <- min(
            RunPCAArgs$npcs %||% 50,
            ncol(object) - 1,
            nrow(object) - 1
        )
        RunPCAArgs$object <- object
        log$info(
            "Running RunPCA (npcs={RunPCAArgs$npcs}, dims=1:{max(RunPCAArgs$dims)}) ..."
        )
        log$debug("  Arguments: {format_args(RunPCAArgs)}")
        object <- do_call(RunPCA, RunPCAArgs)
        RunPCAArgs$object <- NULL
        gc()

        caching$save(object)
    }

    caching <- Cache$new(
        list(object, FindNeighborsArgs),
        prefix = "biopipen.utils.RunSeuratClustering.FindNeighbors",
        cache_dir = cache
    )
    if (caching$is_cached()) {
        log$info("FindNeighbors results loaded from cache")
        object <- caching$restore()
    } else {
        FindNeighborsArgs$reduction <- FindNeighborsArgs[['reduction']] %||%
            object@misc$integrated_new_reduction %||%
            "pca"
        FindNeighborsArgs$object <- object
        if (!is.null(FindNeighborsArgs$dims)) {
            FindNeighborsArgs$dims <- .expand_number(FindNeighborsArgs$dims)
            log$info(
                "Running FindNeighbors (reduction={FindNeighborsArgs$reduction}, dims=1:{max(FindNeighborsArgs$dims)}) ..."
            )
        } else {
            log$info(
                "Running FindNeighbors (reduction={FindNeighborsArgs$reduction}) ..."
            )
        }
        log$debug("  Arguments: {format_args(FindNeighborsArgs)}")
        object <- do_call(FindNeighbors, FindNeighborsArgs)
        FindNeighborsArgs$object <- NULL
        gc()

        caching$save(object)
    }

    caching <- Cache$new(
        list(object, FindClustersArgs),
        prefix = "biopipen.utils.RunSeuratClustering.FindClusters",
        cache_dir = cache
    )
    if (caching$is_cached()) {
        log$info("FindClusters results loaded from cache")
        object <- caching$restore()
    } else {
        log$info("Running FindClusters ...")
        log$debug("  Arguments: {format_args(FindClustersArgs)}")

        # FindClustersArgs$graph.name <- FindClustersArgs$graph.name %||% "RNA_snn"
        FindClustersArgs$object <- object
        FindClustersArgs$random.seed <- FindClustersArgs$random.seed %||% 8525
        resolution <- .expand_findclusters_resolution(
            FindClustersArgs$resolution %||% 0.8
        )
        cluster_name <- FindClustersArgs$cluster.name %||% "seurat_clusters"
        if (length(cluster_name) > 1) {
            stop(
                "[RunSeuratClustering] 'FindClustersArgs$cluster.name' only supports a single name"
            )
        }
        # cluster.name <- paste0(cluster_name, ".", FindClustersArgs$resolution)
        # FindClustersArgs$cluster.name[length(FindClustersArgs$cluster.name)] <- cluster_name
        # log$info("  Using resolution(s): {paste(FindClustersArgs$resolution, collapse = ', ')}")
        for (i in seq_along(resolution)) {
            log$info("  applying resolution: {resolution[i]}")
            FindClustersArgs$resolution <- resolution[i]
            FindClustersArgs$cluster.name <- paste0(
                cluster_name,
                ".",
                resolution[i]
            )
            FindClustersArgs$object <- do_call(FindClusters, FindClustersArgs)
        }
        # object <- do_call(FindClusters, FindClustersArgs)
        object <- FindClustersArgs$object
        FindClustersArgs$object <- NULL
        gc()

        for (clname in FindClustersArgs$cluster.name) {
            object@meta.data[[clname]] <- .recode_clusters(object@meta.data[[
                clname
            ]])
        }
        object@meta.data[[
            cluster_name
        ]] <- object@meta.data[[FindClustersArgs$cluster.name[length(
            FindClustersArgs$cluster.name
        )]]]
        # object@meta.data$seurat_clusters <- .recode_clusters(object@meta.data$seurat_clusters)
        Idents(object) <- cluster_name

        ident_table <- table(object@meta.data[[cluster_name]])
        ident_table <- paste0(names(ident_table), "(", ident_table, ")")
        log$info(
            "  Found clusters (with resolution {FindClustersArgs$resolution[length(FindClustersArgs$resolution)]}):"
        )
        # log every 5 clusters
        for (i in seq(1, length(ident_table), by = 5)) {
            log$info(
                "   | {paste(ident_table[i:min(i + 4, length(ident_table))], collapse = ', ')}"
            )
        }

        caching$save(object)
    }

    caching <- Cache$new(
        list(object, RunUMAPArgs),
        prefix = "biopipen.utils.RunSeuratClustering.RunSeuratUMAP",
        cache_dir = cache
    )
    if (caching$is_cached()) {
        log$info("UMAP results loaded from cache")
        object <- caching$restore()
    } else {
        log$info("Running RunUMAP ...")
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
    cache = NULL
) {
    log <- log %||% get_logger()
    cache <- cache %||% gettempdir()
    cached <- Cache$new(
        list(
            object,
            subset,
            name,
            RunPCAArgs,
            RunUMAPArgs,
            FindNeighborsArgs,
            FindClustersArgs
        ),
        prefix = "biopipen.utils.RunSeuratSubClustering",
        cache_dir = cache
    )
    if (cached$is_cached()) {
        log$info("Subset clustering results loaded from cache: {cached$get_path()}")
        return(cached$restore())
    }
    log$info("Subsetting seurat object ({subset}) ...")
    subobj <- filter(object, !!rlang::parse_expr(subset))
    if (ncol(subobj) < 10) {
        stop(
            "[RunSeuratSubClustering] Not enough (< 10) cells to perform clustering"
        )
    }

    if (name %in% colnames(object@meta.data)) {
        stop(paste0(
            "[RunSeuratSubClustering] Name '",
            name,
            "' already exists in the metadata. Please choose a different name."
        ))
    }

    RunPCAArgs$object <- subobj
    RunPCAArgs$reduction.key <- RunPCAArgs$reduction.key %||%
        paste0(toupper(name), "PC_")
    RunPCAArgs$dims <- RunPCAArgs$dims %||% 1:min(30, ncol(subobj) - 1)
    RunPCAArgs$dims <- .expand_number(RunPCAArgs$dims)
    # https://github.com/satijalab/seurat/issues/1914#issuecomment-1008728797
    RunPCAArgs$npcs <- min(
        RunPCAArgs$npcs %||% 50,
        min(ncol(subobj), nrow(subobj)) - 1
    )
    log$info(
        "- Running RunPCA (reduction.key={RunPCAArgs$reduction.key}, dims=1:{max(RunPCAArgs$dims)}, npcs={RunPCAArgs$npcs}) ..."
    )
    log$debug("  Arguments: {format_args(RunPCAArgs)}")
    subobj <- do_call(RunPCA, RunPCAArgs)
    RunPCAArgs$object <- NULL
    gc()

    FindNeighborsArgs$object <- subobj
    FindNeighborsArgs$reduction <- FindNeighborsArgs[['reduction']] %||%
        subobj@misc$integrated_new_reduction %||%
        "pca"
    if (!is.null(FindNeighborsArgs$dims)) {
        FindNeighborsArgs$dims <- .expand_number(FindNeighborsArgs$dims)
        log$info(
            "- Running FindNeighbors (reduction={FindNeighborsArgs$reduction}, dims=1:{max(FindNeighborsArgs$dims)}) ..."
        )
    } else {
        log$info(
            "- Running FindNeighbors (reduction={FindNeighborsArgs$reduction}) ..."
        )
    }
    log$debug("  Arguments: {format_args(FindNeighborsArgs)}")
    subobj <- do_call(FindNeighbors, FindNeighborsArgs)
    FindNeighborsArgs$object <- NULL
    gc()

    log$info("- Running FindClusters ...")
    FindClustersArgs$object <- subobj
    FindClustersArgs$random.seed <- FindClustersArgs$random.seed %||% 8525
    resolution <- .expand_findclusters_resolution(
        FindClustersArgs$resolution %||% 0.8
    )
    cluster_name <- paste0(name, ".", FindClustersArgs$resolution)
    # FindClustersArgs$cluster.name[length(FindClustersArgs$cluster.name)] <- name
    # log$info("  Using resolution(s): {paste(FindClustersArgs$resolution, collapse = ', ')}")
    log$debug("  Arguments: {format_args(FindClustersArgs)}")
    # subobj <- do_call(FindClusters, FindClustersArgs)
    for (i in seq_along(resolution)) {
        log$info("  applying resolution: {resolution[i]}")
        FindClustersArgs$resolution <- resolution[i]
        FindClustersArgs$cluster.name <- paste0(name, ".", resolution[i])
        FindClustersArgs$object <- do_call(FindClusters, FindClustersArgs)
    }
    subobj <- FindClustersArgs$object
    FindClustersArgs$object <- NULL
    gc()

    for (clname in FindClustersArgs$cluster.name) {
        subobj@meta.data[[clname]] <- .recode_clusters(
            subobj@meta.data[[clname]],
            prefix = "s"
        )
    }
    subobj@meta.data[[
        name
    ]] <- subobj@meta.data[[FindClustersArgs$cluster.name[length(
        FindClustersArgs$cluster.name
    )]]]
    # subobj@meta.data[[name]] <- .recode_clusters(subobj@meta.data$seurat_clusters, prefix = "s")
    Idents(subobj) <- name

    ident_table <- table(subobj@meta.data[[name]])
    ident_table <- paste0(names(ident_table), "(", ident_table, ")")
    log$info(
        "  Found subclusters (with resolution {FindClustersArgs$resolution[length(FindClustersArgs$resolution)]}):"
    )
    # log every 5 clusters
    for (i in seq(1, length(ident_table), by = 5)) {
        log$info(
            "   | {paste(ident_table[i:min(i + 4, length(ident_table))], collapse = ', ')}"
        )
    }

    RunUMAPArgs$reduction.key <- RunUMAPArgs$reduction.key %||%
        paste0(toupper(name), "UMAP_")
    RunUMAPArgs$reduction <- RunUMAPArgs[['reduction']] %||%
        object@misc$integrated_new_reduction %||%
        "pca"
    log$info("- Running RunUMAP ...")
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
        object@meta.data[[clname]] <- subobj@meta.data[
            colnames(object),
            clname,
            drop = TRUE
        ]
    }
    object@meta.data[[name]] <- subobj@meta.data[
        colnames(object),
        name,
        drop = TRUE
    ]

    # Add the subclusters reduction to the original object
    object@reductions[[paste0(name, ".pca")]] <- subobj@reductions[["pca"]]
    object@reductions[[paste0(name, ".umap")]] <- subobj@reductions[["umap"]]

    cached$save(object)
    object
}
#' Rename cluster names
#'
#' @param object Seurat object
#' @param ident Column name in `@meta.data` to rename
#' Defaults to the identity column (from `GetIdentityColumn()`)
#' When `ident` is not identical as the column identified by `GetIdentityColumn()`,
#' @param save_as If not NULL, save the renamed clusters to this new column.
#' If NULL, will overwrite the original `ident` column.
#' @param merge Whether to merge the clusters with the same new name.
#' Default is FALSE. Otherwise, the clusters with the same new name will be suffixed
#' with ".1", ".2", etc. to make them unique.
#' @param backup If not NULL, the original `ident` column will be backed up
#' to this new column before renaming.
#' Default is NULL, which means no backup will be made.
#' Note that if the column name already exists, it will be overwritten.
#' @param ... Additional arguments.
#' If named arguments are provided, they will be used as the mapping from old cluster names to new cluster names.
#' * If a named argument's name does not exist in the original cluster names, it will be ignored.
#' * If a original cluster name does not have a corresponding named argument, it will be kept unchanged.
#' * If you want to exclude some clusters, you can set their new names to `NA`.
#' If unnamed arguments are provided, it must be a single list of mappings.
#' with the names being the original cluster names, and the values being the new cluster names.
#' @details
#' This function provides a convenient way to rename clusters in a Seurat object.
#' You can provide the mapping of old cluster names to new cluster names either
#' as named arguments or as a single list.
#' Unlike [Seurat::RenameIdents()], this function also renames the cluster names in the metadata column,
#' so that the changes are persistent. If `save_as` is provided, the renamed clusters will be saved
#' to a new column, otherwise the original column will be overwritten.
#' In addition, the order of the clusters will be preserved based on the order of the original cluster names.
#' For the default identities (`Idents(object)`), it is only changed when `ident` is NULL or identical
#' to the identity column (`GetIdentityColumn(object)`).
#' @return The Seurat object with renamed clusters
#' @export
#' @importFrom SeuratObject Idents
#' @importFrom rlang %||%
#' @examples
#' \donttest{
#' object <- scplotter::pancreas_sub
#' GetIdentityColumn(object)
#' table(object$seurat_clusters)
#'
#' # Rename using Seurat::RenameIdents
#' object1 <- Seurat::RenameIdents(object, `2` = 'Alpha', `1` = 'Beta')
#' # The order of identities is changed
#' table(SeuratObject::Idents(object1))
#' # The identity column is not changed
#' table(object1$seurat_clusters)
#' # Since the inconsistency, GetIdentityColumn can't determine the identity column
#' GetIdentityColumn(object1)
#'
#' # Rename using RenameSeuratIdents
#' object1 <- RenameSeuratIdents(object, `2` = 'Alpha', `1` = 'Beta')
#' # The order of identities is preserved
#' table(SeuratObject::Idents(object1))
#' # The identity column is also changed
#' table(object1$seurat_clusters)
#' # Since the consistency, GetIdentityColumn can still determine the identity column
#' GetIdentityColumn(object1)
#'
#' # Rename a non-identity column
#' table(object$CellType)
#' object1 <- RenameSeuratIdents(
#'     object, Ductal = 'C1', `Endocrine` = 'C1', ident = 'CellType')
#' table(object1$CellType)
#' table(object1$seurat_clusters)
#' # Identity column is not changed
#' GetIdentityColumn(object1)
#'
#' # Merge clusters with the same new name
#' object1 <- RenameSeuratIdents(
#'    object, Ductal = 'C1', `Endocrine` = 'C1', ident = 'CellType', merge = TRUE)
#' table(object1$CellType)
#'
#' # Exclude some clusters by setting their new names to NA
#' object1 <- RenameSeuratIdents(
#'    object, Ductal = 'C1', `Endocrine` = NA, ident = 'CellType')
#' table(object1$CellType)
#' }
RenameSeuratIdents <- function(
    object,
    ident = NULL,
    save_as = NULL,
    merge = FALSE,
    backup = NULL,
    ...
) {
    ident <- ident %||% GetIdentityColumn(object)
    if (is.null(ident)) {
        stop(
            "[RenameSeuratIdents] Cannot determine the identity column. Please provide the 'ident' argument."
        )
    }
    if (!ident %in% colnames(object@meta.data)) {
        object@meta.data[[ident]] <- Idents(object)
    }
    if (!is.factor(object@meta.data[[ident]])) {
        object@meta.data[[ident]] <- as.factor(object@meta.data[[ident]])
    }
    dots <- rlang::dots_list(...)

    if (
        length(dots) == 1 &&
            (is.null(names(dots)) ||
                identical(names(dots), "") ||
                identical(names(dots), "mapping")) &&
            is.list(dots[[1]])
    ) {
        mapping <- dots[[1]]
    } else if (length(dots) >= 1) {
        mapping <- dots
    } else {
        stop("[RenameSeuratIdents] No mapping provided to rename clusters")
    }

    unchanged <- setdiff(levels(object@meta.data[[ident]]), names(mapping))
    if (length(unchanged) > 0) {
        for (cl in unchanged) {
            mapping[[cl]] <- cl
        }
    }

    # Preserve the order of the original levels
    mapping <- mapping[levels(object@meta.data[[ident]])]
    if (anyNA(unlist(mapping))) {
        na_clusters <- names(mapping)[is.na(unlist(mapping))]
        warning(paste0(
            "[RenameSeuratIdents] The following clusters will be excluded: ",
            paste(na_clusters, collapse = ", ")
        ))
        remaining_clusters <- setdiff(names(mapping), na_clusters)
        subset_seurat <- utils::getFromNamespace("subset_seurat", "scplotter")
        object <- subset_seurat(
            object,
            subset = !!rlang::sym(ident) %in% remaining_clusters
        )
        mapping <- mapping[remaining_clusters]
    }

    if (!isTRUE(merge)) {
        new_names <- unlist(mapping)
        dup_names <- new_names[duplicated(new_names)]
        if (length(dup_names) > 0) {
            for (dup in unique(dup_names)) {
                indices <- which(new_names == dup)
                for (i in seq_along(indices)) {
                    new_names[indices[i]] <- paste0(dup, ".", i)
                }
            }
            mapping <- as.list(new_names)
            names(mapping) <- names(new_names)
        }
    }

    if (!is.null(backup)) {
        if (backup %in% colnames(object@meta.data)) {
            warning(
                paste0(
                    "[RenameSeuratIdents] Backup column '",
                    backup,
                    "' already exists. It will be overwritten."
                ),
                immediate. = TRUE
            )
        }
        object@meta.data[[backup]] <- object@meta.data[[ident]]
    }

    new_levels <- unique(unlist(mapping))

    # Now perform the renaming
    idents <- object@meta.data[[ident]]
    rev_mapping <- setNames(names(mapping), as.character(unlist(mapping)))
    idents <- forcats::fct_recode(idents, !!!rev_mapping)
    idents <- factor(idents, levels = new_levels)

    if (identical(ident, GetIdentityColumn(object))) {
        Idents(object) <- idents
    }
    save_as <- save_as %||% ident
    object@meta.data[[save_as]] <- idents

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
    object,
    no_integration = FALSE,
    IntegrateLayersArgs = list(),
    log = NULL,
    cache = NULL
) {
    log <- log %||% get_logger()
    cache <- cache %||% gettempdir()
    cached <- Cache$new(
        list(object, no_integration, IntegrateLayersArgs),
        prefix = "biopipen.utils.RunSeuratIntegration",
        cache_dir = cache
    )
    if (cached$is_cached()) {
        log$info("Integrated data loaded from cache: {cached$get_path()}")
        return(cached$restore())
    }

    log$info("Performing data integration ...")
    if (!no_integration) {
        method <- IntegrateLayersArgs$method %||% "rpca"
        if (
            !is.null(IntegrateLayersArgs$reference) &&
                is.character(IntegrateLayersArgs$reference)
        ) {
            log$info(
                "  Using reference samples: {paste(IntegrateLayersArgs$reference, collapse = ', ')}"
            )
            samples <- if (is.factor(object$Sample)) {
                levels(object$Sample)
            } else {
                unique(object$Sample)
            }
            IntegrateLayersArgs$reference <- match(
                IntegrateLayersArgs$reference,
                samples
            )
            log$debug(
                "  Transferred to indices: {paste(IntegrateLayersArgs$reference, collapse = ', ')}"
            )
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
        IntegrateLayersArgs$assay <- IntegrateLayersArgs$assay %||%
            DefaultAssay(object)
        if (IntegrateLayersArgs$assay == "SCT") {
            IntegrateLayersArgs$normalization.method <- IntegrateLayersArgs$normalization.method %||%
                "SCT"
        }

        new_reductions <- list(
            "CCAIntegration" = "integrated.cca",
            "RPCAIntegration" = "integrated.rpca",
            "HarmonyIntegration" = "harmony",
            "FastMNNIntegration" = "integration.mnn",
            "scVIIntegration" = "integrated.scvi"
        )
        IntegrateLayersArgs$new.reduction <- IntegrateLayersArgs$new.reduction %||%
            new_reductions[[method]]

        log$debug("  Arguments: {format_args(IntegrateLayersArgs)}")
        IntegrateLayersArgs$object <- object
        object <- do_call(IntegrateLayers, IntegrateLayersArgs)
        IntegrateLayersArgs$object <- NULL
        gc()

        # Save it for dimension reduction plots
        object@misc$integrated_new_reduction <- IntegrateLayersArgs$new.reduction
    }

    log$info("- Joining layers ...")
    # https://github.com/satijalab/seurat/issues/8558#issuecomment-2591323978
    object <- JoinLayers(object, assay = "RNA")
    object <- AddSeuratCommand(
        object,
        "RunSeuratIntegration",
        "RunSeuratIntegration(object, no_integration, IntegrateLayersArgs)",
        params = list(
            no_integration = no_integration,
            IntegrateLayersArgs = IntegrateLayersArgs
        )
    )

    if (inherits(object[[DefaultAssay(object)]], "SCTAssay")) {
        log$info("- Running PrepSCTFindMarkers() ...")
        object <- PrepSCTFindMarkers(object)
        object <- AddSeuratCommand(object, "PrepSCTFindMarkers")
    }

    invisible(gc())
    # gc before serialization: qs2 allocations don't trigger R's gc
    cached$save(object)
    object
}
#' Run Seurat CellCycleScoring
#'
#' This function will run Seurat's CellCycleScoring on the given Seurat object and add the scores and predicted cell cycle phase to the metadata.
#' It also supports mouse data.
#' @param object Seurat object
#' @param s.features The gene names for S phase scoring.
#' Default is `cc.genes$s.genes` for human and the following genes for mouse:
#' Mcm5, Pcna, Tyms, Fen1, Mcm7, Mcm4, Rrm1, Ung, Gins2, Mcm6, Cdca7, Dtl, Prim1, Uhrf1,
#' Cenpu, Hells, Rfc2, Polr1b, Nasp, Rad51ap1, Gmnn, Wdr76, Slbp, Ccne2, Ubr7, Msh2, Rad51,
#' Rrm2, Cdc45, Cdc6, Exo1, Tipin, Dscc1, Blm, Casp8ap2, Usp1, Clspn, Pola1, Chaf1b, Mrpl36, E2f8
#' @param g2m.features The gene names for G2/M phase scoring.
#' Default is `cc.genes$g2m.genes` for human and the following genes for mouse:
#' Hmgb2, Cdk1, Nusap1, Ube2c, Birc5, Tpx2, Top2a, Ndc80, Cks2, Nuf2, Cks1b, Mki67, Tmpo, Cenpf,
#' Tacc3, Pimreg, Smc4, Ccnb2, Ckap2l, Ckap2, Aurkb, Bub1, Kif11, Anp32e, Tubb4b, Gtse1, Kif20b,
#' Hjurp, Cdca3, Jpt1, Cdc20, Ttk, Cdc25c, Kif2c, Rangap1, Ncapd2, Dlgap5, Cdca2, Cdca8, Ect2, Kif23,
#' Hmmr, Aurka, Psrc1, Anln, Lbr, Ckap5, Cenpe, Ctcf, Nek2, G2e3, Gas2l3, Cbx5, Cenpa
#' @param species The species of the data, either "human" or "mouse". Default is "auto".
#' If "auto" is given, the function will try to guess the species based on the gene names in the data.
#' @param trans_args A list of additional arguments to be passed to `RunSeuratTransformation` before running CellCycleScoring.
#' @param log Logger
#' @return The Seurat object with the cell cycle scores and predicted phase added to the metadata.
#' Literally, columns "S.Score", "G2M.Score" and "Phase" will be added to the metadata.
#' @keywords internal
#' @importFrom Seurat CellCycleScoring
#' @importFrom SeuratObject DefaultAssay
#' @importFrom rlang %||%
RunSeuratCellCycleScoring <- function(
    object,
    s.features = NULL,
    g2m.features = NULL,
    species = "auto",
    trans_args = list(),
    log = NULL,
    cache = NULL
) {
    log <- log %||% get_logger()
    orig_assay <- DefaultAssay(object)

    trans_args$object <- object
    trans_args$from_ccs <- TRUE
    trans_args$cache <- cache
    object <- do_call(RunSeuratTransformation, trans_args)
    trans_args$object <- NULL
    gc()

    s.genes.mouse <- c(
        "Mcm5",
        "Pcna",
        "Tyms",
        "Fen1",
        "Mcm7",
        "Mcm4",
        "Rrm1",
        "Ung",
        "Gins2",
        "Mcm6",
        "Cdca7",
        "Dtl",
        "Prim1",
        "Uhrf1",
        "Cenpu",
        "Hells",
        "Rfc2",
        "Polr1b",
        "Nasp",
        "Rad51ap1",
        "Gmnn",
        "Wdr76",
        "Slbp",
        "Ccne2",
        "Ubr7",
        "Msh2",
        "Rad51",
        "Rrm2",
        "Cdc45",
        "Cdc6",
        "Exo1",
        "Tipin",
        "Dscc1",
        "Blm",
        "Casp8ap2",
        "Usp1",
        "Clspn",
        "Pola1",
        "Chaf1b",
        "Mrpl36",
        "E2f8"
    )
    g2m.genes.mouse <- c(
        "Hmgb2",
        "Cdk1",
        "Nusap1",
        "Ube2c",
        "Birc5",
        "Tpx2",
        "Top2a",
        "Ndc80",
        "Cks2",
        "Nuf2",
        "Cks1b",
        "Mki67",
        "Tmpo",
        "Cenpf",
        "Tacc3",
        "Pimreg",
        "Smc4",
        "Ccnb2",
        "Ckap2l",
        "Ckap2",
        "Aurkb",
        "Bub1",
        "Kif11",
        "Anp32e",
        "Tubb4b",
        "Gtse1",
        "Kif20b",
        "Hjurp",
        "Cdca3",
        "Jpt1",
        "Cdc20",
        "Ttk",
        "Cdc25c",
        "Kif2c",
        "Rangap1",
        "Ncapd2",
        "Dlgap5",
        "Cdca2",
        "Cdca8",
        "Ect2",
        "Kif23",
        "Hmmr",
        "Aurka",
        "Psrc1",
        "Anln",
        "Lbr",
        "Ckap5",
        "Cenpe",
        "Ctcf",
        "Nek2",
        "G2e3",
        "Gas2l3",
        "Cbx5",
        "Cenpa"
    )
    cc.genes <- Seurat::cc.genes
    if (species == "auto") {
        gene_names <- rownames(object)
        human_genes <- c(cc.genes$s.genes, cc.genes$g2m.genes)
        mouse_genes <- c(s.genes.mouse, g2m.genes.mouse)
        human_overlap <- sum(gene_names %in% human_genes)
        mouse_overlap <- sum(gene_names %in% mouse_genes)
        if (human_overlap >= mouse_overlap) {
            species <- "human"
        } else {
            species <- "mouse"
        }
    }
    species <- match.arg(species, choices = c("human", "mouse"))
    if (species == "human") {
        s.features <- s.features %||% cc.genes$s.genes
        g2m.features <- g2m.features %||% cc.genes$g2m.genes
    } else {
        s.features <- s.features %||% s.genes.mouse
        g2m.features <- g2m.features %||% g2m.genes.mouse
    }
    log$info(
        "  Running CellCycleScoring with {length(s.features)}/{length(g2m.features)} S/G2M phase genes (species = {species}) ..."
    )
    object <- CellCycleScoring(
        object,
        s.features = s.features,
        g2m.features = g2m.features,
        set.ident = FALSE
    )

    # Restore the default assay if it was changed by transformation
    # which needs to be later normalized
    # Illustration:
    #   RNA ---SCTransform---> SCT ---CellCycleScoring---> SCT (default assay changed to SCT)
    #   Here we change it back to RNA:
    #   SCT ---> RNA ---> Further normalize with the availability to regress out cell cycle scores if needed
    DefaultAssay(object) <- orig_assay

    # Clean up the object to save memory and speed up merging
    # Remove the assays other than original assay
    # If no other assays, remove layers: data and scale.data
    assays_to_remove <- setdiff(names(object@assays), orig_assay)
    if (length(assays_to_remove) > 0) {
        object@assays[assays_to_remove] <- NULL
    } else {
        if ("layers" %in% methods::slotNames(object[[orig_assay]])) {
            object[[orig_assay]]@layers$data <- NULL
            object[[orig_assay]]@layers$scale.data <- NULL
        } else {
            object[[orig_assay]]$data <- NULL
            object[[orig_assay]]$scale.data <- NULL
        }
    }
    gc()

    object <- AddSeuratCommand(
        object,
        "CellCycleScoring",
        paste0(
            "CellCycleScoring(object, s.features = s.features, g2m.features = g2m.features, set.ident = FALSE)"
        )
    )
    return(object)
}
