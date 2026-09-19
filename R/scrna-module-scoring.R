# #####################################################################
# RunModuleScoring
# #####################################################################
#' Run module scoring for expression programs on a Seurat object
#'
#' This function wraps the module scoring logic of the `ModuleScoreCalculator`
#' process in biopipen, and supports multiple scoring tools benchmarked in the
#' scPS paper (see references): Seurat's [Seurat::AddModuleScore()] /
#' [Seurat::CellCycleScoring()], UCell, AUCell, ssGSEA, JASMINE, SCSE and scPS.
#'
#' The features of a module can be given as:
#'
#' - A comma-separated string (e.g. `"HAVCR2,ENTPD1,LAYN,LAG3"`), treated as a
#'   single expression program.
#' - A character vector, also a single expression program.
#' - A list of character vectors, i.e. multiple programs. When the list is
#'   fully named, the names are used as the metadata column names of the scores
#'   (each program gets its own column, prefixed by the module key), e.g.
#'   `list(Exhaustion = c("HAVCR2", "ENTPD1"), Activation = c("IFNG", "GZMB"))`
#'   will add `{module key}_Exhaustion` and `{module key}_Activation` columns to
#'   the metadata. If the list is not named, the columns will be named
#'   `{module key}1`, `{module key}2`, etc. A single unnamed program will be
#'   named by the module key. Use one of the reserved keys `_`, `-`, `*` or `#`
#'   as the module key to skip the prefix.
#'
#' For cell cycle scoring, use `"cc.genes"` or `"cc.genes.updated.2019"`
#' (human) or `"cc.genes.mouse"` as the features, or set `kind` to `"cc"`
#' (with `features` defaulting to `"cc.genes"`). Then `{module key}_S.Score`,
#' `{module key}_G2M.Score` and `{module key}_Phase` columns will be added to
#' the metadata (without the prefix when the module key is one of the reserved
#' no-prefix keys `_`, `-`, `*` or `#`).
#'
#' A module can also perform diffusion map (diffusion components) as a
#' reduction by setting `kind` to `"dm"`, `"diffmap"` or `"diffusion_map"`.
#' In this case, `features` is the number of components to keep (default 2),
#' the first `features` components will be added to the metadata as
#' `{module key}_1`, `{module key}_2`, ..., and a reduction named
#' `{module key}` will be created. The diffusion map is calculated by
#' `destiny::DiffusionMap()` (see
#' <https://bioconductor.org/packages/release/bioc/html/destiny.html>),
#' neither Seurat nor UCell is used. Other key-value pairs of the module will
#' be passed to `destiny::DiffusionMap()`.
#'
#' The parameters (`method`, `nbin`, `ctrl`, `k`, `assay`, `seed`, `search`
#' and those in `...`) are the defaults inherited by all modules. They can be
#' overridden per module by putting the same keys in the module dict.
#' Irrelevant parameters for a tool are ignored, e.g. `nbin` and `ctrl` are
#' only used by `Seurat::AddModuleScore()`, and `maxRank` is only used by UCell.
#'
#' Note that the scores from different methods are NOT comparable with each
#' other (only the column names are consistent). Also, UCell imputes missing
#' genes with 0 while other methods drop them (with a warning from Seurat).
#' The `slot` parameter of UCell is `slot` (not `layer`), while other
#' matrix-based tools (AUCell, ssGSEA, JASMINE, SCSE) read the expression data
#' from the `data` layer (or the layer specified by `layer`).
#'
#' @param object Seurat object
#' @param modules A named list of modules. The keys are the names of the
#' modules and the values are lists of parameters for the modules,
#' inherited from the function arguments (see details). At least `features`
#' should be provided for each module.
#' @param method The default method to calculate the module scores.
#' - seurat: `Seurat::AddModuleScore()`
#' - ucell: `UCell::AddModuleScore_UCell()`
#' - aucell: `AUCell::AUCell_buildRankings()` and `AUCell::AUCell_calcAUC()`
#' - ssgsea: `GSVA::gsva()` with `method = "ssgsea"`
#' - jasmine: JASMINE scoring (see references)
#' - scse: SCSE scoring (see references)
#' - scps: scPS scoring (see references), using the bundled scPS
#'   implementation ported from the scPS script
#'   (<https://github.com/Thakar-Lab/scPS>).
#' @param nbin,ctrl,k,assay,seed,search The default parameters for
#' `Seurat::AddModuleScore()`. See [Seurat::AddModuleScore()] for details.
#' @param ... Additional default parameters for the tools, e.g. `maxRank`,
#' `w_neg` and `slot` for UCell, `aucMaxRank` and `plotStats` for AUCell,
#' `kcdf` and `verbose` for ssGSEA, `layer` for the matrix-based tools.
#' These can also be set per module in `modules`.
#' @param log Logger
#' @return The Seurat object with the module scores added to the metadata.
#' @references
#' Tirosh I, et al. 2016. Dissecting the multicellular ecosystem of metastatic melanoma by single-cell RNA-seq. *Science* 352(6282):189-196. doi:10.1126/science.aad0501. <https://www.science.org/doi/10.1126/science.aad0501> — the `seurat` method.
#'
#' Andreatta M, Carmona SJ. 2021. UCell: Robust and scalable single-cell gene signature scoring. *Comput Struct Biotechnol J* 19:3796-3798. doi:10.1016/j.csbj.2021.06.043. <https://doi.org/10.1016/j.csbj.2021.06.043> — the `ucell` method.
#'
#' Aibar S, et al. 2017. SCENIC: single-cell regulatory network inference and clustering. *Nat Methods* 14:1083-1086. doi:10.1038/nmeth.4463. <https://doi.org/10.1038/nmeth.4463> — the `aucell` method.
#'
#' Barbie DA, et al. 2009. Systematic RNA interference reveals that oncogenic KRAS-driven cancers require TBK1. *Nature* 462:108-112. doi:10.1038/nature08460. <https://doi.org/10.1038/nature08460> — the `ssgsea` method.
#'
#' Noureen N, et al. 2022. Integrated analysis of telomerase enzymatic activity unravels an association with cancer stemness and proliferation. *eLife* 11:e71994. doi:10.7554/eLife.71994. <https://doi.org/10.7554/eLife.71994> — the `jasmine` method.
#'
#' Pont F, et al. 2019. Single-cell signature explorer for personalized transcriptomics studies and drug discovery. *Nucleic Acids Res* 47(19):e90. doi:10.1093/nar/gkz601. <https://doi.org/10.1093/nar/gkz601> — the `scse` method.
#'
#' The scPS paper (benchmarking the above methods, the `scps` method): <https://academic.oup.com/nargab/article/6/3/lqae124/7770961>
#' @importFrom Seurat AddMetaData AddModuleScore CellCycleScoring CreateDimReducObject Embeddings
#' @importFrom SeuratObject GetAssayData
#' @importFrom rlang %||%
#' @export
#' @examples
#' \donttest{
#' data(pbmc_small, package = "SeuratObject")
#' obj <- Seurat::NormalizeData(pbmc_small)
#' # a comma-separated string of features: one score column per module key
#' obj <- RunModuleScoring(
#'     obj,
#'     modules = list(
#'         Exhaustion = list(features = "MS4A1,CD79A"),
#'         Prolif = list(features = "GZMB,GNLY")
#'     ),
#'     # small object needs small `nbin`/`ctrl` (see the hint on error)
#'     nbin = 10, ctrl = 5
#' )
#' head(obj@meta.data[, c("Exhaustion", "Prolif")])
#'
#' # cell cycle scores with UCell (needs the UCell package)
#' if (requireNamespace("UCell", quietly = TRUE)) {
#'     obj <- RunModuleScoring(
#'         obj,
#'         modules = list(CellCycle = list(features = "cc.genes.updated.2019")),
#'         method = "ucell", nbin = 10, ctrl = 5
#'     )
#'     head(obj@meta.data[, c("CellCycle_S.Score", "CellCycle_G2M.Score", "CellCycle_Phase")])
#' }
#' }
RunModuleScoring <- function(
    object,
    modules,
    method = "seurat",
    nbin = 24,
    ctrl = 100,
    k = FALSE,
    assay = NULL,
    seed = 8525,
    search = FALSE,
    ...,
    log = NULL
) {
    log <- log %||% get_logger()
    if (is.null(modules) || length(modules) == 0 || is.null(names(modules))) {
        stop("No modules provided. Please provide a named list of modules.")
    }

    methods <- c("seurat", "ucell", "aucell", "ssgsea", "jasmine", "scse", "scps")
    defaults <- list(
        method = method,
        nbin = nbin,
        ctrl = ctrl,
        k = k,
        assay = assay,
        seed = seed,
        search = search
    )
    defaults <- list_update(defaults, list(...))

    s.genes.mouse <- c(
        "Mcm5", "Pcna", "Tyms", "Fen1", "Mcm7", "Mcm4", "Rrm1", "Ung", "Gins2", "Mcm6",
        "Cdca7", "Dtl", "Prim1", "Uhrf1", "Cenpu", "Hells", "Rfc2", "Polr1b", "Nasp",
        "Rad51ap1", "Gmnn", "Wdr76", "Slbp", "Ccne2", "Ubr7", "Msh2", "Rad51", "Rrm2",
        "Cdc45", "Cdc6", "Exo1", "Tipin", "Dscc1", "Blm", "Casp8ap2", "Usp1", "Clspn",
        "Pola1", "Chaf1b", "Mrpl36", "E2f8"
    )
    g2m.genes.mouse <- c(
        "Hmgb2", "Cdk1", "Nusap1", "Ube2c", "Birc5", "Tpx2", "Top2a", "Ndc80", "Cks2",
        "Nuf2", "Cks1b", "Mki67", "Tmpo", "Cenpf", "Tacc3", "Pimreg", "Smc4", "Ccnb2",
        "Ckap2l", "Ckap2", "Aurkb", "Bub1", "Kif11", "Anp32e", "Tubb4b", "Gtse1",
        "Kif20b", "Hjurp", "Cdca3", "Jpt1", "Cdc20", "Ttk", "Cdc25c", "Kif2c", "Rangap1",
        "Ncapd2", "Dlgap5", "Cdca2", "Cdca8", "Ect2", "Kif23", "Hmmr", "Aurka", "Psrc1",
        "Anln", "Lbr", "Ckap5", "Cenpe", "Ctcf", "Nek2", "G2e3", "Gas2l3", "Cbx5", "Cenpa"
    )

    for (key in names(modules)) {
        module <- list_update(defaults, modules[[key]])

        # `kind: "cc"` also indicates cell cycle scoring; the gene set is
        # chosen by `features`, defaulting to `cc.genes`
        if (identical(module$kind, "cc") && is.null(module$features)) {
            module$features <- "cc.genes"
        }

        if (is.null(module$features) || length(module$features) == 0) {
            # diffusion map modules default `features` to the number of
            # components (2) and are validated in their own branch
            is_dm <- !is.null(module$kind) &&
                module$kind %in% c("dm", "diffmap", "diffusion_map")
            if (!is_dm) {
                stop(paste0("Module '", key, "' has no features"))
            }
        }

        mthd <- module$method %||% "seurat"
        mthd <- match.arg(mthd, choices = methods)
        module$method <- NULL

        log$info("Calculating module '{key}' with method '{mthd}' ...")

        # Column name prefix: the module key, unless it is one of the
        # reserved no-prefix keys (`_`, `-`, `*`, `#`)
        prefix <- if (key %in% c("_", "-", "*", "#")) "" else paste0(key, "_")

        # Diffusion map (diffusion components) branch
        # Note: this uses neither Seurat nor UCell, but destiny
        if (!is.null(module$kind) && module$kind %in% c("dm", "diffmap", "diffusion_map")) {
            require_package("destiny")
            features <- module$features
            if (is.null(features)) {
                features <- 2
            }
            features <- as.integer(features)
            if (is.na(features) || features < 1) {
                stop(paste0(
                    "Module '", key, "' (kind '", module$kind, "'): `features` must be ",
                    "a positive integer (the number of diffusion components to keep)"
                ))
            }
            if (is.null(module$verbose)) {
                module$verbose <- TRUE
            }
            module$features <- NULL
            module$kind <- NULL

            if (!is.null(module$n_pcs)) {
                log$info("- Using cell embeddings from PCA reduction ...")
                module$data <- Embeddings(object, reduction = "pca")
                if (module$n_pcs > ncol(module$data)) {
                    log$warn("- `n_pcs` ({module$n_pcs}) is larger than the number of PCs, using all {ncol(module$data)} PCs ...")
                }
                module$data <- module$data[, 1:min(module$n_pcs, ncol(module$data))]
                module$n_pcs <- NULL
            } else {
                log$info("- Using assay data ...")
                # destiny does not accept sparse matrices, and treats rows
                # as observations, so transpose the genes x cells matrix
                module$data <- t(as.matrix(GetAssayData(object, layer = "data")))
            }

            # destiny removes duplicated rows (e.g. duplicated cells, or
            # cells with identical profiles) before computing the map, so
            # the eigenvectors would have fewer rows than the object;
            # remember the mapping to restore all cells
            dupes <- duplicated(module$data)
            if (any(dupes)) {
                data_orig <- module$data
                module$data <- module$data[!dupes, , drop = FALSE]
                log$warn("- Dropping {sum(dupes)} duplicated cell(s) for the diffusion map ...")
            }

            log$info("- Calculating diffusion map ...")
            # `k` is both a Seurat default (FALSE) and a destiny parameter,
            # so drop it when it carries the Seurat default
            # (use `[[` — `$` would partial-match `kind` once `k` is deleted)
            if (identical(module[["k"]], FALSE)) {
                module[["k"]] <- NULL
            }
            if (is.null(module[["k"]]) && any(dupes) && nrow(module$data) <= 1000L) {
                # destiny's auto `k = n - 1` (small data) exceeds the number
                # of rows once duplicated cells are removed; keep full knn
                # on the remaining cells
                module[["k"]] <- nrow(module$data) - 1L
            }
            dm <- do_call(destiny::DiffusionMap, list_update(
                list(data = module$data),
                .filter_module_args(
                    module,
                    destiny::DiffusionMap,
                    whitelist = c(
                        "sigma", "k", "n_eigs", "density_norm", "distance",
                        "n_pcs", "n_local", "rotate", "censor_val",
                        "censor_range", "missing_range", "vars", "knn_params",
                        "verbose", "suppress_dpt"
                    )
                )
            ))
            ev <- destiny::eigenvectors(dm)
            if (any(dupes)) {
                # duplicated cells (possibly with different names) share the
                # coordinates of their first occurrence; match by value
                # since destiny drops rows by value, not by name, and
                # restore each cell's own name
                m <- match(
                    apply(data_orig, 1, paste, collapse = "\r"),
                    apply(module$data, 1, paste, collapse = "\r")
                )
                ev <- ev[m, , drop = FALSE]
                rownames(ev) <- rownames(data_orig)
            }

            log$info("- Creating DimReduc object ...")
            object[[key]] <- CreateDimReducObject(
                embeddings = data.matrix(as.data.frame(ev[, 1:features])),
                key = paste0(key, "_")
            )

            # add to meta.data
            log$info("- Adding to meta.data ...")
            object <- AddMetaData(
                object,
                object[[key]]@cell.embeddings,
                col.name = colnames(object[[key]]@cell.embeddings)
            )

            next
        }

        # Cell cycle scoring
        is_cc <- FALSE
        if (identical(module$features, "cc.genes")) {
            is_cc <- TRUE
            s.genes <- Seurat::cc.genes$s.genes
            g2m.genes <- Seurat::cc.genes$g2m.genes
        } else if (identical(module$features, "cc.genes.updated.2019")) {
            is_cc <- TRUE
            s.genes <- Seurat::cc.genes.updated.2019$s.genes
            g2m.genes <- Seurat::cc.genes.updated.2019$g2m.genes
        } else if (identical(module$features, "cc.genes.mouse")) {
            is_cc <- TRUE
            s.genes <- s.genes.mouse
            g2m.genes <- g2m.genes.mouse
        }

        if (is_cc) {
            module$features <- NULL
            # score columns are prefixed by the module key unless it is empty
            cc_cols <- paste0(prefix, c("S.Score", "G2M.Score"))
            # the tools write fixed column names (S.Score/G2M.Score/Phase)
            # and overwrite existing ones in place, so save any existing
            # ones and restore them after this call's columns are renamed
            # (a previous cc module may have added columns with the same
            # fixed names)
            saved <- object@meta.data[, intersect(
                colnames(object@meta.data),
                c("S.Score", "G2M.Score", "Phase")
            ), drop = FALSE]
            if (mthd == "seurat") {
                args <- list(object = object, s.features = s.genes, g2m.features = g2m.genes)
                args <- list_update(args, .filter_module_args(module, Seurat::CellCycleScoring))
                object <- do_call(Seurat::CellCycleScoring, args)
                # the tool's fixed-name columns are this module's output
                idx <- match(c("S.Score", "G2M.Score", "Phase"), colnames(object@meta.data))
                colnames(object@meta.data)[idx] <- c(cc_cols, paste0(prefix, "Phase"))
            } else if (mthd == "ucell") {
                require_package("UCell")
                args <- list(
                    obj = object,
                    features = list("S.Score" = s.genes, "G2M.Score" = g2m.genes),
                    name = ""
                )
                args <- list_update(args, .filter_module_args(module, UCell::AddModuleScore_UCell))
                object <- do_call(UCell::AddModuleScore_UCell, args)
                # the tool's fixed-name columns are this module's output
                idx <- match(c("S.Score", "G2M.Score"), colnames(object@meta.data))
                colnames(object@meta.data)[idx] <- cc_cols
            } else {
                scores <- .RunModuleScoringMatrix(
                    object,
                    list("S.Score" = s.genes, "G2M.Score" = g2m.genes),
                    mthd,
                    module,
                    log
                )
                colnames(scores) <- cc_cols
                object <- AddMetaData(object, scores)
            }
            # restore any previous module's fixed-name columns (they were
            # overwritten in place by the tools; skip names that collide
            # with this module's output)
            saved <- saved[, setdiff(colnames(saved), c(cc_cols, paste0(prefix, "Phase"))), drop = FALSE]
            if (ncol(saved) > 0) object <- AddMetaData(object, saved)
            object[[paste0(prefix, "Phase")]] <- ifelse(
                object[[cc_cols[1]]] - object[[cc_cols[2]]] > 0,
                "S",
                ifelse(object[[cc_cols[1]]] - object[[cc_cols[2]]] < 0, "G2M", "G1")
            )
            next
        }

        # Normalize the features to a list of expression programs
        features <- module$features
        if (is.character(features)) {
            if (length(features) == 1) {
                features <- trimws(strsplit(features, ",")[[1]])
            }
            if (length(features) == 0) {
                stop(paste0("Module '", key, "' has no features"))
            }
            features <- list(features)
        } else if (!is.list(features)) {
            stop(paste0(
                "Module '", key, "': features must be a character vector, ",
                "a comma-separated string, or a list of character vectors"
            ))
        }

        # Resolve the column names of the scores
        fnames <- names(features)
        if (!is.null(fnames) && all(nzchar(fnames))) {
            col_names <- make.unique(paste0(prefix, fnames))
        } else if (length(features) == 1) {
            col_names <- key
        } else {
            col_names <- paste0(key, seq_along(features))
        }

        if (mthd == "seurat") {
            args <- list(object = object, features = features, name = "")
            args <- list_update(args, .filter_module_args(module, Seurat::AddModuleScore))
            tryCatch({
                object <- do_call(Seurat::AddModuleScore, args)
            }, error = function(e) {
                if (grepl("cannot take a sample larger than", e$message)) {
                    stop(paste0(
                        "Module '", key, "': ",
                        e$message,
                        " (try a smaller `ctrl`?)"
                    ))
                } else {
                    stop(e)
                }
            })
            # rename the appended score columns to the resolved names
            # (AddModuleScore always names the columns `{name}1..N` and
            # ignores the names of the features list)
            nc <- ncol(object@meta.data)
            n <- length(features)
            colnames(object@meta.data)[(nc - n + 1):nc] <- col_names
        } else if (mthd == "ucell") {
            require_package("UCell")
            args <- list(obj = object, features = features, name = "")
            args <- list_update(args, .filter_module_args(module, UCell::AddModuleScore_UCell))
            object <- do_call(UCell::AddModuleScore_UCell, args)
            # rename the appended score columns to the resolved names
            nc <- ncol(object@meta.data)
            n <- length(features)
            colnames(object@meta.data)[(nc - n + 1):nc] <- col_names
        } else {
            scores <- .RunModuleScoringMatrix(object, features, mthd, module, log)
            colnames(scores) <- col_names
            object <- AddMetaData(object, scores)
        }
    }

    object <- AddSeuratCommand(
        object,
        "RunModuleScoring",
        "RunModuleScoring(object, modules, ...)",
        params = list(
            modules = modules,
            method = method,
            nbin = nbin,
            ctrl = ctrl,
            k = k,
            assay = assay,
            seed = seed,
            search = search
        )
    )
    return(object)
}
# Filter the arguments of a module for a tool call:
# - Our own keys are never passed to the tools
# - If a whitelist is given, only the whitelisted keys are kept
# - If the function has `...`, all the rest are passed through (like the
#   original script does for Seurat functions)
# - Otherwise, only the formal arguments of the function are kept
.filter_module_args <- function(args, fn, whitelist = NULL) {
    args <- args[!names(args) %in% c(
        "features", "method", "kind", "name", "n_pcs",
        "layer", "agg", "keep"
    )]
    if (!is.null(whitelist)) {
        return(args[names(args) %in% whitelist])
    }
    frm <- methods::formalArgs(fn)
    if ("..." %in% frm) {
        return(args)
    }
    args[names(args) %in% frm]
}
# Calculate the module scores for a module using a matrix-based tool,
# returning a cells x sets matrix. The set names are positional and will be
# renamed by the caller.
.RunModuleScoringMatrix <- function(object, features, method, module, log) {
    # some tools (GSVA >= 2.0) require named gene sets
    if (is.null(names(features))) {
        names(features) <- paste0("signature_", seq_along(features))
    }
    switch(
        method,
        aucell = {
            require_package("AUCell")
            expr <- GetAssayData(object, layer = module$layer %||% "data")
            args <- list(exprMat = expr)
            args <- list_update(args, .filter_module_args(
                module,
                AUCell::AUCell_buildRankings,
                whitelist = c("plotStats", "featureType", "splitByFeatures")
            ))
            rankings <- do_call(AUCell::AUCell_buildRankings, args)

            # AUCell requires named gene sets
            if (is.null(names(features)) || !all(nzchar(names(features)))) {
                names(features) <- paste0("signature_", seq_along(features))
            }
            args <- list(geneSets = features, rankings = rankings)
            args <- list_update(args, .filter_module_args(
                module,
                AUCell::AUCell_calcAUC,
                whitelist = c("aucMaxRank", "tiesMethod")
            ))
            auc <- do_call(AUCell::AUCell_calcAUC, args)

            scores <- t(AUCell::getAUC(auc))
            rownames(scores) <- colnames(expr)
            scores
        },
        ssgsea = {
            require_package("GSVA")
            expr <- GetAssayData(object, layer = module$layer %||% "data")
            ssgsea_args <- .filter_module_args(
                module,
                GSVA::gsva,
                whitelist = c(
                    "kcdf", "abs.ranking", "min.sz", "max.sz",
                    "parallel.sz", "mx.diff", "tau", "ssgsea.norm",
                    "verbose", "BPPARAM"
                )
            )
            if (exists("ssgseaParam", envir = asNamespace("GSVA"))) {
                # GSVA >= 2.0: method-specific parameter object, with the
                # pre-2.0 parameter names mapped to the new ones
                param <- list(exprData = expr, geneSets = features)
                if (!is.null(ssgsea_args$min.sz)) {
                    param$minSize <- ssgsea_args$min.sz
                }
                if (!is.null(ssgsea_args$max.sz)) {
                    param$maxSize <- ssgsea_args$max.sz
                }
                if (!is.null(ssgsea_args$tau)) {
                    param$alpha <- ssgsea_args$tau
                }
                if (!is.null(ssgsea_args$ssgsea.norm)) {
                    param$normalize <- ssgsea_args$ssgsea.norm
                }
                gsva_args <- ssgsea_args[c("verbose", "BPPARAM")]
                gsva_args <- gsva_args[!vapply(gsva_args, is.null, logical(1))]
                scores <- do_call(
                    GSVA::gsva,
                    c(list(param = do_call(GSVA::ssgseaParam, param)), gsva_args)
                )
            } else {
                # GSVA < 2.0: the classic signature
                args <- list(expr = expr, gset.idx.list = features, method = "ssgsea")
                args <- list_update(args, ssgsea_args)
                scores <- do_call(GSVA::gsva, args)
            }
            scores <- t(scores)
            rownames(scores) <- colnames(expr)
            scores
        },
        jasmine = {
            expr <- GetAssayData(object, layer = module$layer %||% "data")
            expr <- as.matrix(expr)
            n_genes <- nrow(expr)
            cells <- colnames(expr)
            # JASMINE scoring: per cell, combine
            # (1) the enrichment odds ratio of the signature genes among the
            #     expressed genes, and
            # (2) the mean rank of the signature genes among the genes
            # both scaled to [0, 1] and averaged (Noureen et al. 2022)
            # ponytail: ranks are taken among all genes (not just the expressed
            # ones) for simplicity; upgrade to expressed-only ranks if needed
            scores <- vapply(features, function(sig) {
                sig <- intersect(sig, rownames(expr))
                if (length(sig) == 0) {
                    log$warn("- No genes of the signature are found in the object.")
                    return(rep(NA_real_, length(cells)))
                }
                expr_sig <- expr[sig, , drop = FALSE]
                n_sig_expr <- colSums(expr_sig > 0)
                n_expr <- colSums(expr > 0)
                # enrichment odds ratio with pseudo-counts
                a <- n_sig_expr + 0.5
                b <- n_expr - n_sig_expr + 0.5
                c <- length(sig) - n_sig_expr + 0.5
                d <- (n_genes - n_expr) - (length(sig) - n_sig_expr) + 0.5
                or_score <- 1 / (1 + exp(-log2((a * d) / (b * c))))
                # mean rank of the signature genes (rank 1 = highest expression)
                ranks <- apply(expr_sig, 2, function(x) rank(-x, ties.method = "average"))
                rank_score <- 1 - (colMeans(ranks) - 1) / n_genes
                (or_score + rank_score) / 2
            }, numeric(length(cells)))
            # vapply already returns cells x signatures
            rownames(scores) <- cells
            scores
        },
        scse = {
            expr <- GetAssayData(object, layer = module$layer %||% "data")
            # Matrix::colSums handles both sparse and dense matrices
            total <- Matrix::colSums(expr)
            # SCSE scoring: normalized total expression of the signature genes
            # (Pont et al. 2019)
            scores <- vapply(features, function(sig) {
                sig <- intersect(sig, rownames(expr))
                if (length(sig) == 0) {
                    log$warn("- No genes of the signature are found in the object.")
                    return(rep(NA_real_, ncol(expr)))
                }
                Matrix::colSums(expr[sig, , drop = FALSE]) / total
            }, numeric(ncol(expr)))
            # vapply already returns cells x signatures
            rownames(scores) <- colnames(expr)
            scores
        },
        scps = {
            require_package("GSEABase")
            # write the features as a GMT file
            gmt_file <- tempfile(fileext = ".gmt")
            gmt_lines <- vapply(seq_along(features), function(i) {
                nm <- names(features)[i]
                if (is.null(nm) || !nzchar(nm)) {
                    nm <- paste0("signature_", i)
                }
                paste(c(nm, "na", features[[i]]), collapse = "\t")
            }, character(1))
            writeLines(gmt_lines, gmt_file)
            gene_sets <- GSEABase::getGmt(gmt_file)
            scores <- scPS(Seurat_data = object, GeneSet = gene_sets)
            scores <- t(scores)
            rownames(scores) <- colnames(object)
            scores
        }
    )
}
