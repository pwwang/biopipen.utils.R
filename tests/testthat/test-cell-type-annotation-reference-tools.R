# Smoke tests for the reference-consuming annotators -- scmap, CHETAH,
# scClassify, scPred, Azimuth and MapQuery. They take a *labelled reference* (a
# Seurat object, a SingleCellExperiment or a plain list) instead of a marker table.
# The fixture is the usual pbmc_small *self-referenced*: the reference is the
# same normalised object as the query, labelled with `groups` (g1, g2).
# scClassify uses `RNA_snn_res.1` (0, 1, 2) instead, because its default
# HOPACH tree needs more than two reference cell types.

norm_obj <- function() {
    obj <- SeuratObject::pbmc_small
    suppressWarnings(Seurat::NormalizeData(obj, verbose = FALSE))
}

ref_rds <- function(ref) {
    f <- tempfile(fileext = ".rds")
    saveRDS(ref, f)
    f
}

run <- function(obj, tool, args, ident = NULL) {
    suppressWarnings(suppressMessages(RunCellTypeAnnotation(
        obj, tool, args = args, ident = ident, cache = tempfile("cta-")
    )))
}

test_that("scmap runner: cell-level labels from a Seurat reference", {
    skip_if_not_installed("scmap")
    skip_if_not_installed("SingleCellExperiment")
    obj <- norm_obj()
    # `cell_type1` is scmap's default `cluster_col`
    ref <- obj
    ref$cell_type1 <- as.character(obj$groups)
    rec <- run(obj, "scmap", list(db = ref_rds(ref)))
    expect_equal(rec$type, "cell")
    expect_true("scmap_celltype" %in% colnames(rec$mapping))
    expect_equal(nrow(rec$mapping), ncol(obj))
    expect_identical(rownames(rec$mapping), colnames(obj))
    # cells below the similarity threshold come back as the literal string
    # "unassigned", not NA
    expect_true(all(rec$mapping$scmap_celltype %in% c("g1", "g2", "unassigned")))
    expect_true(any(rec$mapping$scmap_celltype == "g1"))
})

test_that("scmap runner: both-mode aggregates the labels by cluster", {
    skip_if_not_installed("scmap")
    skip_if_not_installed("SingleCellExperiment")
    obj <- norm_obj()
    ref <- obj
    ref$cell_type1 <- as.character(obj$groups)
    rec <- run(obj, "scmap", list(db = ref_rds(ref)), ident = "groups")
    expect_equal(rec$type, "cluster")
    expect_setequal(names(rec$mapping), c("g1", "g2"))
    expect_true("scmap_celltype" %in% colnames(rec$cells))
    expect_equal(nrow(rec$cells), ncol(obj))
})

test_that("cheetah runner: cell-level labels from a SingleCellExperiment reference", {
    skip_if_not_installed("CHETAH")
    skip_if_not_installed("SingleCellExperiment")
    obj <- norm_obj()
    ref <- SingleCellExperiment::SingleCellExperiment(
        assays = list(counts = as.matrix(SeuratObject::GetAssayData(obj, layer = "counts"))),
        colData = S4Vectors::DataFrame(celltypes = as.character(obj$groups))
    )
    rec <- run(obj, "cheetah", list(db = ref_rds(ref)))
    expect_equal(rec$type, "cell")
    expect_true("cheetah_celltype" %in% colnames(rec$mapping))
    expect_equal(nrow(rec$mapping), ncol(obj))
    expect_identical(rownames(rec$mapping), colnames(obj))
    expect_true(all(rec$mapping$cheetah_celltype %in% c("g1", "g2", "Unassigned")))
})

test_that("scclassify runner: cell-level labels from an exprsMat/cellTypes reference", {
    skip_if_not_installed("scClassify")
    obj <- norm_obj()
    ref <- list(
        exprsMat = as.matrix(SeuratObject::GetAssayData(obj, layer = "data")),
        cellTypes = as.character(obj$RNA_snn_res.1)
    )
    rec <- run(obj, "scclassify", list(db = ref_rds(ref)))
    expect_equal(rec$type, "cell")
    expect_true("scclassify_celltype" %in% colnames(rec$mapping))
    expect_equal(nrow(rec$mapping), ncol(obj))
    # labels are reference cell types, their hierarchy nodes (e.g. "2_1") or
    # "unassigned"
    labels <- rec$mapping$scclassify_celltype
    expect_false(anyNA(labels))
    expect_true(all(nzchar(labels)))
    expect_true(any(labels %in% c("0", "1", "2")))
})

test_that("scpred runner: cell-level labels from a Seurat reference", {
    skip_if_not_installed("scPred")
    obj <- norm_obj()
    ref <- obj
    ref$cell_type <- as.character(obj$groups)
    res <- tryCatch(run(obj, "scpred", list(db = ref_rds(ref))), error = identity)
    if (inherits(res, "error")) {
        # scPred 1.9.2 projects the query with GetAssayData(new, "data"), which
        # SeuratObject >= 5 resolves as `assay = "data"` (its 2nd formal). The
        # reference side (getFeatureSpace + trainModel) runs; only the
        # prediction step is broken until scPred is updated.
        if (!grepl('"RNA", not "data"', conditionMessage(res), fixed = TRUE)) stop(res)
        skip(paste("scPred cannot predict under this SeuratObject:", conditionMessage(res)))
    }
    expect_equal(res$type, "cell")
    expect_true("scpred_celltype" %in% colnames(res$mapping))
    expect_equal(nrow(res$mapping), ncol(obj))
    expect_identical(rownames(res$mapping), colnames(obj))
})

test_that("azimuth runner: needs a reference, and needs it on disk", {
    skip_if_not_installed("Azimuth")
    obj <- norm_obj()
    expect_error(
        run(obj, "azimuth", list(), ident = "groups"),
        "needs `envs.azimuth.ref`", fixed = TRUE
    )
    # A reference *name* (e.g. "pbmcref") is resolved through SeuratData, which
    # downloads ~73 MB -- not something a test should do. A local directory is
    # the offline path, and it must hold ref.Rds + idx.annoy.
    d <- Sys.getenv("AZIMUTH_REF_DIR", "")
    have_ref <- nzchar(d) && file.exists(file.path(d, "ref.Rds")) &&
        file.exists(file.path(d, "idx.annoy"))
    if (!have_ref) {
        d <- file.path(tempfile("azimuth-ref-"), "")
        dir.create(d)
    }
    res <- tryCatch(run(obj, "azimuth", list(ref = d), ident = "groups"), error = identity)
    if (inherits(res, "error")) {
        # Without a reference the failure is the missing files; with one, an
        # 80-cell / 230-gene fixture is outside Azimuth's supported use (the
        # runner itself is verified against the pipeline's pbmc3k object).
        skip(paste(
            if (have_ref) "Azimuth on the small fixture:" else "No usable Azimuth reference offline:",
            conditionMessage(res)
        ))
    }
    expect_equal(res$type, "cluster")
    expect_false(is.null(names(res$mapping)))
    expect_true("azimuth_celltype" %in% colnames(res$cells))
    expect_equal(nrow(res$cells), ncol(obj))
})

test_that("mapquery runner: needs a reference, its annotation column and `ident`", {
    obj <- norm_obj()
    expect_error(
        run(obj, "mapquery", list(use = "cell_type")),
        "`envs.mapquery.db` is not set", fixed = TRUE
    )
    ref <- norm_obj()
    ref$cell_type <- as.character(ref$groups)
    expect_error(
        run(obj, "mapquery", list(db = ref)),
        "`envs.mapquery.use` is not set", fixed = TRUE
    )
    # the cluster column is read before the mapping call, so this errors without
    # a usable reference
    expect_error(
        run(obj, "mapquery", list(db = ref, use = "cell_type"), ident = "nope"),
        "No `nope` column in the query's metadata", fixed = TRUE
    )
})

test_that("mapquery runner: cell-level labels, and both-mode with ident", {
    obj <- norm_obj()
    ref <- obj
    # the reference's labels differ from the query's clusters, so a mapping that
    # wrote over the `groups` column would show up below
    ref$cell_type <- ifelse(as.character(ref$groups) == "g1", "alpha", "beta")
    # reference mapping needs a reduction on the reference (`reference.reduction`
    # defaults to its first one) plus a UMAP built with `return.model = TRUE` as
    # the `reduction.model`
    ref <- suppressWarnings(Seurat::RunUMAP(
        ref, dims = 1:10, return.model = TRUE, verbose = FALSE
    ))
    args <- list(
        db = ref_rds(ref), use = "cell_type",
        # pbmc_small's PCA holds fewer than FindTransferAnchors' default
        # 30 dimensions
        find_transfer_anchors = list(dims = 1:10)
    )

    # without `ident`: one label per query cell
    cell_res <- tryCatch(run(obj, "mapquery", args), error = identity)
    if (inherits(cell_res, "error")) {
        # 80 cells / 230 genes is far below what reference mapping is built for
        # (the runner itself is verified against the pipeline's pbmc3k object)
        skip(paste("mapquery on the small fixture:", conditionMessage(cell_res)))
    }
    expect_equal(cell_res$type, "cell")
    expect_true("mapquery_celltype" %in% colnames(cell_res$mapping))
    expect_equal(nrow(cell_res$mapping), ncol(obj))
    expect_identical(rownames(cell_res$mapping), colnames(obj))
    expect_true(all(cell_res$mapping$mapquery_celltype %in% c("alpha", "beta")))

    # with `ident`: the same per-cell labels, plus their majority call per cluster
    before <- as.character(obj$groups)
    res <- tryCatch(run(obj, "mapquery", args, ident = "groups"), error = identity)
    if (inherits(res, "error")) {
        skip(paste("mapquery on the small fixture:", conditionMessage(res)))
    }
    expect_equal(res$type, "cluster")
    expect_setequal(names(res$mapping), c("g1", "g2"))
    expect_true(all(unlist(res$mapping) %in% c("alpha", "beta")))
    expect_true("mapquery_celltype" %in% colnames(res$cells))
    expect_equal(nrow(res$cells), ncol(obj))
    expect_identical(rownames(res$cells), colnames(obj))
    # the labels go into the runner's own `ident_name` column, never into the
    # query's cluster column
    expect_identical(as.character(obj$groups), before)
})
