# Tests for the degenerate-layer guard in RunSeuratTransformation
# pbmc_small's counts are split into 3 layers; layer "b" is forced down to a
# single cell, which is the shape that makes SCTransform fail in row_gmean().

make_degenerate_obj <- function(n_b_cells = 1L) {
    obj <- Seurat::CreateSeuratObject(
        SeuratObject::LayerData(SeuratObject::pbmc_small, layer = "counts")
    )
    f <- rep(c("a", "b", "c"), length.out = ncol(obj))
    obj[["RNA"]] <- split(obj[["RNA"]], f = f)
    obj$Sample <- f
    b_cells <- colnames(SeuratObject::LayerData(obj, layer = "counts.b"))
    subset(
        obj,
        cells = c(setdiff(colnames(obj), b_cells), b_cells[seq_len(n_b_cells)])
    )
}

test_that("degenerate layer errors before SCTransform", {
    obj <- make_degenerate_obj()
    err <- tryCatch(
        RunSeuratTransformation(
            obj,
            use_sct = TRUE,
            on_degenerate = "error",
            cell_qc = "nFeature_RNA > 500 & percent.mt < 10",
            cache = FALSE
        ),
        error = function(e) conditionMessage(e)
    )
    expect_match(err, "min_cells_layer = 3")
    expect_match(err, "1 of 3 counts layers")
    expect_match(err, "- b: 1 cell")
    expect_match(err, 'cell_qc = "nFeature_RNA > 500 & percent.mt < 10"', fixed = TRUE)
    expect_match(err, 'on_degenerate = "drop"', fixed = TRUE)

    # default on_degenerate is "error"
    expect_error(
        RunSeuratTransformation(obj, use_sct = TRUE, cache = FALSE),
        "fewer than min_cells_layer = 3"
    )
})

test_that("on_degenerate = drop removes the layer and records it", {
    obj <- make_degenerate_obj()
    res <- RunSeuratTransformation(
        obj,
        use_sct = TRUE,
        on_degenerate = "drop",
        cache = FALSE
    )
    layers <- SeuratObject::Layers(res[["RNA"]])
    expect_false("counts.b" %in% layers)
    expect_setequal(layers, c("counts.a", "counts.c"))
    expect_equal(ncol(res), 53)
    deg <- res@misc$degenerate_layers
    expect_s3_class(deg, "data.frame")
    expect_equal(nrow(deg), 1L)
    expect_equal(deg$layer, "counts.b")
    expect_equal(deg$sample, "b")
    expect_equal(deg$cells, 1L)
})

test_that("healthy single-layer object is unchanged", {
    res <- RunSeuratTransformation(
        SeuratObject::pbmc_small,
        use_sct = FALSE,
        cache = FALSE
    )
    expect_equal(ncol(res), 80)
    expect_equal(nrow(res), 230)
    expect_true("pca" %in% Seurat::Reductions(res))
    expect_null(res@misc$degenerate_layers)
})

test_that("guard is not SCT-only and can be disabled", {
    # A 1-cell layer makes Seurat's own FindVariableFeatures fail in loess
    # ("invalid 'x'"), even with the guard disabled, so use a 2-cell layer:
    # still degenerate (< min_cells_layer), but the log-normalization route
    # can complete.
    obj <- make_degenerate_obj(n_b_cells = 2L)
    expect_error(
        RunSeuratTransformation(
            obj,
            use_sct = FALSE,
            min_cells_layer = 3,
            cache = FALSE
        ),
        "fewer than min_cells_layer = 3"
    )
    # min_cells_layer = 0 disables the check
    res <- RunSeuratTransformation(
        obj,
        use_sct = FALSE,
        min_cells_layer = 0,
        cache = FALSE
    )
    expect_equal(ncol(res), ncol(obj))
    expect_null(res@misc$degenerate_layers)
})

test_that("on_degenerate is validated", {
    expect_error(
        RunSeuratTransformation(
            SeuratObject::pbmc_small,
            on_degenerate = "nope",
            cache = FALSE
        ),
        "should be one of"
    )
})

test_that("all layers degenerate errors even with on_degenerate = drop", {
    obj <- make_degenerate_obj()
    # keep only the single-cell layer
    obj <- subset(obj, cells = colnames(SeuratObject::LayerData(obj, layer = "counts.b")))
    expect_error(
        RunSeuratTransformation(obj, use_sct = FALSE, on_degenerate = "drop"),
        "all 1 counts layers have fewer than min_cells_layer = 3 cells",
        fixed = TRUE
    )
})
