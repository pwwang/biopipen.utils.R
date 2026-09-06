# Tests for EnsureSeuratScaleData
# Seurat >= 5.0.0 is in Imports, so all paths run without skips.
# Note: SCT multi-model integration is too heavy to build here; the
# per-model residual logic it shares with the single-model path is covered
# by the SCT tests below.

make_counts <- function(ng, nc, lambda, seed) {
    set.seed(seed)
    cnt <- matrix(rpois(ng * nc, lambda = lambda), ng, nc,
        dimnames = list(paste0("G", seq_len(ng)), paste0("C", seq_len(nc))))
    # sprinkle in some higher counts so SCT/ScaleData see real dispersion
    cnt[sample(seq_len(ng * nc), ng * 3)] <- 6
    as(cnt, "dgCMatrix")
}

# RNA object with log-normalized data, no scale.data layer
make_rna_obj <- function(ng = 60, nc = 40, seed = 1) {
    obj <- Seurat::CreateSeuratObject(make_counts(ng, nc, lambda = 0.8, seed = seed))
    Seurat::NormalizeData(obj, verbose = FALSE)
}

# scale.data of all features: the all-feature scaling reference
rna_ref <- function(obj) {
    ref <- suppressWarnings(Seurat::ScaleData(
        obj, features = rownames(obj), verbose = FALSE
    ))
    SeuratObject::GetAssayData(ref, assay = "RNA", layer = "scale.data")
}

# SCT object (single model over all cells); scale.data holds the variable features only
make_sct_obj <- function(ng = 400, nc = 80, seed = 7) {
    obj <- Seurat::CreateSeuratObject(make_counts(ng, nc, lambda = 0.5, seed = seed))
    Seurat::SCTransform(obj, variable.features.n = 150, verbose = FALSE)
}

sct_ref <- function(obj) {
    SeuratObject::GetAssayData(obj, assay = "SCT", layer = "scale.data")
}
sct_model <- function(obj) {
    levels(obj[["SCT"]])[1]
}
# marker genes the SCT model can compute residuals for, missing from scale.data
sct_markers <- function(obj) {
    mfeats <- rownames(Seurat::SCTResults(obj[["SCT"]], "feature.attributes", model = sct_model(obj)))
    setdiff(mfeats, rownames(sct_ref(obj)))
}
# what Seurat::GetResidual itself would add (oracle; needs the umi assay present)
sct_oracle_row <- function(obj, features) {
    res <- Seurat::GetResidual(
        obj, features = features, assay = "SCT",
        umi.assay = "RNA", na.rm = FALSE, verbose = FALSE
    )
    SeuratObject::GetAssayData(res, assay = "SCT", layer = "scale.data")[features, ]
}

test_that("RNA: added scale.data matches scaling data of all features", {
    obj <- make_rna_obj()
    ref <- rna_ref(obj)
    allfeat <- rownames(ref)

    # keep only the first 30 rows in scale.data, like a marker-only object
    obj2 <- SeuratObject::SetAssayData(obj, assay = "RNA", layer = "scale.data",
        new.data = ref[seq_len(30), , drop = FALSE])

    res <- EnsureSeuratScaleData(obj2, features = allfeat)
    got <- SeuratObject::GetAssayData(res, assay = "RNA", layer = "scale.data")
    expect_setequal(rownames(got), rownames(ref))
    # newly added rows identical to the all-feature scaling result
    # (log-normalized data => z-scores), existing rows untouched
    got <- got[match(rownames(ref), rownames(got)), ]
    expect_equal(as.matrix(got), as.matrix(ref), tolerance = 1e-6)
})

test_that("RNA: scale.data is created from scratch when absent", {
    obj <- make_rna_obj()  # never scaled: scale.data is empty (0 x 0)
    ref <- rna_ref(obj)
    feats <- rownames(ref)[c(2, 10, 33, 59)]

    res <- EnsureSeuratScaleData(obj, features = feats)
    got <- SeuratObject::GetAssayData(res, assay = "RNA", layer = "scale.data")
    expect_equal(rownames(got), feats)
    expect_equal(as.matrix(got), as.matrix(ref[feats, , drop = FALSE]), tolerance = 1e-6)
})

test_that("RNA: object is returned unchanged when nothing is missing", {
    obj <- make_rna_obj()
    ref <- rna_ref(obj)
    obj2 <- SeuratObject::SetAssayData(obj, assay = "RNA", layer = "scale.data",
        new.data = ref)
    res <- EnsureSeuratScaleData(obj2, features = rownames(ref)[1:5])
    expect_identical(res, obj2)
})

test_that("RNA: named list of feature groups is flattened", {
    obj <- make_rna_obj()
    ref <- rna_ref(obj)
    groups <- list(
        A = rownames(ref)[31:45],
        B = rownames(ref)[46:60]
    )
    obj2 <- SeuratObject::SetAssayData(obj, assay = "RNA", layer = "scale.data",
        new.data = ref[seq_len(30), , drop = FALSE])
    res <- EnsureSeuratScaleData(obj2, features = groups)
    got <- SeuratObject::GetAssayData(res, assay = "RNA", layer = "scale.data")
    expect_setequal(rownames(got), rownames(ref))
    got <- got[match(rownames(ref), rownames(got)), ]
    expect_equal(as.matrix(got), as.matrix(ref), tolerance = 1e-6)
})

test_that("RNA: metadata column names are not treated as missing features", {
    obj <- make_rna_obj()
    ref <- rna_ref(obj)
    obj$ZZZ_meta <- rnorm(ncol(obj))

    # all requested features are in scale.data, only the meta column would be missing
    obj2 <- SeuratObject::SetAssayData(obj, assay = "RNA", layer = "scale.data",
        new.data = ref)
    res <- EnsureSeuratScaleData(obj2, features = c(rownames(ref)[1:5], "ZZZ_meta"))
    expect_identical(res, obj2)

    # with a real missing feature next to the meta column name
    obj3 <- SeuratObject::SetAssayData(obj, assay = "RNA", layer = "scale.data",
        new.data = ref[seq_len(30), , drop = FALSE])
    res2 <- EnsureSeuratScaleData(obj3, features = c(rownames(ref)[31], "ZZZ_meta"))
    got <- SeuratObject::GetAssayData(res2, assay = "RNA", layer = "scale.data")
    expect_false("ZZZ_meta" %in% rownames(got))
    expect_true(all(rownames(ref)[1:31] %in% rownames(got)))
})

test_that("RNA: features absent from the data layer are skipped with a warning", {
    obj <- make_rna_obj()
    expect_warning(
        res <- EnsureSeuratScaleData(obj, features = "ZZZ"),
        "not in the data layer"
    )
    expect_identical(res, obj)

    # mix of a real missing feature and an absent one: real one still added
    ref <- rna_ref(obj)
    obj2 <- SeuratObject::SetAssayData(obj, assay = "RNA", layer = "scale.data",
        new.data = ref[seq_len(30), , drop = FALSE])
    expect_warning(
        res2 <- EnsureSeuratScaleData(obj2, features = c(rownames(ref)[31], "ZZZ")),
        "not in the data layer"
    )
    got <- SeuratObject::GetAssayData(res2, assay = "RNA", layer = "scale.data")
    expect_true(all(rownames(ref)[1:31] %in% rownames(got)))
    expect_false("ZZZ" %in% rownames(got))
})

test_that("RNA: zero-variance features match the all-feature scaling zeros", {
    obj <- make_rna_obj()
    ref <- rna_ref(obj)
    # make one gene constant in the data layer
    d <- SeuratObject::GetAssayData(obj, layer = "data")
    d["G60", ] <- 1
    obj <- SeuratObject::SetAssayData(obj, layer = "data", new.data = d)
    ref <- rna_ref(obj)

    obj2 <- SeuratObject::SetAssayData(obj, assay = "RNA", layer = "scale.data",
        new.data = ref[seq_len(30), , drop = FALSE])
    res <- EnsureSeuratScaleData(obj2, features = rownames(ref))
    got <- SeuratObject::GetAssayData(res, assay = "RNA", layer = "scale.data")
    expect_equal(as.numeric(got["G60", ]), as.numeric(ref["G60", ]), tolerance = 1e-6)
})

test_that("SCT: adds residuals for features missing from scale.data (umi assay present)", {
    obj <- make_sct_obj()
    marker <- sct_markers(obj)[1]
    before <- sct_ref(obj)
    expect_false(marker %in% rownames(before))

    res <- EnsureSeuratScaleData(obj, features = marker, umi_assay = "RNA")
    got <- sct_ref(res)
    expect_true(marker %in% rownames(got))
    expect_equal(nrow(got), nrow(before) + 1)
    # pre-existing rows untouched
    expect_equal(as.matrix(got[rownames(before), ]), as.matrix(before))
    # new row equals what Seurat::GetResidual computes
    oracle <- sct_oracle_row(obj, marker)
    expect_equal(as.numeric(got[marker, ]), as.numeric(oracle))
})

test_that("SCT: residuals computed from the SCT counts when the umi assay was removed", {
    obj <- make_sct_obj()
    marker <- sct_markers(obj)[1]
    oracle <- sct_oracle_row(obj, marker)

    obj2 <- obj
    obj2[["RNA"]] <- NULL
    res <- EnsureSeuratScaleData(obj2, features = marker)
    got <- sct_ref(res)
    expect_true(marker %in% rownames(got))
    expect_equal(as.numeric(got[marker, ]), as.numeric(oracle))
})

test_that("SCT: scale.data is created when empty and the umi assay was removed", {
    obj <- make_sct_obj()
    marker <- sct_markers(obj)[1]
    oracle <- sct_oracle_row(obj, marker)

    obj2 <- obj
    obj2[["RNA"]] <- NULL
    obj2@assays$SCT@scale.data <- matrix(numeric(), 0, 0)  # stripped like a real object
    expect_equal(dim(sct_ref(obj2)), c(0, 0))

    res <- EnsureSeuratScaleData(obj2, features = marker)
    got <- sct_ref(res)
    expect_true(marker %in% rownames(got))
    expect_equal(as.numeric(got[marker, ]), as.numeric(oracle))
})
