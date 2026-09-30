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

# --- recording the clip range of the scalers ------------------------------
# The clip the scalers applied is recorded in `object@misc$scale_clip` (one
# entry per assay), so EnsureSeuratScaleData() can reuse it without being told.

test_that("RecordScaleClip: writes misc$scale_clip, returns the object, validates", {
    obj <- make_rna_obj()
    res <- RecordScaleClip(obj, "RNA", c(-10, 10))
    expect_s4_class(res, "Seurat")
    expect_equal(res@misc$scale_clip[["RNA"]], c(-10, 10))

    # assay = NULL records against the default assay
    res2 <- RecordScaleClip(obj, clip.range = c(-5, 5))
    expect_equal(res2@misc$scale_clip[["RNA"]], c(-5, 5))

    # an existing record is overwritten, and the records are per assay
    res3 <- RecordScaleClip(res, "RNA", c(-1, 1))
    expect_equal(res3@misc$scale_clip[["RNA"]], c(-1, 1))
    res4 <- RecordScaleClip(res3, "SCT", c(-2, 2))
    expect_equal(res4@misc$scale_clip[["RNA"]], c(-1, 1))
    expect_equal(res4@misc$scale_clip[["SCT"]], c(-2, 2))

    expect_error(RecordScaleClip(obj, "RNA", -10), "numeric vector of length 2")
    expect_error(RecordScaleClip(obj, "RNA", c(1, 2, 3)), "numeric vector of length 2")
    expect_error(RecordScaleClip(obj, "RNA", c("a", "b")), "numeric vector of length 2")
})

test_that("RunSeuratTransformation records the clip ScaleData used", {
    # suppressWarnings: RunPCA on the tiny fixture warns about the number of
    # dimensions, which is unrelated to the recorded clip
    obj <- suppressWarnings(
        RunSeuratTransformation(SeuratObject::pbmc_small, cache = FALSE)
    )
    expect_equal(obj@misc$scale_clip[["RNA"]], c(-10, 10))

    obj2 <- suppressWarnings(RunSeuratTransformation(
        SeuratObject::pbmc_small,
        cache = FALSE,
        ScaleDataArgs = list(scale.max = Inf)
    ))
    expect_equal(obj2@misc$scale_clip[["RNA"]], c(-Inf, Inf))
})

test_that("RunSeuratTransformation records the clip of the SCT models", {
    obj <- suppressWarnings(RunSeuratTransformation(
        SeuratObject::pbmc_small, use_sct = TRUE, cache = FALSE
    ))
    expect_s4_class(obj[["SCT"]], "SCTAssay")
    model <- levels(obj[["SCT"]])[1]
    # the value SCTransform stored in the model, read back from both slots
    expect_equal(
        obj@misc$scale_clip[["SCT"]],
        SCTResults(obj[["SCT"]], "clips", model = model)$sct
    )
    expect_equal(
        obj@misc$scale_clip[["SCT"]],
        SCTResults(obj[["SCT"]], "arguments", model = model)$sct.clip.range
    )
})

# --- clamping of the added z-scores ---------------------------------------
# A single outlier cell drives |z| to ~sqrt(n_cells - 1): at 40 cells
# (make_rna_obj) that is only ~6.2, so the cap needs a bigger fixture.
# Two degenerate genes in the data layer:
#   G_SPIKE - all zeros except one cell far above zero -> large positive z
#   G_DROP  - constant positive in every cell except one zero -> large negative z

# Seurat rewrites "_" to "-" in feature names at creation time, and rows not in
# the assay's features are silently clipped on SetAssayData, so the degenerate
# genes are created as GSPIKE/GDROP and renamed afterwards. The layers go
# through the slots directly: the public LayerData<- setter reconciles the layer
# against the features slot (still holding the old names) and drops the row.
rename_features <- function(obj, map) {
    assay <- obj[["RNA"]]
    nm <- function(x) {
        hit <- match(x, names(map))
        ifelse(is.na(hit), x, unname(map[hit]))
    }
    for (lyr in names(assay@layers)) {
        x <- assay@layers[[lyr]]
        dimnames(x)[[1]] <- nm(dimnames(x)[[1]])
        assay@layers[[lyr]] <- x
    }
    rownames(assay@features) <- nm(rownames(assay@features))
    obj[["RNA"]] <- assay
    obj
}

make_degenerate_rna_obj <- function(nc = 400, seed = 1) {
    cnt <- make_counts(58, nc, lambda = 0.8, seed = seed)
    degen <- matrix(0, 2, nc, dimnames = list(c("GSPIKE", "GDROP"), colnames(cnt)))
    degen[, seq_len(5)] <- 1  # detected in a few cells, like a real gene
    obj <- Seurat::CreateSeuratObject(rbind(cnt, as(degen, "dgCMatrix")))
    obj <- Seurat::NormalizeData(obj, verbose = FALSE)
    obj <- rename_features(obj, c(GSPIKE = "G_SPIKE", GDROP = "G_DROP"))
    d <- SeuratObject::GetAssayData(obj, layer = "data")
    d["G_SPIKE", ] <- 0
    d["G_SPIKE", 1] <- 50
    d["G_DROP", ] <- 40
    d["G_DROP", 1] <- 0
    SeuratObject::SetAssayData(obj, layer = "data", new.data = d)
}

# the z-scores an uncapped scaling of the degenerate genes gives, i.e. what
# `data` holds after t(scale(t(...))) without any clipping
degen_raw <- function(obj) {
    data <- SeuratObject::GetAssayData(obj, layer = "data")
    raw <- t(scale(t(as.matrix(data[c("G_SPIKE", "G_DROP"), , drop = FALSE]))))
    # scale() tags the result with the centre/scale it used; compare the values
    attr(raw, "scaled:center") <- NULL
    attr(raw, "scaled:scale") <- NULL
    raw
}

degen_scaled <- function(res) {
    as.matrix(SeuratObject::GetAssayData(
        res, assay = "RNA", layer = "scale.data"
    )[c("G_SPIKE", "G_DROP"), ])
}

test_that("default: no record and no clip.range clamps the z-scores to +/- 10", {
    obj <- make_degenerate_rna_obj()
    raw <- degen_raw(obj)
    expect_gt(max(raw), 10)      # the fixture does violate a +/- 10 cap
    expect_lt(min(raw), -10)
    res <- EnsureSeuratScaleData(obj, features = c("G_SPIKE", "G_DROP"))
    got <- degen_scaled(res)
    # entries inside the default range are written verbatim
    within <- abs(raw) <= 10
    expect_equal(got[within], raw[within], tolerance = 1e-6)
    # the offending entries are pinned to the default range, on both tails
    expect_gt(sum(raw > 10), 0)
    expect_gt(sum(raw < -10), 0)
    expect_equal(got[raw > 10], rep(10, sum(raw > 10)))
    expect_equal(got[raw < -10], rep(-10, sum(raw < -10)))

    # the default is a no-op for a well-behaved gene, which still matches the
    # scaling of all features by Seurat::ScaleData
    ref <- rna_ref(obj)
    res2 <- EnsureSeuratScaleData(obj, features = rownames(ref))
    got2 <- as.matrix(SeuratObject::GetAssayData(
        res2, assay = "RNA", layer = "scale.data"
    ))
    got2 <- got2[match(rownames(ref), rownames(got2)), ]
    keep <- !rownames(ref) %in% c("G_SPIKE", "G_DROP")
    # premise: the regular genes of the fixture stay well inside +/- 10 (~1.7)
    expect_lt(max(abs(got2[keep, ])), 10)
    expect_lt(max(abs(got2[keep, ] - as.matrix(ref[keep, ]))), 1e-9)
})

test_that("default: a recorded clip.range wins over the +/- 10 default", {
    obj <- make_degenerate_rna_obj()
    raw <- degen_raw(obj)
    expect_gt(max(raw), 5)       # the record is tighter than the fixture
    expect_lt(min(raw), -5)
    res <- EnsureSeuratScaleData(
        RecordScaleClip(obj, "RNA", c(-5, 5)),
        features = c("G_SPIKE", "G_DROP")
    )
    got <- degen_scaled(res)
    within <- abs(raw) <= 5
    expect_equal(got[within], raw[within], tolerance = 1e-6)
    expect_gt(sum(raw > 5), 0)
    expect_gt(sum(raw < -5), 0)
    expect_equal(got[raw > 5], rep(5, sum(raw > 5)))
    expect_equal(got[raw < -5], rep(-5, sum(raw < -5)))
})

test_that("default: clip.range = NULL opts out of clamping", {
    obj <- make_degenerate_rna_obj()
    raw <- degen_raw(obj)
    res <- EnsureSeuratScaleData(obj, features = c("G_SPIKE", "G_DROP"), clip.range = NULL)
    expect_equal(degen_scaled(res), raw, tolerance = 1e-6)
    # an explicit NULL also beats a recorded range
    res2 <- EnsureSeuratScaleData(
        RecordScaleClip(obj, "RNA", c(-5, 5)),
        features = c("G_SPIKE", "G_DROP"),
        clip.range = NULL
    )
    expect_equal(degen_scaled(res2), raw, tolerance = 1e-6)
})

test_that("default: a malformed clip.range is rejected", {
    obj <- make_degenerate_rna_obj()
    feats <- c("G_SPIKE", "G_DROP")
    expect_error(
        EnsureSeuratScaleData(obj, features = feats, clip.range = -10),
        "numeric vector of length 2"
    )
    expect_error(
        EnsureSeuratScaleData(obj, features = feats, clip.range = c(1, 2, 3)),
        "numeric vector of length 2"
    )
    expect_error(
        EnsureSeuratScaleData(obj, features = feats, clip.range = c("a", "b")),
        "numeric vector of length 2"
    )
})

test_that("reuse: a recorded clip.range clamps both tails", {
    obj <- make_degenerate_rna_obj()
    raw <- degen_raw(obj)
    res <- EnsureSeuratScaleData(
        RecordScaleClip(obj, "RNA", c(-10, 10)),
        features = c("G_SPIKE", "G_DROP")
    )
    got <- degen_scaled(res)
    # entries inside the cap are written verbatim
    within <- abs(raw) <= 10
    expect_equal(got[within], raw[within], tolerance = 1e-6)
    # the offending entries are pinned to the cap, on both tails
    expect_gt(sum(raw > 10), 0)
    expect_gt(sum(raw < -10), 0)
    expect_equal(got[raw > 10], rep(10, sum(raw > 10)))
    expect_equal(got[raw < -10], rep(-10, sum(raw < -10)))
})

test_that("reuse: an explicit clip.range wins over the record", {
    obj <- RecordScaleClip(make_degenerate_rna_obj(), "RNA", c(-10, 10))
    raw <- degen_raw(obj)
    res <- EnsureSeuratScaleData(
        obj,
        features = c("G_SPIKE", "G_DROP"),
        clip.range = c(-5, 5)
    )
    # the explicit range is not written back to the record
    expect_equal(res@misc$scale_clip[["RNA"]], c(-10, 10))
    got <- degen_scaled(res)
    within <- abs(raw) <= 5
    expect_equal(got[within], raw[within], tolerance = 1e-6)
    expect_gt(sum(raw > 5), 0)
    expect_equal(got[raw > 5], rep(5, sum(raw > 5)))
    expect_lte(max(got), 5)
    expect_gte(min(got), -5)
})

test_that("reuse: an infinite clip.range keeps the uncapped z-scores", {
    obj <- make_degenerate_rna_obj()
    raw <- degen_raw(obj)
    expect_gt(max(raw), 10)
    expect_lt(min(raw), -10)
    res <- EnsureSeuratScaleData(
        RecordScaleClip(obj, "RNA", c(-10, 10)),
        features = c("G_SPIKE", "G_DROP"),
        clip.range = c(-Inf, Inf)
    )
    expect_equal(degen_scaled(res), raw, tolerance = 1e-6)
    # an explicit infinite range opts out of the default too
    res2 <- EnsureSeuratScaleData(
        obj, features = c("G_SPIKE", "G_DROP"), clip.range = c(-Inf, Inf)
    )
    expect_equal(degen_scaled(res2), raw, tolerance = 1e-6)
})

test_that("SCT: the +/- 10 default does not bite with no record", {
    obj <- make_sct_obj()
    marker <- sct_markers(obj)[1]
    oracle <- sct_oracle_row(obj, marker)
    # The models clip their residuals at +/- 1.63, so the oracle stays well
    # inside the +/- 10 default and the default clamp is a no-op here; the
    # added row is asserted against the oracle, not against the model clip.
    expect_lt(max(abs(oracle)), 10)
    res <- EnsureSeuratScaleData(obj, features = marker)
    expect_equal(as.numeric(sct_ref(res)[marker, ]), as.numeric(oracle))
})

test_that("SCT: a record tighter than the model clip clamps the added rows", {
    obj <- make_sct_obj()
    before <- sct_ref(obj)

    marker <- sct_markers(obj)[1]
    oracle <- sct_oracle_row(obj, marker)
    expect_gt(max(oracle), 1)    # the model clip (+/- ~1.6) exceeds the record
    res <- EnsureSeuratScaleData(
        RecordScaleClip(obj, "SCT", c(-1, 1)),
        features = marker
    )
    got <- sct_ref(res)
    # pre-existing rows are copied verbatim
    expect_equal(as.matrix(got[rownames(before), ]), as.matrix(before))
    # the added row is clamped, and only outside the record
    expect_lte(max(got[marker, ]), 1)
    expect_gte(min(got[marker, ]), -1)
    within <- abs(oracle) <= 1
    expect_equal(as.numeric(got[marker, ])[within], as.numeric(oracle)[within], tolerance = 1e-6)
    expect_gt(sum(oracle > 1), 0)
    expect_equal(as.numeric(got[marker, ])[oracle > 1], rep(1, sum(oracle > 1)))

    # the lower bound bites too (residuals of this fixture bottom out ~-0.6)
    marker2 <- sct_markers(obj)[2]
    oracle2 <- sct_oracle_row(obj, marker2)
    expect_lt(min(oracle2), -0.3)
    res2 <- EnsureSeuratScaleData(
        RecordScaleClip(obj, "SCT", c(-0.3, 1)),
        features = marker2
    )
    got2 <- as.numeric(sct_ref(res2)[marker2, ])
    expect_lte(max(got2), 1)
    expect_gte(min(got2), -0.3)
    within2 <- oracle2 >= -0.3 & oracle2 <= 1
    expect_equal(got2[within2], as.numeric(oracle2)[within2], tolerance = 1e-6)
    expect_equal(got2[oracle2 < -0.3], rep(-0.3, sum(oracle2 < -0.3)))
})
