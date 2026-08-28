# Tests for RunModuleScoring
# pbmc_small (230 genes) needs small nbin/ctrl or AddModuleScore errors
# "cannot take a sample larger than the population"

obj <- NormalizeData(SeuratObject::pbmc_small)

test_that("seurat: comma-string gives one column per module key", {
    res <- RunModuleScoring(
        obj,
        modules = list(
            Exhaustion = list(features = c("MS4A1", "CD79A")),
            Prolif = list(features = c("GZMB", "GNLY"))
        ),
        nbin = 10, ctrl = 5
    )
    expect_true(all(c("Exhaustion", "Prolif") %in% colnames(res@meta.data)))
    expect_false(any(grepl("^(Exhaustion|Prolif)[0-9]+$", colnames(res@meta.data))))
    expect_true("RunModuleScoring" %in% names(res@commands))
})

test_that("seurat: named list gives one column per name", {
    res <- RunModuleScoring(
        obj,
        modules = list(CytotoxicT = list(
            features = list(
                Exhaustion = c("MS4A1", "CD79A"),
                Activation = c("GZMB", "GNLY"),
                Proliferation = c("CST3", "TCL1A")
            )
        )),
        nbin = 10, ctrl = 5
    )
    expect_true(all(c("CytotoxicT_Exhaustion", "CytotoxicT_Activation", "CytotoxicT_Proliferation") %in%
                    colnames(res@meta.data)))
    expect_false(any(grepl("^CytotoxicT[0-9]+$", colnames(res@meta.data))))
})

test_that("seurat: unnamed multi-group gives key1, key2", {
    res <- RunModuleScoring(
        obj,
        modules = list(Double = list(
            features = list(c("MS4A1", "CD79A"), c("GZMB", "GNLY"))
        )),
        nbin = 10, ctrl = 5
    )
    expect_true(all(c("Double1", "Double2") %in% colnames(res@meta.data)))
})

test_that("method is per-module", {
    skip_if_not_installed("UCell")
    res <- RunModuleScoring(
        obj,
        modules = list(
            UExh = list(features = "MS4A1,CD79A", maxRank = 500),
            SExh = list(features = "GZMB,GNLY", method = "seurat")
        ),
        method = "ucell", nbin = 10, ctrl = 5
    )
    expect_true(all(c("UExh", "SExh") %in% colnames(res@meta.data)))
})

test_that("errors", {
    expect_error(
        RunModuleScoring(obj, modules = list(Bad = list(features = ""))),
        "no features"
    )
    expect_error(
        RunModuleScoring(obj, modules = list(Bad = list(features = ""))),
        "no features"
    )
    expect_error(
        RunModuleScoring(obj, modules = list(Bad = list(features = "A", method = "nope"))),
        "arg"
    )
    # with destiny installed the module actually runs; `features` is the
    # number of components and must be a positive integer
    expect_error(
        RunModuleScoring(obj, modules = list(Bad = list(features = "A", kind = "dm"))),
        if (requireNamespace("destiny", quietly = TRUE)) "positive integer" else "destiny"
    )
    # scps runs on the bundled implementation; a signature whose genes are
    # not scaled in the object errors with the ScaleData hint
    expect_error(
        RunModuleScoring(obj, modules = list(Scps = list(features = "A", method = "scps"))),
        if (requireNamespace("GSEABase", quietly = TRUE)) "not scaled" else "GSEABase"
    )
    # with AUCell installed the module actually runs; a signature with no
    # genes found in the object triggers AUCell's own error
    expect_error(
        RunModuleScoring(obj, modules = list(Auc = list(features = "A", method = "aucell"))),
        if (requireNamespace("AUCell", quietly = TRUE)) "genes" else "AUCell"
    )
    expect_error(
        RunModuleScoring(obj, modules = list()),
        "No modules"
    )
})

test_that("ssgsea", {
    skip_if_not_installed("GSVA")
    res <- RunModuleScoring(
        obj,
        modules = list(Sig1 = list(features = "MS4A1,CD79A")),
        method = "ssgsea"
    )
    expect_true("Sig1" %in% colnames(res@meta.data))
})

test_that("diffusion map (destiny)", {
    skip_if_not_installed("destiny")
    # `kind: "dm"` creates a reduction plus one meta column per component;
    # destiny internally triggers a Matrix coercion deprecation warning
    res <- suppressWarnings(RunModuleScoring(
        obj,
        modules = list(DM = list(kind = "dm")),
        nbin = 10, ctrl = 5
    ))
    expect_true(all(c("DM_1", "DM_2") %in% colnames(res@meta.data)))
    expect_true("DM" %in% Reductions(res))
    expect_equal(ncol(res[["DM"]]@cell.embeddings), 2)

    # `features` is the number of components to keep; PCA embeddings can
    # be used via `n_pcs`
    res <- suppressWarnings(RunModuleScoring(
        obj,
        modules = list(DM = list(kind = "dm", features = 3, n_pcs = 10)),
        nbin = 10, ctrl = 5
    ))
    expect_true(all(c("DM_1", "DM_2", "DM_3") %in% colnames(res@meta.data)))
})

test_that("aucell", {
    skip_if_not_installed("AUCell")
    res <- RunModuleScoring(
        obj,
        modules = list(Auc = list(features = "MS4A1,CD79A")),
        method = "aucell"
    )
    expect_true("Auc" %in% colnames(res@meta.data))
    expect_true(all(is.finite(res$Auc)))
})

test_that("scps", {
    skip_if_not_installed("GSEABase")
    # scPS runs PCA on the scale.data of the signature genes, so the
    # object must be scaled for them
    scaled <- ScaleData(obj, features = rownames(obj))
    res <- suppressWarnings(RunModuleScoring(
        scaled,
        modules = list(Scps = list(features = "MS4A1,CD79A,GZMB,GNLY,CST3,TCL1A")),
        method = "scps"
    ))
    expect_true("Scps" %in% colnames(res@meta.data))
    expect_true(all(is.finite(res$Scps)))
})

test_that("jasmine and scse", {
    res <- RunModuleScoring(
        obj,
        modules = list(Sig1 = list(features = "MS4A1,CD79A")),
        method = "jasmine"
    )
    expect_true("Sig1" %in% colnames(res@meta.data))
    expect_true(all(res@meta.data$Sig1 >= 0 & res@meta.data$Sig1 <= 1))

    res <- RunModuleScoring(
        obj,
        modules = list(Sig2 = list(features = "MS4A1,CD79A")),
        method = "scse"
    )
    expect_true("Sig2" %in% colnames(res@meta.data))
    expect_true(all(res@meta.data$Sig2 >= 0))

    # missing-gene signatures give NA, not errors
    res <- RunModuleScoring(
        obj,
        modules = list(Sig3 = list(features = "NOTAGENE")),
        method = "jasmine"
    )
    expect_true(all(is.na(res@meta.data$Sig3)))
})

# synthetic object with human cc genes (pbmc_small lacks them)
set.seed(1)
syn_genes <- c("GNLY", "MS4A1", "CD79A", "GZMB", "CST3", "TCL1A",
               Seurat::cc.genes$s.genes[1:30], Seurat::cc.genes$g2m.genes[1:30])
syn_counts <- matrix(rpois(80 * length(syn_genes), 1), nrow = length(syn_genes),
                     dimnames = list(syn_genes, paste0("cell", 1:80)))
syn <- NormalizeData(CreateSeuratObject(counts = syn_counts))

test_that("cell cycle scoring", {
    res <- RunModuleScoring(
        syn,
        modules = list(`_` = list(features = "cc.genes")), # `_` is a reserved key: no column prefix; or using kind = "cc"
        nbin = 10, ctrl = 5
    )
    expect_true(all(c("S.Score", "G2M.Score", "Phase") %in% colnames(res@meta.data)))
    expect_true(all(res$Phase %in% c("S", "G2M", "G1")))

    res <- RunModuleScoring(
        syn,
        modules = list(CellCycle = list(features = "cc.genes.mouse")),
        method = "ucell"
    )
    expect_true(all(c("CellCycle_S.Score", "CellCycle_G2M.Score", "CellCycle_Phase") %in% colnames(res@meta.data)))

    skip_if_not_installed("UCell")
    res <- RunModuleScoring(
        syn,
        modules = list(`_` = list(features = "cc.genes")),
        method = "ucell"
    )
    expect_true(all(c("S.Score", "G2M.Score", "Phase") %in% colnames(res@meta.data)))

    res <- RunModuleScoring(
        syn,
        modules = list(`_` = list(features = "cc.genes")),
        method = "scse"
    )
    expect_true(all(c("S.Score", "G2M.Score", "Phase") %in% colnames(res@meta.data)))
})
