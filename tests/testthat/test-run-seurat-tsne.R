# Tests for RunSeuratTSNE() and RunTSNEArgs on RunSeuratClustering() /
# RunSeuratSubClustering().

make_tsne_fixture <- function() {
    obj <- SeuratObject::pbmc_small
    # pbmc_small ships with a pre-computed "tsne" reduction; drop it so tests
    # observe only reductions produced by the functions under test.
    obj[["tsne"]] <- NULL
    obj <- Seurat::NormalizeData(obj, verbose = FALSE)
    obj <- Seurat::FindVariableFeatures(obj, verbose = FALSE)
    obj <- Seurat::ScaleData(obj, verbose = FALSE)
    obj <- suppressWarnings(Seurat::RunPCA(obj, verbose = FALSE))
    obj
}

test_that("1. RunSeuratTSNE adds an 80x2 tsne reduction", {
    obj <- make_tsne_fixture()
    res <- RunSeuratTSNE(obj, list(dims = 10, perplexity = 10), cache = FALSE)

    expect_true("tsne" %in% Seurat::Reductions(res))
    expect_equal(dim(Seurat::Embeddings(res, "tsne")), c(80L, 2L))
})

test_that("2. RunSeuratTSNE records resolved default reduction 'pca'", {
    obj <- make_tsne_fixture()
    res <- RunSeuratTSNE(obj, list(dims = 10, perplexity = 10), cache = FALSE)

    expect_equal(
        res@commands[["RunSeuratTSNE"]]@params$RunTSNEArgs$reduction,
        "pca"
    )
})

test_that("3. RunSeuratTSNE honours integrated_new_reduction", {
    obj <- make_tsne_fixture()
    res_pca <- RunSeuratTSNE(
        obj,
        list(dims = 10, perplexity = 10),
        cache = FALSE
    )

    # Create a "harmony" reduction whose cell embeddings genuinely differ from
    # "pca" so we prove Seurat::RunTSNE actually consumes it.
    emb <- Seurat::Embeddings(obj, "pca")
    emb_h <- emb[rev(seq_len(nrow(emb))), , drop = FALSE]
    rownames(emb_h) <- rownames(emb)
    colnames(emb_h) <- paste0("harmony_", seq_len(ncol(emb_h)))
    obj[["harmony"]] <- Seurat::CreateDimReducObject(
        embeddings = emb_h,
        key = "harmony_",
        assay = "RNA"
    )
    obj@misc$integrated_new_reduction <- "harmony"

    res_harmony <- RunSeuratTSNE(
        obj,
        list(dims = 10, perplexity = 10),
        cache = FALSE
    )

    expect_equal(
        res_harmony@commands[["RunSeuratTSNE"]]@params$RunTSNEArgs$reduction,
        "harmony"
    )
    expect_false(identical(
        Seurat::Embeddings(res_harmony, "tsne")[1:10, , drop = FALSE],
        Seurat::Embeddings(res_pca, "tsne")[1:10, , drop = FALSE]
    ))
})

test_that("4. RunSeuratTSNE respects custom reduction.name", {
    obj <- make_tsne_fixture()
    res <- RunSeuratTSNE(
        obj,
        list(dims = 10, perplexity = 10, reduction.name = "tsne2"),
        cache = FALSE
    )

    expect_true("tsne2" %in% Seurat::Reductions(res))
    expect_false("tsne" %in% Seurat::Reductions(res))
})

test_that("5. RunSeuratTSNE handles numeric features via DE markers and sets dims = NULL", {
    obj <- make_tsne_fixture()
    # Sparse expression across 10 DE markers has duplicate all-zero cell rows
    # on pbmc_small, so pass check_duplicates = FALSE for Rtsne.
    res <- RunSeuratTSNE(
        obj,
        list(features = 10, perplexity = 10, check_duplicates = FALSE),
        cache = FALSE
    )
    res_pca5 <- RunSeuratTSNE(
        obj,
        list(dims = 1:5, perplexity = 10),
        cache = FALSE
    )

    expect_equal(dim(Seurat::Embeddings(res, "tsne")), c(80L, 2L))
    params <- res@commands[["RunSeuratTSNE"]]@params$RunTSNEArgs
    expect_equal(length(params$features), 10L)
    expect_null(params$dims)
    # Prove Seurat::RunTSNE actually took the features branch instead of its
    # default dims = 1:5.
    expect_false(identical(
        Seurat::Embeddings(res, "tsne")[1:10, , drop = FALSE],
        Seurat::Embeddings(res_pca5, "tsne")[1:10, , drop = FALSE]
    ))
})

test_that("6. RunSeuratTSNE clamps default perplexity on 80 cells", {
    obj <- make_tsne_fixture()
    res <- RunSeuratTSNE(obj, list(dims = 10), cache = FALSE)

    expect_true("tsne" %in% Seurat::Reductions(res))
    expect_equal(
        res@commands[["RunSeuratTSNE"]]@params$RunTSNEArgs$perplexity,
        floor((80 - 1) / 3)
    )
})

test_that("7. RunSeuratTSNE errors when supplied perplexity exceeds Rtsne limit", {
    obj <- make_tsne_fixture()
    expect_error(
        RunSeuratTSNE(obj, list(dims = 10, perplexity = 40), cache = FALSE),
        "\\[RunSeuratTSNE\\].*perplexity.*40.*80.*26"
    )
})

test_that("8. RunSeuratTSNE expands scalar dims to 1:N", {
    obj <- make_tsne_fixture()
    res <- RunSeuratTSNE(obj, list(dims = 10, perplexity = 10), cache = FALSE)

    expect_equal(
        res@commands[["RunSeuratTSNE"]]@params$RunTSNEArgs$dims,
        1:10
    )
})

test_that("9. RunSeuratClustering with RunTSNEArgs produces pca, umap, and tsne", {
    obj <- make_tsne_fixture()
    res <- RunSeuratClustering(
        obj,
        RunTSNEArgs = list(dims = 10, perplexity = 10),
        cache = FALSE
    )

    expect_true(all(c("pca", "umap", "tsne") %in% Seurat::Reductions(res)))
})

test_that("10. RunSeuratClustering without RunTSNEArgs skips t-SNE and preserves clusters", {
    obj <- make_tsne_fixture()
    res_with <- RunSeuratClustering(
        obj,
        RunTSNEArgs = list(dims = 10, perplexity = 10),
        cache = FALSE
    )
    res_without <- RunSeuratClustering(obj, cache = FALSE)

    expect_false("tsne" %in% Seurat::Reductions(res_without))
    expect_setequal(Seurat::Reductions(res_without), c("pca", "umap"))
    expect_true(identical(
        levels(SeuratObject::Idents(res_with)),
        levels(SeuratObject::Idents(res_without))
    ))
    expect_true(identical(
        SeuratObject::Idents(res_with),
        SeuratObject::Idents(res_without)
    ))
})

test_that("11. RunSeuratClustering caches t-SNE separately", {
    obj <- make_tsne_fixture()
    cache_dir <- tempfile("tsne_cache_")
    on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

    res1 <- RunSeuratClustering(
        obj,
        RunTSNEArgs = list(dims = 10, perplexity = 10),
        cache = cache_dir
    )
    expect_true("tsne" %in% Seurat::Reductions(res1))

    expect_output(
        res2 <- RunSeuratClustering(
            obj,
            RunTSNEArgs = list(dims = 10, perplexity = 10),
            cache = cache_dir
        ),
        "t-SNE results loaded from cache"
    )
    expect_true("tsne" %in% Seurat::Reductions(res2))
})

test_that("12. RunSeuratSubClustering with RunTSNEArgs attaches prefixed tsne with unique key", {
    obj <- make_tsne_fixture()
    res <- RunSeuratSubClustering(
        obj,
        subset = "groups == 'g1'",
        name = "g1sub",
        RunTSNEArgs = list(dims = 10, perplexity = 5),
        cache = FALSE
    )

    expect_true("g1sub.tsne" %in% Seurat::Reductions(res))
    expect_equal(SeuratObject::Key(res[["g1sub.tsne"]]), "G1SUBTSNE_")
    expect_false("tsne" %in% Seurat::Reductions(res))
})

test_that("13. RunSeuratSubClustering without RunTSNEArgs skips t-SNE", {
    obj <- make_tsne_fixture()
    res_null <- RunSeuratSubClustering(
        obj,
        subset = "groups == 'g1'",
        name = "g1sub",
        RunTSNEArgs = NULL,
        cache = FALSE
    )
    res_default <- RunSeuratSubClustering(
        obj,
        subset = "groups == 'g1'",
        name = "g1sub",
        cache = FALSE
    )

    expect_false("g1sub.tsne" %in% Seurat::Reductions(res_default))
    expect_false("tsne" %in% Seurat::Reductions(res_default))
    expect_false("g1sub.tsne" %in% Seurat::Reductions(res_null))
    expect_false("tsne" %in% Seurat::Reductions(res_null))
    expect_identical(res_default$g1sub, res_null$g1sub)
})

test_that("14. RunSeuratTSNE handles cells subset perplexity and features+dims warning", {
    obj <- make_tsne_fixture()
    sub_cells <- colnames(obj)[1:20]
    res_cells <- RunSeuratTSNE(
        obj,
        list(dims = 10, cells = sub_cells),
        cache = FALSE
    )
    expect_equal(dim(Seurat::Embeddings(res_cells, "tsne")), c(20L, 2L))
    expect_equal(
        res_cells@commands[["RunSeuratTSNE"]]@params$RunTSNEArgs$perplexity,
        floor((20 - 1) / 3)
    )

    expect_output(
        res_fd <- RunSeuratTSNE(
            obj,
            list(
                features = rownames(obj)[1:15],
                dims = 10,
                perplexity = 10,
                check_duplicates = FALSE
            ),
            cache = FALSE
        ),
        "will be ignored"
    )
    expect_null(res_fd@commands[["RunSeuratTSNE"]]@params$RunTSNEArgs$dims)
})

test_that("15. RunSeuratTSNE deduplicates identical coordinates and keeps all cells", {
    expect_output(
        res <- RunSeuratTSNE(SeuratObject::pbmc_small, list(dims = 10)),
        "WARN.*5 of 80 cells have identical coordinates.*running t-SNE on the 75 unique cells"
    )

    expect_true("tsne" %in% Seurat::Reductions(res))
    expect_equal(dim(Seurat::Embeddings(res, "tsne")), c(80L, 2L))
})

test_that("16. RunSeuratTSNE succeeds and deduplicates even with check_duplicates = TRUE", {
    expect_output(
        res <- RunSeuratTSNE(
            SeuratObject::pbmc_small,
            list(dims = 10, check_duplicates = TRUE)
        ),
        "running t-SNE on the 75 unique cells"
    )

    expect_true("tsne" %in% Seurat::Reductions(res))
    expect_equal(dim(Seurat::Embeddings(res, "tsne")), c(80L, 2L))
    params <- res@commands[["RunSeuratTSNE"]]@params
    expect_true(params$RunTSNEArgs$check_duplicates)
    expect_equal(length(params$RunTSNEArgs$cells), 75L)
})

test_that("17. RunSeuratTSNE maps each duplicate onto its representative's coordinates", {
    expect_output(
        res <- RunSeuratTSNE(SeuratObject::pbmc_small, list(dims = 10)),
        "identical coordinates"
    )

    tsne <- Seurat::Embeddings(res, "tsne")
    expect_equal(nrow(unique(round(tsne, 10))), 75L)

    emb <- Seurat::Embeddings(SeuratObject::pbmc_small, "pca")[
        , 1:10, drop = FALSE
    ]
    key <- do.call(paste, c(as.data.frame(emb), sep = "\r"))
    dup_groups <- split(seq_along(key), key)
    dup_groups <- dup_groups[lengths(dup_groups) > 1L]
    expect_true(length(dup_groups) > 0L)
    for (idx in dup_groups) {
        for (dup in idx[-1L]) {
            expect_equal(tsne[dup, ], tsne[idx[1L], ], tolerance = 0)
        }
    }

    params <- res@commands[["RunSeuratTSNE"]]@params
    expect_equal(params$dedup$n_duplicates, 5L)
    expect_equal(params$dedup$n_unique, 75L)
    expect_equal(params$dedup$cells, 80L)
    expect_equal(length(params$RunTSNEArgs$cells), 75L)
})

test_that("18. RunSeuratTSNE leaves cells and dedup unset with no warning when duplicate-free", {
    obj <- make_tsne_fixture()
    res <- NULL
    out <- capture.output(
        expect_no_warning(
            res <- RunSeuratTSNE(
                obj,
                list(dims = 10, perplexity = 10),
                cache = FALSE
            )
        )
    )

    expect_true("tsne" %in% Seurat::Reductions(res))
    expect_null(
        res@commands[["RunSeuratTSNE"]]@params$RunTSNEArgs$check_duplicates
    )
    expect_null(res@commands[["RunSeuratTSNE"]]@params$RunTSNEArgs$cells)
    expect_null(res@commands[["RunSeuratTSNE"]]@params$dedup)
    expect_equal(nrow(Seurat::Embeddings(res, "tsne")), ncol(obj))
    expect_false(any(grepl("WARN|identical coordinates", out)))
})

test_that("19. RunSeuratTSNE warns and retries with check_duplicates = FALSE on features path", {
    obj <- make_tsne_fixture()
    expect_output(
        res <- RunSeuratTSNE(
            obj,
            list(features = rownames(obj)[1:15], perplexity = 10),
            cache = FALSE
        ),
        "retrying with check_duplicates = FALSE"
    )

    expect_equal(dim(Seurat::Embeddings(res, "tsne")), c(80L, 2L))
})
