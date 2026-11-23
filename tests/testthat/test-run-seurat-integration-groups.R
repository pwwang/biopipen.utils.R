# Behavioural tests for `group.by.vars` in RunSeuratIntegration().
# Seurat's HarmonyIntegration() ignores `group.by.vars`, so the integration
# function must run harmony directly on the PCA embeddings instead.

library(biopipen.utils)
suppressPackageStartupMessages(library(Seurat))

make_integration_object <- function() {
    set.seed(42)
    n_genes <- 120L
    n_per_sample <- 80L
    samples <- rep(c("Sample1", "Sample2", "Sample3"), each = n_per_sample)

    counts <- matrix(
        rpois(n_genes * length(samples), lambda = 5),
        nrow = n_genes,
        dimnames = list(
            paste0("Gene", seq_len(n_genes)),
            paste0("Cell", seq_along(samples))
        )
    )
    # Sample-specific expression so the PCA embeddings have real structure.
    counts[1:20, samples == "Sample3"] <- counts[1:20, samples == "Sample3"] * 3
    counts[21:40, samples != "Sample3"] <- counts[21:40, samples != "Sample3"] * 3
    counts <- Matrix::Matrix(counts, sparse = TRUE)

    meta <- data.frame(
        Sample = factor(samples, levels = c("Sample1", "Sample2", "Sample3")),
        row.names = colnames(counts)
    )
    obj <- CreateSeuratObject(counts = counts, meta.data = meta)
    # Deliberately not the same partition as `Sample`.
    obj$Batch2 <- factor(
        ifelse(obj$Sample == "Sample3", "BatchB", "BatchA"),
        levels = c("BatchA", "BatchB")
    )

    obj[["RNA"]] <- split(obj[["RNA"]], f = obj$Sample)
    obj <- NormalizeData(obj, verbose = FALSE)
    obj <- FindVariableFeatures(obj, verbose = FALSE)
    obj <- ScaleData(obj, verbose = FALSE)
    RunPCA(obj, npcs = 15, verbose = FALSE)
}

test_that("group.by.vars changes the harmony embeddings", {
    skip_if_not_installed("harmony")

    obj <- make_integration_object()

    set.seed(1)
    no_groups <- RunSeuratIntegration(
        obj,
        IntegrateLayersArgs = list(method = "harmony"),
        cache = FALSE
    )
    set.seed(1)
    with_groups <- RunSeuratIntegration(
        obj,
        IntegrateLayersArgs = list(method = "harmony", group.by.vars = "Batch2"),
        cache = FALSE
    )

    expect_true("harmony" %in% Reductions(no_groups))
    expect_true("harmony" %in% Reductions(with_groups))

    emb_no_groups <- Embeddings(no_groups[["harmony"]])
    emb_with_groups <- Embeddings(with_groups[["harmony"]])

    expect_equal(dim(emb_no_groups), dim(emb_with_groups))
    # Before the fix, `group.by.vars` was silently ignored and these were equal.
    expect_gt(mean(abs(emb_no_groups - emb_with_groups)), 1e-3)
})

test_that("group.by.vars harmony run is reproducible for a fixed seed", {
    skip_if_not_installed("harmony")

    obj <- make_integration_object()

    set.seed(7)
    first <- RunSeuratIntegration(
        obj,
        IntegrateLayersArgs = list(method = "harmony", group.by.vars = "Batch2"),
        cache = FALSE
    )
    set.seed(7)
    second <- RunSeuratIntegration(
        obj,
        IntegrateLayersArgs = list(method = "harmony", group.by.vars = "Batch2"),
        cache = FALSE
    )

    expect_equal(
        Embeddings(first[["harmony"]]),
        Embeddings(second[["harmony"]]),
        tolerance = 1e-8
    )
})

test_that("group.by.vars is validated and rejected for non-harmony methods", {
    skip_if_not_installed("harmony")

    obj <- make_integration_object()

    expect_error(
        RunSeuratIntegration(
            obj,
            IntegrateLayersArgs = list(method = "harmony", group.by.vars = "NotAColumn"),
            cache = FALSE
        ),
        "NotAColumn"
    )
    expect_error(
        RunSeuratIntegration(
            obj,
            IntegrateLayersArgs = list(method = "cca", group.by.vars = "Batch2"),
            cache = FALSE
        ),
        "Only the harmony method supports"
    )
})

test_that("RunHarmonyIntegration records the covariates used", {
    skip_if_not_installed("harmony")

    obj <- make_integration_object()

    set.seed(11)
    integrated <- RunSeuratIntegration(
        obj,
        IntegrateLayersArgs = list(method = "harmony", group.by.vars = "Batch2"),
        cache = FALSE
    )

    expect_true("RunHarmonyIntegration" %in% names(integrated@commands))
    params <- integrated@commands[["RunHarmonyIntegration"]]@params
    expect_equal(params$group.by.vars, "Batch2")
    expect_equal(params$reduction, "pca")
    expect_equal(params$n_new_reduction, "harmony")
    expect_equal(integrated@misc$integrated_new_reduction, "harmony")
})
