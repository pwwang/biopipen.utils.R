# Smoke tests for the marker/reference-based runners. pbmc_small: 80 cells,
# 230 genes, metadata `groups` (g1, g2).

norm_obj <- function() {
    obj <- SeuratObject::pbmc_small
    suppressWarnings(Seurat::NormalizeData(obj, verbose = FALSE))
}

universal_markers <- function(obj, n = 5) {
    genes <- rownames(obj)
    df <- data.frame(
        cell_type = rep(c("A", "B"), each = n),
        gene = c(genes[1:n], genes[(n + 1):(2 * n)]),
        stringsAsFactors = FALSE
    )
    f <- tempfile(fileext = ".tsv")
    write.table(df, f, sep = "\t", quote = FALSE, row.names = FALSE)
    f
}

test_that("scsorter runner: cell-level and both-mode", {
    skip_if_not_installed("scSorter")
    obj <- norm_obj()
    # without `ident`: upstream scSorter labels every cell it is given
    rec <- RunCellTypeAnnotation(
        obj, "scsorter", args = list(db = universal_markers(obj)),
        cache = tempfile("cta-")
    )
    expect_equal(rec$type, "cell")
    expect_true("scsorter_celltype" %in% colnames(rec$mapping))
    expect_equal(nrow(rec$mapping), ncol(obj))
    expect_identical(rownames(rec$mapping), colnames(obj))

    rec2 <- RunCellTypeAnnotation(
        obj, "scsorter", args = list(db = universal_markers(obj)),
        ident = "groups", cache = tempfile("cta-")
    )
    expect_equal(rec2$type, "cluster")
    expect_setequal(names(rec2$mapping), c("g1", "g2"))
})

test_that("singler runner: cell-level and both-mode from a SummarizedExperiment reference", {
    skip_if_not_installed("SingleR")
    skip_if_not_installed("SummarizedExperiment")
    obj <- norm_obj()
    # Reference labels are deliberately different from the cluster ids. The
    # Bioconductor build names the per-cluster rows; the old CRAN build (no
    # `test` formal) returns an unnamed list, so the name assertion below is
    # only made when the Bioconductor API is in use.
    labels <- ifelse(as.character(obj$groups) == "g1", "TypeA", "TypeB")
    ref <- SummarizedExperiment::SummarizedExperiment(
        assays = list(logcounts = as.matrix(SeuratObject::GetAssayData(obj, layer = "data"))),
        colData = S4Vectors::DataFrame(label = labels)
    )
    rds <- tempfile(fileext = ".rds")
    saveRDS(ref, rds)

    # without `ident`: no `clusters` is passed, so SingleR labels each cell
    cell_rec <- suppressWarnings(suppressMessages(RunCellTypeAnnotation(
        obj, "singler", args = list(db = rds), cache = tempfile("cta-")
    )))
    expect_equal(cell_rec$type, "cell")
    expect_true("singler_celltype" %in% colnames(cell_rec$mapping))
    expect_equal(nrow(cell_rec$mapping), ncol(obj))
    expect_identical(rownames(cell_rec$mapping), colnames(obj))
    expect_true(all(cell_rec$mapping$singler_celltype %in% c("TypeA", "TypeB")))

    rec <- suppressWarnings(suppressMessages(RunCellTypeAnnotation(
        obj, "singler", args = list(db = rds), ident = "groups", cache = tempfile("cta-")
    )))
    expect_equal(rec$type, "cluster")
    expect_equal(length(rec$mapping), 2)
    expect_true(all(unlist(rec$mapping) %in% c("TypeA", "TypeB")))
    if ("test" %in% names(formals(SingleR::SingleR))) {
        expect_setequal(names(rec$mapping), c("g1", "g2"))
    }
})

test_that("scina runner: cell-level and both-mode", {
    skip_if_not_installed("SCINA")
    obj <- norm_obj()
    db <- universal_markers(obj)
    rec <- RunCellTypeAnnotation(obj, "scina", args = list(db = db), cache = tempfile("cta-"))
    expect_equal(rec$type, "cell")
    expect_equal(nrow(rec$mapping), ncol(obj))
    expect_true("scina_celltype" %in% colnames(rec$mapping))

    rec2 <- RunCellTypeAnnotation(
        obj, "scina", args = list(db = db), ident = "groups", cache = tempfile("cta-")
    )
    expect_equal(rec2$type, "cluster")
    expect_setequal(names(rec2$mapping), c("g1", "g2"))
    expect_equal(nrow(rec2$cells), ncol(obj))
})

test_that("cellid runner: cell-level and both-mode", {
    suppressWarnings(skip_if_not_installed("CelliD"))
    obj <- norm_obj()
    rec <- suppressWarnings(suppressMessages(RunCellTypeAnnotation(
        obj, "cellid", args = list(db = universal_markers(obj, 15)), cache = tempfile("cta-")
    )))
    expect_equal(rec$type, "cell")
    expect_equal(nrow(rec$mapping), ncol(obj))

    rec2 <- suppressWarnings(suppressMessages(RunCellTypeAnnotation(
        obj, "cellid", args = list(db = universal_markers(obj, 15)), ident = "groups",
        cache = tempfile("cta-")
    )))
    expect_equal(rec2$type, "cluster")
    expect_equal(nrow(rec2$cells), ncol(obj))
})

test_that("sccatch and llmcelltype runners exist (heavy deps not run here)", {
    expect_true(is.function(biopipen.utils:::.run_celltypeannotation_sccatch))
    expect_true(is.function(biopipen.utils:::.run_celltypeannotation_llmcelltype))
})
