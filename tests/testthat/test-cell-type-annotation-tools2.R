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

    # `group_gsea` is additive: left unset and set to FALSE are the same run
    rec2_off <- suppressWarnings(suppressMessages(RunCellTypeAnnotation(
        obj, "cellid", args = list(db = universal_markers(obj, 15), group_gsea = FALSE),
        ident = "groups", cache = tempfile("cta-")
    )))
    expect_identical(rec2_off, rec2)

    # with `group_gsea = TRUE` the cluster mapping comes from CelliD's own
    # group mode (RunGroupGSEA), while `cells` stays the per-cell HGT frame
    rec3 <- suppressWarnings(suppressMessages(RunCellTypeAnnotation(
        obj, "cellid", args = list(db = universal_markers(obj, 15), group_gsea = TRUE),
        ident = "groups", cache = tempfile("cta-")
    )))
    expect_equal(rec3$type, "cluster")
    expect_setequal(names(rec3$mapping), c("g1", "g2"))
    expect_true(all(unlist(rec3$mapping) %in% c("A", "B")))
    expect_equal(nrow(rec3$cells), ncol(obj))
    expect_identical(rec3$cells, rec2$cells)
})

test_that("garnett runner: cell-level, both-mode, and cluster_extend", {
    skip_if_not_installed("garnett")
    skip_if_not_installed("SeuratWrappers")
    skip_if_not_installed("monocle3")
    skip_if_not_installed("withr")
    # garnett's own code calls monocle3's `colData()` unqualified (it imports
    # monocle3 as a whole, not the function), so the namespace has to be on the
    # search path while it trains and classifies
    withr::local_package("monocle3")
    obj <- SeuratObject::pbmc_small
    genes <- rownames(obj)
    markers <- data.frame(
        cell_type = rep(c("A", "B"), each = 10),
        gene = c(genes[1:10], genes[11:20]),
        stringsAsFactors = FALSE
    )
    marker_file <- tempfile(fileext = ".txt")
    markers_to_garnett_file(markers, marker_file)

    # The classifier is trained here (db = "none": symbols as-is, no gene ID
    # conversion), so the test needs no fixture binary and no network
    cds <- suppressWarnings(SeuratWrappers::as.cell_data_set(obj))
    cds <- monocle3::estimate_size_factors(cds)
    classifier <- suppressWarnings(garnett::train_cell_classifier(
        cds = cds, marker_file = marker_file, db = "none",
        cds_gene_id_type = "custom", marker_file_gene_id_type = "custom",
        min_observations = 5, num_unknown = 50
    ))
    classifier_file <- tempfile(fileext = ".rds")
    saveRDS(classifier, classifier_file)

    run <- function(args, ident = "groups") {
        suppressWarnings(suppressMessages(RunCellTypeAnnotation(
            obj, "garnett", args = args, ident = ident, cache = tempfile("cta-")
        )))
    }

    # without `ident`: per-cell labels
    cell_rec <- run(list(classifier = classifier_file), ident = NULL)
    expect_equal(cell_rec$type, "cell")
    expect_equal(nrow(cell_rec$mapping), ncol(obj))
    expect_true("garnett_celltype" %in% colnames(cell_rec$mapping))

    # `cluster_extend` is additive: left unset and set to FALSE are the same
    rec <- run(list(classifier = classifier_file))
    rec_off <- run(list(classifier = classifier_file, cluster_extend = FALSE))
    expect_identical(rec_off, rec)
    expect_equal(rec$type, "cluster")
    expect_setequal(names(rec$mapping), c("g1", "g2"))
    expect_equal(nrow(rec$cells), ncol(obj))
    expect_true(all(unlist(rec$mapping) %in% c("A", "B", "Unknown")))

    # with `cluster_extend = TRUE` + `ident`, the cluster mapping comes from
    # garnett's own `cluster_ext_type` (the `garnett_cluster` column is fed
    # from `ident`), and the per-cell frame is untouched
    rec_ext <- run(list(classifier = classifier_file, cluster_extend = TRUE))
    expect_equal(rec_ext$type, "cluster")
    expect_setequal(names(rec_ext$mapping), c("g1", "g2"))
    expect_true(all(unlist(rec_ext$mapping) %in% c("A", "B", "Unknown")))
    expect_true(all(vapply(rec_ext$mapping, length, integer(1)) == 1L))
    expect_equal(nrow(rec_ext$cells), ncol(obj))
    expect_identical(rec_ext$cells, rec$cells)
})

test_that("sccatch and llmcelltype runners exist (heavy deps not run here)", {
    expect_true(is.function(biopipen.utils:::.run_celltypeannotation_sccatch))
    expect_true(is.function(biopipen.utils:::.run_celltypeannotation_llmcelltype))
})
