# ScInfeR (Swain, Shekhawat & Yadav, 2025, doi:10.1093/bib/bbaf253) is an R
# package distributed on GitHub only (GPL-3). It annotates the *clusters* --
# round 1 scores each cluster against the marker table -- and reads a reduction
# off the object for the message-passing round it falls back to when a second
# cell type scores within 0.1 of the best one, which labels those cells
# individually. So `ident` is mandatory and the per-cell labels come back in
# `cells`. It is installed on this machine, and a marker table of our own needs
# no network, so it is exercised for real here.

# ScInfeR needs a reduction (default `umap`); pbmc_small carries `pca` and
# `tsne` but no `umap`
scinfer_obj <- function() {
    obj <- SeuratObject::pbmc_small
    obj <- suppressWarnings(Seurat::NormalizeData(obj, verbose = FALSE))
    if (!"pca" %in% Seurat::Reductions(obj)) {
        obj <- suppressWarnings(Seurat::RunPCA(obj, verbose = FALSE))
    }
    suppressWarnings(Seurat::RunUMAP(obj, dims = 1:10, verbose = FALSE))
}

# A universal marker table (cell_type/gene) over the object's own genes, so
# every marker is found in the expression matrix. No `weight` column: the
# conversion falls back to weight 1.
scinfer_markers <- function(obj, n = 5) {
    df <- data.frame(
        cell_type = rep(c("A", "B"), each = n),
        gene = c(rownames(obj)[1:n], rownames(obj)[(n + 1):(2 * n)]),
        stringsAsFactors = FALSE
    )
    f <- tempfile(fileext = ".tsv")
    write.table(df, f, sep = "\t", quote = FALSE, row.names = FALSE)
    f
}

run <- function(obj, tool, args, ident = NULL) {
    suppressWarnings(suppressMessages(RunCellTypeAnnotation(
        obj, tool, args = args, ident = ident, cache = tempfile("cta-")
    )))
}

# Run a tool expected to fail, and hand back the message of the error it raised
run_error <- function(obj, tool, args, ident = NULL) {
    err <- suppressWarnings(tryCatch(
        run(obj, tool, args = args, ident = ident),
        error = function(e) conditionMessage(e)
    ))
    expect_true(
        is.character(err),
        info = paste(tool, "unexpectedly succeeded")
    )
    err
}

test_that("scinfer is registered and reachable as a cluster-level tool", {
    tools <- celltype_annotation_tools("scinfer")
    expect_equal(tools$scinfer$level, "cluster")
    expect_false(isTRUE(tools$scinfer$h5ad))
    expect_true(is.function(get(
        ".run_celltypeannotation_scinfer",
        envir = asNamespace("biopipen.utils")
    )))
})

test_that("markers_to_scinfer_df drops the negative markers, weights by 1", {
    df <- data.frame(
        cell_type = c("A", "A", "A", "B"),
        gene = c("G1", "G2", "G3", "G4"),
        direction = c("positive", "negative", "positive", "positive"),
        stringsAsFactors = FALSE
    )
    out <- markers_to_scinfer_df(df)
    expect_equal(colnames(out), c("celltype", "marker", "weight"))
    expect_equal(out$celltype, c("A", "A", "B"))
    expect_equal(out$marker, c("G1", "G3", "G4"))
    expect_equal(out$weight, c(1, 1, 1))

    # a `weight` column is passed through
    df$weight <- c(0.5, 0.1, 2, 3)
    out <- markers_to_scinfer_df(df)
    expect_equal(out$weight, c(0.5, 2, 3))
})

test_that("scinfer runner: cluster mapping and per-cell labels on pbmc_small", {
    obj <- scinfer_obj()
    rec <- run(
        obj, "scinfer",
        args = list(db = scinfer_markers(obj)),
        ident = "groups"
    )
    expect_equal(rec$type, "cluster")
    expect_setequal(names(rec$mapping), c("g1", "g2"))
    expect_true("scinfer_celltype" %in% colnames(rec$cells))
    expect_equal(nrow(rec$cells), ncol(obj))
    expect_setequal(rownames(rec$cells), colnames(obj))
    # every cluster is labelled with one of the marker table's cell types
    expect_true(all(unlist(rec$mapping) %in% c("A", "B")))
})

test_that("scinfer runner: names the reduction the object is missing", {
    obj <- scinfer_obj()
    err <- run_error(
        obj, "scinfer",
        args = list(db = scinfer_markers(obj), reduction = "harmony"),
        ident = "groups"
    )
    expect_match(err, "scinfer.reduction", fixed = TRUE)
    expect_match(err, "harmony", fixed = TRUE)
    # the object's own reductions are named, `umap` among them
    expect_match(err, "umap", fixed = TRUE)
    expect_match(err, "pca", fixed = TRUE)
})
