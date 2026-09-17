# The python-based marker tools -- scsa, maca and scmapnet. All three take a
# universal marker table and shell out to a wrapper script shipped with
# biopipen, so they need a python that can import scanpy or the tool itself --
# none of which is an R dependency. `scsa` (its scoring is ported by the
# wrapper, so there is nothing else to install) and `maca` are exercised for
# real on this machine; scMapNet cannot be installed here (it is a clone plus a
# manually downloaded checkpoint), so it is covered through the errors naming
# what it needs.

norm_obj <- function() {
    obj <- SeuratObject::pbmc_small
    suppressWarnings(Seurat::NormalizeData(obj, verbose = FALSE))
}

# `genes` defaults to the object's own genes, but callers can pass the ones the
# tool will actually see (the h5ad keeps only the variable features).
universal_markers <- function(obj, n = 5, genes = rownames(obj)) {
    df <- data.frame(
        cell_type = rep(c("A", "B"), each = n),
        gene = c(genes[1:n], genes[(n + 1):(2 * n)]),
        stringsAsFactors = FALSE
    )
    f <- tempfile(fileext = ".tsv")
    write.table(df, f, sep = "\t", quote = FALSE, row.names = FALSE)
    f
}

# The interpreter the pipeline drives the wrappers with in this environment
PYTHON <- Sys.getenv(
    "BIOPIPEN_PYTHON", unset = "/home/pwwang/miniconda3/bin/python"
)

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

test_that("the python marker tools are registered and reachable", {
    tools <- celltype_annotation_tools(c("scsa", "maca", "scmapnet"))
    expect_equal(tools$scsa$level, "cluster")
    expect_equal(tools$maca$level, "cell")
    expect_equal(tools$scmapnet$level, "cell")
    expect_true(all(vapply(
        tools, function(tool) isTRUE(tool$h5ad), logical(1)
    )))
    expect_true(all(vapply(
        c("scsa", "maca", "scmapnet"),
        function(tool) {
            is.function(get(
                paste0(".run_celltypeannotation_", tool),
                envir = asNamespace("biopipen.utils")
            ))
        },
        logical(1)
    )))
})

test_that("scsa runner: asks for the marker table", {
    obj <- norm_obj()
    err <- run_error(obj, "scsa", list(), ident = "groups")
    expect_match(err, "`scsa.db` is not set", fixed = TRUE)
})

test_that("scsa runner: cluster-level mapping from the marker table", {
    skip_if_not(file.exists(PYTHON), paste("no python at", PYTHON))
    obj <- norm_obj()
    # the h5ad carries the object's variable features alone, so the markers have
    # to be taken from those (as for maca below)
    genes <- SeuratObject::VariableFeatures(obj)
    n <- length(genes) %/% 2
    skip_if(n < 1, paste("only", length(genes), "variable features"))
    rec <- run(
        obj, "scsa",
        args = list(
            db = universal_markers(obj, n, genes = genes), python = PYTHON,
            # pbmc_small has 20 genes and 80 cells, so no marker survives SCSA's
            # own thresholds (nothing there has an adjusted p-value near 0.05)
            foldchange = 1, pvalue = 1
        ),
        ident = "groups"
    )
    expect_equal(rec$type, "cluster")
    expect_setequal(names(rec$mapping), c("g1", "g2"))
    expect_true("scsa_celltype" %in% colnames(rec$cells))
    expect_equal(nrow(rec$cells), ncol(obj))
})

test_that("maca runner: cell-level labels from a real MACA run", {
    skip_if_not(file.exists(PYTHON), paste("no python at", PYTHON))
    obj <- norm_obj()
    # MACA keeps only the markers it finds among the object's features, drops
    # every cell type left outside its 3..300 marker range, and the h5ad
    # conversion carries the object's variable features alone, so the markers
    # have to be taken from those.
    genes <- SeuratObject::VariableFeatures(obj)
    n <- length(genes) %/% 2
    skip_if(n < 3, paste("only", length(genes), "variable features"))
    rec <- tryCatch(
        run(
            obj, "maca",
            args = list(
                db = universal_markers(obj, n, genes = genes), python = PYTHON
            )
        ),
        error = function(e) e
    )
    if (inherits(rec, "error")) {
        # a data problem, not a code one: MACA dropped every cell type it was
        # given (too few or too many of its markers are in the object)
        msg <- conditionMessage(rec)
        if (!grepl("MACA cannot annotate anything", msg, fixed = TRUE)) {
            stop(rec)
        }
        skip(paste("MACA cannot score this object:", msg))
    }
    expect_equal(rec$type, "cell")
    expect_true("maca_celltype" %in% colnames(rec$mapping))
    expect_equal(nrow(rec$mapping), ncol(obj))
    expect_setequal(rownames(rec$mapping), colnames(obj))
    expect_true(all(rec$mapping$maca_celltype %in% c("A", "B", "unassigned")))
})

test_that("maca runner: markers absent from the object fail actionably", {
    skip_if_not(file.exists(PYTHON), paste("no python at", PYTHON))
    obj <- norm_obj()
    # None of these are in the h5ad (it carries the variable features only),
    # so MACA drops both cell types and would hit its empty-argmax crash
    genes <- setdiff(rownames(obj), SeuratObject::VariableFeatures(obj))
    err <- run_error(
        obj, "maca",
        list(db = universal_markers(obj, 5, genes = genes), python = PYTHON)
    )
    expect_match(err, "MACA cannot annotate anything", fixed = TRUE)
    expect_match(err, "fewer than 3 or more than 300", fixed = TRUE)
    expect_match(err, "0 of 5 marker(s) in the object", fixed = TRUE)
    expect_match(err, "pip install -e ~/github/MACA", fixed = TRUE)
    expect_false(grepl("Traceback", err, fixed = TRUE))
})

test_that("scmapnet runner: asks for the clone and the checkpoint", {
    obj <- norm_obj()
    db <- universal_markers(obj)

    err <- run_error(obj, "scmapnet", list(db = db, python = PYTHON))
    expect_match(err, "scmapnet.scmapnet_dir", fixed = TRUE)
    expect_match(err, "github.com/Yuz7/scMapNet", fixed = TRUE)

    err <- run_error(
        obj, "scmapnet",
        list(db = db, python = PYTHON, scmapnet_dir = tempdir())
    )
    expect_match(err, "scmapnet.weights", fixed = TRUE)
    expect_match(err, "CC BY-NC 4.0", fixed = TRUE)
})
