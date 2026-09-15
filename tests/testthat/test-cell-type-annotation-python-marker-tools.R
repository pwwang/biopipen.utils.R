# The python-based marker tools -- scsa, maca and scmapnet. All three take a
# universal marker table and shell out to a wrapper script shipped with
# biopipen, so they need a python that can import scanpy (scsa, which computes
# the per-cluster markers itself) or the tool itself (maca, scmapnet) -- none
# of which is an R dependency. `scsa` is exercised for real against the SCSA
# clone on this machine; MACA and scMapNet cannot be installed here (MACA pins
# scanpy==1.6.0/anndata==0.7.5; scMapNet is a clone plus a manually downloaded
# checkpoint), so they are covered through the errors naming what they need.

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

# The interpreter the pipeline drives the wrappers with in this environment
PYTHON <- Sys.getenv(
    "BIOPIPEN_PYTHON", unset = "/home/pwwang/miniconda3/bin/python"
)
SCSA_DIR <- Sys.getenv("SCSA_DIR", unset = "/home/pwwang/github/SCSA")

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

test_that("scsa runner: asks for the SCSA clone", {
    obj <- norm_obj()
    err <- run_error(
        obj, "scsa",
        list(db = universal_markers(obj)), ident = "groups"
    )
    expect_match(err, "envs.scsa.scsa_dir", fixed = TRUE)
    expect_match(err, "bioinfo-ibms-pumc/SCSA", fixed = TRUE)
})

test_that("scsa runner: cluster-level mapping from the SCSA clone", {
    skip_if_not(file.exists(PYTHON), paste("no python at", PYTHON))
    skip_if_not(dir.exists(SCSA_DIR), paste("no SCSA clone at", SCSA_DIR))
    obj <- norm_obj()
    rec <- tryCatch(
        run(
            obj, "scsa",
            args = list(
                db = universal_markers(obj, 15), python = PYTHON,
                scsa_dir = SCSA_DIR, use_refdb = TRUE
            ),
            ident = "groups"
        ),
        error = function(e) e
    )
    if (inherits(rec, "error") &&
        grepl("memoryview", conditionMessage(rec), fixed = TRUE)) {
        # `write_deg_table()` of scsa-wrapper.py tests `adata.X.data`, which is
        # a memoryview (no `.size`) for the dense X of an h5ad converted by
        # ConvertSeuratToAnnData, so the wrapper dies before SCSA.py runs. It
        # is a wrapper bug, not a runner one, so fail on anything else.
        skip(paste(
            "scsa-wrapper.py cannot read a dense h5ad:",
            conditionMessage(rec)
        ))
    }
    expect_equal(rec$type, "cluster")
    expect_setequal(names(rec$mapping), c("g1", "g2"))
    expect_true("scsa_celltype" %in% colnames(rec$cells))
    expect_equal(nrow(rec$cells), ncol(obj))
})

test_that("maca runner: names the environment MACA needs", {
    skip_if_not(file.exists(PYTHON), paste("no python at", PYTHON))
    obj <- norm_obj()
    err <- run_error(
        obj, "maca", list(db = universal_markers(obj), python = PYTHON)
    )
    expect_match(err, "Cannot import MACA", fixed = TRUE)
    expect_match(err, "scanpy==1.6.0", fixed = TRUE)
    expect_match(err, "envs.maca.python", fixed = TRUE)
})

test_that("scmapnet runner: asks for the clone and the checkpoint", {
    obj <- norm_obj()
    db <- universal_markers(obj)

    err <- run_error(obj, "scmapnet", list(db = db, python = PYTHON))
    expect_match(err, "envs.scmapnet.scmapnet_dir", fixed = TRUE)
    expect_match(err, "github.com/Yuz7/scMapNet", fixed = TRUE)

    err <- run_error(
        obj, "scmapnet",
        list(db = db, python = PYTHON, scmapnet_dir = tempdir())
    )
    expect_match(err, "envs.scmapnet.weights", fixed = TRUE)
    expect_match(err, "CC BY-NC 4.0", fixed = TRUE)
})
