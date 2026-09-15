# Structural tests for the heavy/python-based runners. Their real behaviour is
# verified against pipeline artifacts, not on pbmc_small: here we only prove the
# runners exist, are reachable through the engine's switch, and fail for their
# own dependency/missing-input reasons (never with a missing function).

test_that("heavy runners exist and are reachable through the engine", {
    obj <- SeuratObject::pbmc_small
    expect_true(is.function(biopipen.utils:::.run_celltypeannotation_garnett))
    expect_true(is.function(biopipen.utils:::.run_celltypeannotation_celltypist))
    expect_true(is.function(biopipen.utils:::.run_celltypeannotation_schdeepinsight))
    expect_true(is.function(biopipen.utils:::.run_celltypeannotation_scbert))
    expect_true(is.function(biopipen.utils:::.run_celltypeannotation_scagenttype))
    expect_true(is.function(biopipen.utils:::.run_celltypeannotation_cellassign))
    expect_true(is.function(patch_garnett_make_predictions))

    reaches <- function(tool, args = list(), ident = NULL) {
        err <- tryCatch(
            {
                RunCellTypeAnnotation(
                    obj, tool, args = args, ident = ident, cache = tempfile("cta-")
                )
                NULL
            },
            error = function(e) conditionMessage(e)
        )
        expect_false(is.null(err), info = paste(tool, "unexpectedly succeeded"))
        expect_false(
            grepl("could not find function", err, fixed = TRUE),
            info = paste(tool, "runner not reachable:", err)
        )
        invisible(err)
    }

    suppressWarnings(reaches("garnett"))
    suppressWarnings(reaches("cellassign"))
    suppressWarnings(reaches("scbert"))
    suppressWarnings(reaches("schdeepinsight"))
    suppressWarnings(reaches("scagenttype"))
    suppressWarnings(reaches("celltypist"))
})

test_that("the h5ad tools declare h5ad and get a cached conversion path", {
    tools <- celltype_annotation_tools()
    expect_true(all(vapply(
        c("celltypist", "schdeepinsight", "scbert", "scagenttype"),
        function(t) isTRUE(tools[[t]]$h5ad), logical(1)
    )))
    expect_false(isTRUE(tools$garnett$h5ad))
    expect_false(isTRUE(tools$cellassign$h5ad))
})
