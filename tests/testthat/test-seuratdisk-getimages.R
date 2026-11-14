# SeuratDisk's GetImages() passes `index$global$images` to unlist()'s
# `recursive` argument POSITIONALLY:
#     unique(x = c(unlist(x = assays.images, index$global$images)))
# R 4.4 silently ignored it; R >= 4.5 aborts with
# "'recursive' must be a length-1 vector", so every LoadH5Seurat() of an
# h5Seurat carrying global images fails (the AnnData2Seurat proc of the
# CellCellCommunication / CellTypist pipelines hit exactly this).
# biopipen.utils patches the function; these tests pin the upstream difference,
# the patched behaviour, and that monkey_patch() installs it.

skip_if_not_installed("SeuratDisk")

index_with_images <- function() {
    list(
        global = list(images = c("a", "b")),
        # GetAssays() expects `slots` as a named logical vector
        RNA = list(images = "img", slots = c(counts = TRUE, data = TRUE))
    )
}

index_without_images <- function() {
    list(
        global = list(images = NULL),
        RNA = list(images = NULL, slots = c(counts = TRUE))
    )
}

patched <- function() utils::getFromNamespace(".GetImages", "biopipen.utils")

test_that("unlist()'s `recursive` argument changed in R 4.5 (the upstream bug)", {
    # this is the exact expression SeuratDisk's GetImages() evaluates; the
    # second, positional argument is `recursive`
    if (getRversion() >= "4.5") {
        expect_error(
            unlist(list("img"), c("a", "b")),
            "'recursive' must be a length-1 vector",
            fixed = TRUE
        )
    } else {
        expect_identical(unlist(list("img"), c("a", "b")), "img")
    }
    # the form the patch uses: works on both, keeps both image sets
    expect_setequal(
        unique(c(unlist(list("img")), c("a", "b"))),
        c("img", "a", "b")
    )
})

test_that("the patched GetImages() returns the image union on any R version", {
    idx <- index_with_images()
    expect_setequal(patched()(images = NULL, index = idx, assays = "RNA"), c("a", "b", "img"))
    expect_setequal(patched()(images = "img", index = idx, assays = "RNA"), "img")
    expect_setequal(patched()(images = c("a", "img"), index = idx, assays = "RNA"), c("a", "img"))
})

test_that("the patched GetImages() keeps the early returns", {
    idx <- index_with_images()
    expect_null(patched()(images = FALSE, index = idx, assays = "RNA"))
    expect_identical(patched()(images = NA, index = idx, assays = "RNA"), c("a", "b"))
    expect_null(patched()(images = NULL, index = index_without_images(), assays = "RNA"))
})

test_that("monkey_patch() installs the patched GetImages in SeuratDisk", {
    monkey <- utils::getFromNamespace("monkey_patch", "biopipen.utils")
    monkey("SeuratDisk", "GetImages", patched())

    installed <- utils::getFromNamespace("GetImages", "SeuratDisk")
    # the installed copy is ours, not the buggy upstream one
    expect_identical(deparse(installed), deparse(patched()))

    idx <- index_with_images()
    res <- tryCatch(
        installed(images = NULL, index = idx, assays = "RNA"),
        error = function(e) paste("ERROR:", conditionMessage(e))
    )
    expect_setequal(res, c("a", "b", "img"))
})
