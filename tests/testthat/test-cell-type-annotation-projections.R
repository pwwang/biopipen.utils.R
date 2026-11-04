# Tests for RunCellTypeAnnotation(return_object = TRUE) and
# SeuratProjectionPayload(), the payload used by biopipen's
# CellTypeAnnotation process to carry a case's projections out of a fork.
# pbmc_small ships with two reductions (pca, tsne).

obj <- SeuratObject::pbmc_small
obj$clusters <- factor(as.character(obj$groups), levels = c("g1", "g2"))

test_that("RunCellTypeAnnotation return_object returns record and object", {
    res <- RunCellTypeAnnotation(
        obj, "direct",
        args = list(cell_types = list(g1 = "T", g2 = "B")),
        ident = "clusters",
        return_object = TRUE
    )
    expect_type(res, "list")
    expect_named(res, c("record", "object"))
    # the direct tool produces no object to hand back
    expect_null(res$object)
    expect_identical(res$record$type, "cluster")

    # the default call returns the record alone, untouched
    rec <- RunCellTypeAnnotation(
        obj, "direct",
        args = list(cell_types = list(g1 = "T", g2 = "B")),
        ident = "clusters"
    )
    expect_identical(rec, res$record)
    expect_null(attr(rec, "object"))
})

test_that("SeuratProjectionPayload extracts the reductions", {
    expected <- Reductions(obj)  # pca, tsne
    expect_gte(length(expected), 2)

    payload <- SeuratProjectionPayload(obj)
    expect_length(payload, length(expected))
    expect_setequal(vapply(payload, `[[`, character(1), "name"), expected)
    for (entry in payload) {
        expect_true(inherits(entry$embeddings, "matrix"))
        expect_equal(nrow(entry$embeddings), ncol(obj))
        expect_type(entry$key, "character")
        expect_true(nzchar(entry$key))
        expect_identical(entry$assay, DefaultAssay(obj[[entry$name]]))
    }
})

test_that("SeuratProjectionPayload honours exclude", {
    payload <- SeuratProjectionPayload(obj, exclude = "pca")
    expect_setequal(
        vapply(payload, `[[`, character(1), "name"),
        setdiff(Reductions(obj), "pca")
    )
})
