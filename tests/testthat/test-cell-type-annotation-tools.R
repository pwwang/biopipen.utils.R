# Smoke tests for the individual tool runners. Each test needs its tool package;
# the heavy python-based tools are covered by their own tasks.

test_that("hitype runner: cell-level and cluster-level", {
    skip_if_not_installed("hitype")
    obj <- SeuratObject::pbmc_small
    rec <- RunCellTypeAnnotation(
        obj, "hitype", args = list(db = "hitypedb_pbmc3k"), cache = tempfile("cta-")
    )
    expect_equal(rec$type, "cell")
    expect_equal(nrow(rec$mapping), ncol(obj))
    expect_true("hitype" %in% colnames(rec$mapping))

    rec2 <- RunCellTypeAnnotation(
        obj, "hitype", args = list(db = "hitypedb_pbmc3k"), ident = "groups",
        cache = tempfile("cta-")
    )
    expect_equal(rec2$type, "cluster")
    expect_setequal(names(rec2$mapping), c("g1", "g2"))
})

test_that("sctype runner: cluster-level mapping from a universal marker table", {
    skip_if_not_installed("HGNChelper")
    obj <- SeuratObject::pbmc_small
    obj <- suppressWarnings(Seurat::NormalizeData(obj, verbose = FALSE))
    obj <- suppressWarnings(Seurat::ScaleData(obj, features = rownames(obj), verbose = FALSE))
    genes <- rownames(obj)
    markers <- data.frame(
        cell_type = rep(c("A", "B"), each = 5),
        gene = c(genes[1:5], genes[11:15]),
        stringsAsFactors = FALSE
    )
    f <- tempfile(fileext = ".tsv")
    write.table(markers, f, sep = "\t", quote = FALSE, row.names = FALSE)

    rec <- RunCellTypeAnnotation(
        obj, "sctype", args = list(db = f), ident = "groups", cache = tempfile("cta-")
    )
    expect_equal(rec$type, "cluster")
    expect_setequal(names(rec$mapping), c("g1", "g2"))
    expect_true(all(unlist(rec$mapping) %in% c("A", "B", "Unknown")))
})
