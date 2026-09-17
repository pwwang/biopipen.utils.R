# Tests for RunCellTypeAnnotation() and the direct/cell runners.
# pbmc_small: 80 cells, 230 genes. `groups` is a character column holding g2 and
# g1 (in that order), so a factor column with a known level order is added for
# the order-sensitive assertions.

obj <- SeuratObject::pbmc_small
obj$clusters <- factor(as.character(obj$groups), levels = c("g1", "g2"))

test_that("celltype_annotation_tools lists the built-in tools", {
    tools <- celltype_annotation_tools()
    expect_true(all(c(
        "hitype", "sctype", "sccatch", "celltypist", "scsorter", "scina", "garnett",
        "singler", "schdeepinsight", "llmcelltype", "cellassign", "scbert",
        "scagenttype", "cellid", "direct", "cell",
        "ucell", "aucell", "gsva", "singscore"
    ) %in% names(tools)))
    expect_equal(tools$sctype$level, "cluster")
    expect_equal(tools$hitype$level, "cell")
    expect_true(tools$scbert$h5ad)
    expect_false(isTRUE(tools$sctype$h5ad))
    expect_error(celltype_annotation_tools("nope"), "Unknown cell type annotation tool")
})

test_that(".cta_scratch is deterministic and argument-sensitive", {
    s1 <- biopipen.utils:::.cta_scratch("/tmp/cache", "hitype", list(db = "x"), "groups")
    s2 <- biopipen.utils:::.cta_scratch("/tmp/cache", "hitype", list(db = "x"), "groups")
    s3 <- biopipen.utils:::.cta_scratch("/tmp/cache", "hitype", list(db = "y"), "groups")
    expect_identical(s1, s2)
    expect_false(identical(s1, s3))
    expect_true(grepl("celltype_annotation/hitype/", s1, fixed = TRUE))
})

test_that("direct runner: mapping, '-'/'NA' handling and more_cell_types", {
    rec <- RunCellTypeAnnotation(
        obj, "direct",
        args = list(
            cell_types = list(g1 = "T", g2 = "-"),
            more_cell_types = list(second = list(g1 = "X", g2 = "Y"))
        ),
        ident = "clusters"
    )
    expect_equal(rec$type, "cluster")
    expect_equal(rec$mapping, list(g1 = "T", g2 = "g2"))
    expect_equal(rec$more$second, list(g1 = "X", g2 = "Y"))
    expect_null(rec$cells)

    # a key that is not a cluster is dropped (logger warning); the rest is kept,
    # and clusters missing from the list keep their own names
    rec_bad <- RunCellTypeAnnotation(
        obj, "direct", args = list(cell_types = list(g1 = "T", g3 = "Z")),
        ident = "clusters"
    )
    expect_equal(rec_bad$mapping, list(g1 = "T", g2 = "g2"))

    # positional cell types are matched to the idents in level order
    rec2 <- RunCellTypeAnnotation(
        obj, "direct", args = list(cell_types = c("A", "B")), ident = "clusters"
    )
    expect_equal(rec2$mapping, list(g1 = "A", g2 = "B"))

    # "NA" marks a cluster to be removed downstream; "-" keeps the cluster name
    rec3 <- RunCellTypeAnnotation(
        obj, "direct", args = list(cell_types = list(g1 = "-", g2 = "NA")),
        ident = "clusters"
    )
    expect_equal(rec3$mapping, list(g1 = "g1", g2 = NA))

    # no cell types: empty mapping
    rec4 <- suppressWarnings(RunCellTypeAnnotation(
        obj, "direct", args = list(cell_types = list()), ident = "clusters"
    ))
    expect_equal(rec4$mapping, list())
})

test_that("cell runner: cell-level data.frame, and cluster aggregation with ident", {
    df <- data.frame(
        Cell = colnames(obj), T1 = "CT1", T2 = "CT2", stringsAsFactors = FALSE
    )
    f <- tempfile(fileext = ".tsv")
    write.table(df, f, sep = "\t", quote = FALSE, row.names = FALSE)

    rec <- RunCellTypeAnnotation(obj, "cell", args = list(cell_types = paste0(f, "#1,2,3")))
    expect_equal(rec$type, "cell")
    expect_equal(nrow(rec$mapping), ncol(obj))
    expect_equal(colnames(rec$mapping), c("T1", "T2"))
    expect_true(all(rec$mapping$T1 == "CT1"))
    expect_null(rec$cells)

    rec2 <- RunCellTypeAnnotation(
        obj, "cell", args = list(cell_types = paste0(f, "#1,2,3")), ident = "clusters"
    )
    expect_equal(rec2$type, "cluster")
    expect_setequal(names(rec2$mapping), c("g1", "g2"))
    expect_true(all(unlist(rec2$mapping) == "CT1"))
    expect_equal(nrow(rec2$cells), ncol(obj))
})

test_that("engine: object can be a path, and errors are loud", {
    p <- tempfile(fileext = ".rds")
    saveRDS(obj, p)
    rec <- RunCellTypeAnnotation(
        p, "direct", args = list(cell_types = list(g1 = "T", g2 = "U")),
        ident = "clusters", cache = tempfile("cta-cache-")
    )
    expect_equal(rec$mapping$g1, "T")

    p2 <- tempfile(fileext = ".qs")
    save_obj(obj, p2)
    rec2 <- RunCellTypeAnnotation(
        p2, "direct", args = list(cell_types = list(g1 = "T", g2 = "U")),
        ident = "clusters", cache = tempfile("cta-cache-")
    )
    expect_equal(rec2$mapping$g2, "U")

    p3 <- tempfile(fileext = ".rds")
    saveRDS(1:10, p3)
    expect_error(
        RunCellTypeAnnotation(p3, "direct", args = list(cell_types = list()), ident = "clusters"),
        "must be a Seurat object"
    )
    expect_error(RunCellTypeAnnotation(obj, "sctype"), "needs `ident`")
    expect_error(RunCellTypeAnnotation(obj, "nope"), "Unknown cell type annotation tool")
})

test_that("the h5ad conversion is cached (skipped without the python toolchain)", {
    cache <- tempfile("cta-cache-")
    dir.create(cache)
    conv <- function() {
        r <- tryCatch(
            .cta_h5ad(obj, "celltypist", list(assay = NULL), cache, get_logger()),
            error = function(e) e
        )
        if (inherits(r, "error")) {
            skip(paste("h5ad conversion unavailable:", conditionMessage(r)))
        }
        r
    }
    p1 <- conv()
    p2 <- conv()
    expect_equal(p1, p2)
    expect_true(file.exists(p1))
})
