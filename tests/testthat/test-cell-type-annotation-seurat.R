# Tests for EmbedSeuratCellTypeAnnotation().
# pbmc_small: 80 cells, `groups` is a character column holding g2 and g1.
# `groups` is NOT the identity column here (Idents() is 0/1/2, matching
# RNA_snn_res.1), so using it as `ident` leaves Idents() untouched inside
# RenameSeuratIdents() and keeps the set_ident assertions unambiguous.

obj <- SeuratObject::pbmc_small

# Cell-level TSV: Cell + T1 + T2, of which the `cell` tool keeps T1 and T2
cell_tsv <- function() {
    df <- data.frame(
        Cell = colnames(obj), T1 = "CT1", T2 = "CT2", stringsAsFactors = FALSE
    )
    f <- tempfile(fileext = ".tsv")
    write.table(df, f, sep = "\t", quote = FALSE, row.names = FALSE)
    f
}

test_that("cluster-level record: values, level order and identity", {
    rec <- RunCellTypeAnnotation(
        obj, "direct",
        args = list(cell_types = list(g1 = "T", g2 = "B")), ident = "groups"
    )
    out <- EmbedSeuratCellTypeAnnotation(obj, rec, ident = "groups")

    expect_true("CellType" %in% colnames(out@meta.data))
    expect_equal(levels(out$CellType), c("T", "B"))
    expect_equal(
        as.character(out$CellType),
        ifelse(as.character(obj$groups) == "g1", "T", "B")
    )
    expect_equal(as.character(Idents(out)), as.character(out$CellType))
    # the cluster column is kept, only the annotation is added
    expect_true("groups" %in% colnames(out@meta.data))
})

test_that("set_ident = FALSE keeps the original identity", {
    rec <- RunCellTypeAnnotation(
        obj, "direct",
        args = list(cell_types = list(g1 = "T", g2 = "B")), ident = "groups"
    )
    out <- EmbedSeuratCellTypeAnnotation(
        obj, rec, ident = "groups", anno_col = "MyType", set_ident = FALSE
    )

    expect_true("MyType" %in% colnames(out@meta.data))
    expect_false("CellType" %in% colnames(out@meta.data))
    expect_equal(as.character(Idents(out)), as.character(Idents(obj)))
})

test_that("merge controls whether equal cell types are merged", {
    rec <- RunCellTypeAnnotation(
        obj, "direct", args = list(cell_types = list(g1 = "T", g2 = "T")),
        ident = "groups"
    )
    out1 <- EmbedSeuratCellTypeAnnotation(obj, rec, ident = "groups")
    expect_equal(levels(out1$CellType), c("T.1", "T.2"))

    out2 <- EmbedSeuratCellTypeAnnotation(obj, rec, ident = "groups", merge = TRUE)
    expect_equal(levels(out2$CellType), "T")
})

test_that("`case` and `add_prefix` name the new columns", {
    rec <- RunCellTypeAnnotation(
        obj, "direct",
        args = list(
            cell_types = list(g1 = "T", g2 = "B"),
            more_cell_types = list(second = list(g1 = "X", g2 = "Y"))
        ),
        ident = "groups"
    )

    out <- EmbedSeuratCellTypeAnnotation(obj, rec, case = "c1", ident = "groups")
    expect_true(all(c("c1_CellType", "c1_second") %in% colnames(out@meta.data)))
    expect_false(any(c("CellType", "second") %in% colnames(out@meta.data)))
    expect_equal(
        unname(as.character(out$c1_second)),
        ifelse(as.character(obj$groups) == "g1", "X", "Y")
    )
    expect_equal(as.character(Idents(out)), as.character(out$c1_CellType))

    out2 <- EmbedSeuratCellTypeAnnotation(
        obj, rec, case = "c1", ident = "groups", add_prefix = FALSE
    )
    expect_true("CellType" %in% colnames(out2@meta.data))
})

test_that("both-mode record: per-cell labels and the aggregated annotation", {
    f <- cell_tsv()
    rec <- RunCellTypeAnnotation(
        obj, "cell", args = list(cell_types = paste0(f, "#1,2,3")), ident = "groups"
    )
    expect_equal(rec$type, "cluster")

    # merge = TRUE: both clusters are CT1, so without it they would get the
    # `CT1.1`/`CT1.2` suffixes
    out <- EmbedSeuratCellTypeAnnotation(
        obj, rec, case = "c1", ident = "groups", merge = TRUE
    )
    expect_true(all(c("c1_T1", "c1_T2", "c1_CellType") %in% colnames(out@meta.data)))
    expect_equal(unique(as.character(out$c1_T1)), "CT1")
    expect_equal(unique(as.character(out$c1_CellType)), "CT1")
})

test_that("cell-level record: the annotation column is renamed to anno_col", {
    f <- cell_tsv()
    rec <- RunCellTypeAnnotation(obj, "cell", args = list(cell_types = paste0(f, "#1,2,3")))
    expect_equal(rec$type, "cell")

    out <- EmbedSeuratCellTypeAnnotation(obj, rec)
    expect_true(all(c("CellType", "T2") %in% colnames(out@meta.data)))
    expect_false("T1" %in% colnames(out@meta.data))
    expect_equal(unique(as.character(out$CellType)), "CT1")

    out2 <- EmbedSeuratCellTypeAnnotation(obj, rec, case = "c1")
    expect_true(all(c("c1_CellType", "c1_T2") %in% colnames(out2@meta.data)))
    expect_false(any(c("T1", "c1_T1") %in% colnames(out2@meta.data)))
})

test_that("records can be applied one after another, and overwrite their columns", {
    rec1 <- RunCellTypeAnnotation(
        obj, "direct", args = list(cell_types = list(g1 = "T", g2 = "B")),
        ident = "groups"
    )
    rec2 <- RunCellTypeAnnotation(
        obj, "direct", args = list(cell_types = list(g1 = "U", g2 = "V")),
        ident = "groups"
    )

    out <- EmbedSeuratCellTypeAnnotation(obj, rec1, case = "c1", ident = "groups")
    out <- EmbedSeuratCellTypeAnnotation(out, rec2, case = "c2", ident = "groups")
    expect_true(all(c("c1_CellType", "c2_CellType") %in% colnames(out@meta.data)))
    expect_setequal(as.character(out$c2_CellType), c("U", "V"))

    # re-applying the same case replaces the column instead of duplicating it
    out <- EmbedSeuratCellTypeAnnotation(out, rec1, case = "c1", ident = "groups")
    expect_equal(sum(colnames(out@meta.data) == "c1_CellType"), 1)
    expect_setequal(as.character(out$c1_CellType), c("T", "B"))
})

test_that("EmbedSeuratCellTypeAnnotation validates its input", {
    rec <- RunCellTypeAnnotation(
        obj, "direct", args = list(cell_types = list(g1 = "T", g2 = "B")),
        ident = "groups"
    )

    expect_error(
        EmbedSeuratCellTypeAnnotation(1:10, rec), "must be a Seurat object"
    )
    expect_error(
        EmbedSeuratCellTypeAnnotation(obj, list(type = "nope")), "`type` = 'cluster' or 'cell'"
    )
    expect_error(
        EmbedSeuratCellTypeAnnotation(obj, rec, anno_col = NULL), "`anno_col`"
    )
    expect_error(
        EmbedSeuratCellTypeAnnotation(obj, rec, ident = "nope"), "Cannot determine the column"
    )
})
