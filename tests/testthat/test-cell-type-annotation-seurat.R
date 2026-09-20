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

test_that("RunSeuratCellTypeAnnotation is the two calls in one", {
    direct_args <- list(cell_types = list(g1 = "T", g2 = "B"))

    rec <- RunCellTypeAnnotation(obj, "direct", args = direct_args, ident = "groups")
    manual <- EmbedSeuratCellTypeAnnotation(obj, rec, ident = "groups")

    out <- RunSeuratCellTypeAnnotation(obj, "direct", args = direct_args, ident = "groups")

    expect_setequal(colnames(out@meta.data), colnames(manual@meta.data))
    expect_equal(as.character(out$CellType), as.character(manual$CellType))
    expect_equal(levels(out$CellType), levels(manual$CellType))
    expect_equal(as.character(Idents(out)), as.character(Idents(manual)))

    # provenance, as the other Run* entry points of the package
    expect_true("RunSeuratCellTypeAnnotation" %in% names(out@commands))
})

test_that("RunSeuratCellTypeAnnotation composes over cases", {
    out <- RunSeuratCellTypeAnnotation(
        obj, "direct", args = list(cell_types = list(g1 = "T", g2 = "B")),
        ident = "groups", case = "c1"
    )
    out <- RunSeuratCellTypeAnnotation(
        out, "direct", args = list(cell_types = list(g1 = "U", g2 = "V")),
        ident = "groups", case = "c2"
    )

    expect_true(all(c("c1_CellType", "c2_CellType") %in% colnames(out@meta.data)))
    expect_setequal(as.character(out$c2_CellType), c("U", "V"))
    expect_equal(as.character(Idents(out)), as.character(out$c2_CellType))
})

test_that("RunSeuratCellTypeAnnotation resolves `ident` like the process", {
    direct_args <- list(cell_types = list(g1 = "T", g2 = "B"))

    # "ident" is an alias for the identity column, which on pbmc_small is
    # RNA_snn_res.1 (the column matching Idents(), not `groups`)
    alias <- RunSeuratCellTypeAnnotation(
        obj, "direct", args = direct_args, ident = "ident", set_ident = FALSE
    )
    expect_true("CellType" %in% colnames(alias@meta.data))

    # a cluster-level tool with no ident falls back to the identity column too
    fallback <- RunSeuratCellTypeAnnotation(
        obj, "direct", args = direct_args, set_ident = FALSE
    )
    expect_equal(as.character(fallback$CellType), as.character(alias$CellType))

    # ...while the cell-level tool stays cell-level: the annotation is the
    # tool's first column, not an aggregation over clusters
    f <- cell_tsv()
    cell_only <- RunSeuratCellTypeAnnotation(
        obj, "cell", args = list(cell_types = paste0(f, "#1,2,3"))
    )
    expect_true(all(c("CellType", "T2") %in% colnames(cell_only@meta.data)))
    expect_equal(unique(as.character(cell_only$CellType)), "CT1")
})

test_that("RunSeuratCellTypeAnnotation keys celltypist on over_clustering", {
    # celltypist is not runnable here, so the two calls are stubbed to capture
    # the `ident` the wrapper hands to the embedding
    run_with <- NULL
    embedded_with <- NULL
    local_mocked_bindings(
        RunCellTypeAnnotation = function(object, tool, args = list(), ident = NULL, ...) {
            run_with <<- ident
            list(mapping = list(g1 = "T", g2 = "B"), type = "cluster", more = NULL)
        },
        EmbedSeuratCellTypeAnnotation = function(object, record, ident = NULL, ...) {
            embedded_with <<- ident
            object
        },
        .package = "biopipen.utils"
    )
    cta <- function(...) RunSeuratCellTypeAnnotation(obj, "celltypist", ..., set_ident = FALSE)

    # an explicit `ident`: the tool is run on it, the mapping is keyed on
    # over_clustering
    cta(args = list(over_clustering = "groups", model = "x.pkl"), ident = "groups")
    expect_equal(run_with, "groups")
    expect_equal(embedded_with, "groups")

    # no `ident`: the identity column is resolved for the run, and the mapping is
    # still keyed on over_clustering
    cta(args = list(over_clustering = "over_clusters", model = "x.pkl"))
    expect_equal(run_with, "RNA_snn_res.1")
    expect_equal(embedded_with, "over_clusters")

    # over_clustering = FALSE is not a key: without an `ident` the call is not
    # cluster-based at all
    cta(args = list(over_clustering = FALSE, model = "x.pkl"))
    expect_null(run_with)
    expect_null(embedded_with)
})

test_that("RunSeuratCellTypeAnnotation validates its input", {
    direct_args <- list(cell_types = list(g1 = "T", g2 = "B"))

    # a path is rejected before the tool runs
    expect_error(
        RunSeuratCellTypeAnnotation("obj.rds", "direct", args = direct_args),
        "`object` must be a Seurat object"
    )
    expect_error(
        RunSeuratCellTypeAnnotation(obj, c("direct", "cell")), "`tool` must be a single tool name"
    )
    expect_error(
        RunSeuratCellTypeAnnotation(obj, "nope"), "Unknown cell type annotation tool"
    )
    # the tool name is case insensitive
    expect_equal(
        as.character(RunSeuratCellTypeAnnotation(
            obj, "DIRECT", args = direct_args, ident = "groups"
        )$CellType),
        as.character(RunSeuratCellTypeAnnotation(
            obj, "direct", args = direct_args, ident = "groups"
        )$CellType)
    )
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
