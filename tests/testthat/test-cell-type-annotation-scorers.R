# Smoke tests for the equal-weighting scorer runners (ucell/aucell/gsva/
# singscore). pbmc_small: 80 cells, 230 genes, metadata `groups` (g1, g2).

norm_obj <- function() {
    obj <- SeuratObject::pbmc_small
    suppressWarnings(Seurat::NormalizeData(obj, verbose = FALSE))
}

# `n` positive markers per cell type; with `negative = TRUE` the last marker of
# "A" becomes a negative-direction marker
marker_df <- function(obj, n = 5, negative = FALSE) {
    genes <- rownames(obj)
    df <- data.frame(
        cell_type = rep(c("A", "B"), each = n),
        gene = c(genes[1:n], genes[(n + 1):(2 * n)]),
        stringsAsFactors = FALSE
    )
    if (negative) {
        df$direction <- c(rep("positive", n - 1), "negative", rep("positive", n))
    }
    df
}

marker_tsv <- function(df) {
    f <- tempfile(fileext = ".tsv")
    write.table(df, f, sep = "\t", quote = FALSE, row.names = FALSE)
    f
}

# 20 cells over 200 genes, where genes 1:5 are only expressed in the first half
# and genes 6:10 only in the second half, so the expected labels are known
split_obj <- function() {
    set.seed(42)
    counts <- matrix(rpois(200 * 20, 1), nrow = 200, ncol = 20)
    rownames(counts) <- paste0("G", 1:200)
    colnames(counts) <- paste0("C", 1:20)
    counts[1:5, 1:10] <- counts[1:5, 1:10] + 50
    counts[6:10, 11:20] <- counts[6:10, 11:20] + 50
    obj <- suppressWarnings(Seurat::NormalizeData(
        SeuratObject::CreateSeuratObject(counts = counts), verbose = FALSE
    ))
    obj$grp <- rep(c("x", "y"), each = 10)
    obj
}

test_that("the scorers pick the argmax signature of the right cell", {
    # Guards the cell/signature alignment of each runner's argmax: on this
    # object every cell must get the label of the half it belongs to
    obj <- split_obj()
    db <- marker_tsv(marker_df(obj, n = 5))
    expected <- rep(c("A", "B"), each = 10)

    for (tool in c("ucell", "aucell", "gsva", "singscore")) {
        pkg <- c(
            ucell = "UCell", aucell = "AUCell", gsva = "GSVA",
            singscore = "singscore"
        )[[tool]]
        skip_if_not_installed(pkg)
        rec <- suppressWarnings(RunCellTypeAnnotation(
            obj, tool, args = list(db = db), cache = tempfile("cta-")
        ))
        expect_equal(rec$mapping[[1]], expected, info = tool)
        rec2 <- suppressWarnings(RunCellTypeAnnotation(
            obj, tool, args = list(db = db), ident = "grp",
            cache = tempfile("cta-")
        ))
        expect_equal(rec2$mapping, list(x = "A", y = "B"), info = tool)
    }
})

test_that("ucell runner: cell-level, both-mode and direction handling", {
    skip_if_not_installed("UCell")
    obj <- norm_obj()
    db <- marker_tsv(marker_df(obj))

    rec <- RunCellTypeAnnotation(
        obj, "ucell", args = list(db = db), cache = tempfile("cta-")
    )
    expect_equal(rec$type, "cell")
    expect_equal(nrow(rec$mapping), ncol(obj))
    expect_true("ucell_celltype" %in% colnames(rec$mapping))
    expect_true(all(rec$mapping$ucell_celltype %in% c("A", "B")))

    rec2 <- RunCellTypeAnnotation(
        obj, "ucell", args = list(db = db), ident = "groups",
        cache = tempfile("cta-")
    )
    expect_equal(rec2$type, "cluster")
    expect_setequal(names(rec2$mapping), c("g1", "g2"))
    expect_equal(nrow(rec2$cells), ncol(obj))

    # the negative marker is kept, as UCell's `-` gene-name suffix
    df_neg <- marker_df(obj, negative = TRUE)
    neg_gene <- df_neg$gene[df_neg$direction == "negative"]
    sets <- markers_to_ucell_list(df_neg)
    expect_true(paste0(neg_gene, "-") %in% sets$A)
    expect_false(paste0(neg_gene, "-") %in% sets$B)

    rec3 <- RunCellTypeAnnotation(
        obj, "ucell", args = list(db = marker_tsv(df_neg)), cache = tempfile("cta-")
    )
    expect_equal(nrow(rec3$mapping), ncol(obj))
    expect_true(all(rec3$mapping$ucell_celltype %in% c("A", "B")))
})

test_that("aucell runner: cell-level, both-mode and dropped negative markers", {
    skip_if_not_installed("AUCell")
    obj <- norm_obj()
    db <- marker_tsv(marker_df(obj))

    # with the default aucMaxRank (5% of 230 genes), AUCell warns that only the
    # top 12 genes are used
    rec <- suppressWarnings(RunCellTypeAnnotation(
        obj, "aucell", args = list(db = db), cache = tempfile("cta-")
    ))
    expect_equal(rec$type, "cell")
    expect_equal(nrow(rec$mapping), ncol(obj))
    expect_true("aucell_celltype" %in% colnames(rec$mapping))
    expect_true(all(rec$mapping$aucell_celltype %in% c("A", "B")))

    rec2 <- suppressWarnings(RunCellTypeAnnotation(
        obj, "aucell", args = list(db = db), ident = "groups",
        cache = tempfile("cta-")
    ))
    expect_equal(rec2$type, "cluster")
    expect_setequal(names(rec2$mapping), c("g1", "g2"))
    expect_equal(nrow(rec2$cells), ncol(obj))

    # a negative-direction marker does not stop AUCell: it is dropped, and the
    # gene sets stay positive-only
    df_neg <- marker_df(obj, negative = TRUE)
    neg_gene <- df_neg$gene[df_neg$direction == "negative"]
    expect_false(neg_gene %in% markers_to_named_list(df_neg)$A)

    rec3 <- suppressWarnings(RunCellTypeAnnotation(
        obj, "aucell", args = list(db = marker_tsv(df_neg)), cache = tempfile("cta-")
    ))
    expect_equal(nrow(rec3$mapping), ncol(obj))
    expect_true(all(rec3$mapping$aucell_celltype %in% c("A", "B")))
})

test_that("gsva runner: cell-level, both-mode and dropped negative markers", {
    skip_if_not_installed("GSVA")
    obj <- norm_obj()
    db <- marker_tsv(marker_df(obj))

    rec <- RunCellTypeAnnotation(
        obj, "gsva", args = list(db = db), cache = tempfile("cta-")
    )
    expect_equal(rec$type, "cell")
    expect_equal(nrow(rec$mapping), ncol(obj))
    expect_true("gsva_celltype" %in% colnames(rec$mapping))
    expect_true(all(rec$mapping$gsva_celltype %in% c("A", "B")))

    rec2 <- RunCellTypeAnnotation(
        obj, "gsva", args = list(db = db), ident = "groups",
        cache = tempfile("cta-")
    )
    expect_equal(rec2$type, "cluster")
    expect_setequal(names(rec2$mapping), c("g1", "g2"))
    expect_equal(nrow(rec2$cells), ncol(obj))

    # a negative-direction marker does not stop GSVA: it is dropped
    df_neg <- marker_df(obj, negative = TRUE)
    rec3 <- RunCellTypeAnnotation(
        obj, "gsva", args = list(db = marker_tsv(df_neg)), cache = tempfile("cta-")
    )
    expect_equal(nrow(rec3$mapping), ncol(obj))
    expect_true(all(rec3$mapping$gsva_celltype %in% c("A", "B")))
})

test_that("singscore runner: cell-level, both-mode and native down sets", {
    skip_if_not_installed("singscore")
    obj <- norm_obj()
    db <- marker_tsv(marker_df(obj))

    rec <- RunCellTypeAnnotation(
        obj, "singscore", args = list(db = db), cache = tempfile("cta-")
    )
    expect_equal(rec$type, "cell")
    expect_equal(nrow(rec$mapping), ncol(obj))
    expect_true("singscore_celltype" %in% colnames(rec$mapping))
    expect_true(all(rec$mapping$singscore_celltype %in% c("A", "B")))

    rec2 <- RunCellTypeAnnotation(
        obj, "singscore", args = list(db = db), ident = "groups",
        cache = tempfile("cta-")
    )
    expect_equal(rec2$type, "cluster")
    expect_setequal(names(rec2$mapping), c("g1", "g2"))
    expect_equal(nrow(rec2$cells), ncol(obj))

    # the negative marker goes to the `down` slot of its cell type
    df_neg <- marker_df(obj, negative = TRUE)
    neg_gene <- df_neg$gene[df_neg$direction == "negative"]
    sets <- markers_to_singscore_list(df_neg)
    expect_true(neg_gene %in% sets$A$down)
    expect_false(neg_gene %in% sets$A$up)
    expect_length(sets$B$down, 0)

    rec3 <- RunCellTypeAnnotation(
        obj, "singscore", args = list(db = marker_tsv(df_neg)),
        cache = tempfile("cta-")
    )
    expect_equal(nrow(rec3$mapping), ncol(obj))
    expect_true(all(rec3$mapping$singscore_celltype %in% c("A", "B")))
})
