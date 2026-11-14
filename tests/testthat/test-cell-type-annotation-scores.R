# Per-cell scores reported by RunCellTypeAnnotation(). Tools that compute a
# score for each candidate label return `scores`, one `<tool>.score` value per
# cell: the score of the label the tool assigned to that cell. The unit tests
# pin `.cta_label_scores()` (both matrix orientations, missing labels and
# non-finite values); the runner tests pin the contract for an R scorer
# (aucell), a transposed R scorer (gsva), a tool without scores (direct) and
# the python wrapper forwards (scsa/maca/scbert), whose wrappers are emulated
# by a fake python so no tool has to be installed.
# pbmc_small: 80 cells, 230 genes, metadata `groups` (g1, g2).

norm_obj <- function() {
    obj <- SeuratObject::pbmc_small
    suppressWarnings(Seurat::NormalizeData(obj, verbose = FALSE))
}

marker_df <- function(obj, n = 5) {
    genes <- rownames(obj)
    data.frame(
        cell_type = rep(c("A", "B"), each = n),
        gene = c(genes[1:n], genes[(n + 1):(2 * n)]),
        stringsAsFactors = FALSE
    )
}

marker_tsv <- function(df) {
    f <- tempfile(fileext = ".tsv")
    write.table(df, f, sep = "\t", quote = FALSE, row.names = FALSE)
    f
}

cta_label_scores <- function() {
    getFromNamespace(".cta_label_scores", "biopipen.utils")
}

test_that(".cta_label_scores() picks each cell's assigned label", {
    f <- cta_label_scores()
    # cells x candidates
    scores <- matrix(
        c(1, 2, 3, 4, 5, 6), nrow = 3,
        dimnames = list(c("c1", "c2", "c3"), c("A", "B"))
    )
    labels <- c("A", "B", "A")

    rec <- f(scores, labels, "tool")
    expect_s3_class(rec, "data.frame")
    expect_named(rec, "tool.score")
    expect_identical(rownames(rec), c("c1", "c2", "c3"))
    expect_equal(rec[["tool.score"]], c(1, 5, 3))

    # candidates x cells is detected by the label count and transposed back
    expect_equal(f(t(scores), labels, "tool"), rec)

    # a label that is not a column of the matrix gives NA for that cell
    expect_equal(
        f(scores, c("A", "nope", "A"), "tool")[["tool.score"]],
        c(1, NA, 3)
    )

    # Inf/NaN are reported as NA; finite values are left alone
    bad <- scores
    bad[1, "A"] <- -Inf
    bad[2, "B"] <- 7
    bad[3, "A"] <- NaN
    expect_equal(f(bad, labels, "tool")[["tool.score"]], c(NA, 7, NA))
})

test_that("aucell scores the label it assigned to each cell", {
    skip_if_not_installed("AUCell")
    obj <- norm_obj()
    db <- marker_tsv(marker_df(obj))
    cache <- tempfile("cta-")

    # AUCell breaks ranking ties randomly, so seed the runner and the
    # recomputation below identically to get the very same AUC matrix
    set.seed(20240101)
    rec <- suppressWarnings(suppressMessages(RunCellTypeAnnotation(
        obj, "aucell", args = list(db = db), cache = cache
    )))
    set.seed(20240101)
    rec2 <- suppressWarnings(suppressMessages(RunCellTypeAnnotation(
        obj, "aucell", args = list(db = db), ident = "groups", cache = cache
    )))

    expect_equal(rec$type, "cell")
    expect_s3_class(rec$scores, "data.frame")
    expect_named(rec$scores, "aucell.score")
    expect_identical(rownames(rec$scores), colnames(obj))
    expect_identical(
        rec$mapping$aucell_celltype,
        rec2$cells$aucell_celltype
    )
    expect_equal(rec2$scores, rec$scores)
    expect_gt(sum(!is.na(rec$scores[["aucell.score"]])), 0)

    # Recompute AUCell's cells x sets matrix the way the runner does, so the
    # reported value can be checked against the AUC of the assigned label (and
    # the label against the argmax), not just against its range
    sets <- markers_to_named_list(load_marker_table(db))
    expr <- as.matrix(GetAssayData(obj, assay = "RNA", layer = "data"))
    set.seed(20240101)
    rankings <- suppressWarnings(AUCell::AUCell_buildRankings(
        expr, plotStats = FALSE, splitByBlocks = TRUE
    ))
    auc <- suppressWarnings(AUCell::AUCell_calcAUC(
        sets, rankings,
        aucMaxRank = ceiling(0.05 * nrow(expr)), normAUC = TRUE
    ))
    auc_cells <- t(AUCell::getAUC(auc))
    labels <- rec$mapping$aucell_celltype

    expect_equal(
        rec$scores[["aucell.score"]],
        unname(auc_cells[cbind(
            seq_along(labels), match(labels, colnames(auc_cells))
        )])
    )
    expect_true(all(rec$scores[["aucell.score"]] >= 0))
    expect_identical(
        labels,
        colnames(auc_cells)[max.col(auc_cells, ties.method = "first")]
    )
})

test_that("gsva scores the label it assigned to each cell", {
    skip_if_not_installed("GSVA")
    obj <- norm_obj()
    db <- marker_tsv(marker_df(obj))
    cache <- tempfile("cta-")

    rec <- suppressWarnings(suppressMessages(RunCellTypeAnnotation(
        obj, "gsva", args = list(db = db), cache = cache
    )))
    rec2 <- suppressWarnings(suppressMessages(RunCellTypeAnnotation(
        obj, "gsva", args = list(db = db), ident = "groups", cache = cache
    )))

    expect_equal(rec$type, "cell")
    expect_s3_class(rec$scores, "data.frame")
    expect_named(rec$scores, "gsva.score")
    expect_identical(rownames(rec$scores), colnames(obj))
    expect_equal(nrow(rec$scores), ncol(obj))
    expect_true(is.numeric(rec$scores[["gsva.score"]]))
    expect_true(all(is.finite(rec$scores[["gsva.score"]])))
    expect_identical(rec$mapping$gsva_celltype, rec2$cells$gsva_celltype)
    expect_equal(rec2$scores, rec$scores)

    # gsva() returns sets x cells; the recomputation also pins the transposed
    # orientation .cta_label_scores() has to handle here
    sets <- markers_to_named_list(load_marker_table(db))
    expr <- as.matrix(GetAssayData(obj, assay = "RNA", layer = "data"))
    gsva_scores <- GSVA::gsva(
        GSVA::gsvaParam(
            expr, sets, kcdf = "Gaussian", minSize = 1, maxSize = Inf
        ),
        verbose = FALSE
    )
    labels <- rec$mapping$gsva_celltype
    expect_equal(
        rec$scores[["gsva.score"]],
        unname(gsva_scores[cbind(match(labels, rownames(gsva_scores)), seq_along(labels))])
    )
    expect_identical(
        labels,
        rownames(gsva_scores)[max.col(t(gsva_scores), ties.method = "first")]
    )
})

test_that("a tool without numeric scores returns no `scores`", {
    obj <- SeuratObject::pbmc_small
    rec <- RunCellTypeAnnotation(
        obj, "direct",
        args = list(cell_types = list(g1 = "T", g2 = "B")),
        ident = "groups"
    )
    expect_false("scores" %in% names(rec))
    expect_null(rec$scores)

    # `direct` is registered as a cluster-level tool, so the public API rejects
    # ident = NULL before a record exists: there is no ident-less record whose
    # missing `scores` could be asserted, and that rejection is the contract.
    expect_error(
        RunCellTypeAnnotation(
            obj, "direct",
            args = list(cell_types = list(g1 = "T", g2 = "B")),
            ident = NULL
        ),
        "cluster-level and needs `ident`",
        fixed = TRUE
    )
})

# ---- python wrapper forwards -------------------------------------------------

# A stand-in for the python the wrappers run with: `get_biopipen_dir()` asks it
# for the biopipen install dir with `-c`, and every other call is the wrapper
# invocation, where it copies the fixture TSV to the `-o` path. That is all the
# R side reads back, so scsa/maca/scbert can be exercised here without the
# tools (or an h5ad conversion) being installed.
fake_python <- function(dir, biopipen_dir, fixture) {
    script <- file.path(dir, "python")
    writeLines(c(
        "#!/bin/sh",
        "if [ \"$1\" = \"-c\" ]; then",
        paste0("  printf '%s\\n' \"", biopipen_dir, "\""),
        "  exit 0",
        "fi",
        "out=\"\"",
        "while [ $# -gt 0 ]; do",
        "  if [ \"$1\" = \"-o\" ]; then out=\"$2\"; shift; fi",
        "  shift",
        "done",
        paste0("if [ -n \"$out\" ]; then cp \"", fixture, "\" \"$out\"; fi")
    ), script)
    Sys.chmod(script, "0755")
    script
}

wrapper_fixture <- function(dir, tool, cells, with_score = TRUE) {
    df <- data.frame(
        cell = cells,
        celltype = c("A", "B", "A"),
        stringsAsFactors = FALSE
    )
    names(df)[2] <- paste0(tool, "_celltype")
    if (with_score) {
        # NaN mirrors the wrappers' unannotated cell
        df[[paste0(tool, "_score")]] <- c(0.5, NaN, 0.25)
    }
    file <- file.path(
        dir, paste0(tool, if (with_score) "" else "-noscore", ".tsv")
    )
    write.table(df, file, sep = "\t", quote = FALSE, row.names = FALSE)
    file
}

run_wrapper <- function(obj, tool, ident, with_score, work, db) {
    cells <- colnames(obj)[1:3]
    fixture <- wrapper_fixture(work, tool, cells, with_score)
    python <- fake_python(work, file.path(work, "biopipen"), fixture)
    args <- list(db = db, python = python)
    if (identical(tool, "scbert")) {
        model <- file.path(work, "model.ckpt")
        label_dict <- file.path(work, "label_dict.json")
        invisible(file.create(model, label_dict))
        args <- c(args, list(ref = "ref", model = model, label_dict = label_dict))
    }
    tag <- paste(
        tool, if (is.null(ident)) "noident" else ident,
        if (with_score) "score" else "noscore",
        sep = "-"
    )
    scratch <- file.path(work, tag)
    dir.create(scratch)
    ctx <- list(
        cache = work, h5ad = file.path(work, "unused.h5ad"), scratch = scratch
    )
    runner <- getFromNamespace(
        paste0(".run_celltypeannotation_", tool), "biopipen.utils"
    )
    suppressWarnings(suppressMessages(runner(obj, args, ident, ctx)))
}

test_that("the python wrappers forward per-cell scores without leaking them", {
    obj <- norm_obj()
    work <- tempfile("cta-python-")
    dir.create(work)
    dir.create(file.path(work, "biopipen", "scripts", "scrna"), recursive = TRUE)
    db <- marker_tsv(marker_df(obj))
    cells <- colnames(obj)[1:3]

    for (tool in c("scsa", "maca", "scbert")) {
        # scsa annotates clusters only: the public API rejects ident = NULL and
        # the runner cannot aggregate without clusters, so it is exercised in
        # cluster mode alone
        idents <- if (identical(tool, "scsa")) list("groups") else list(NULL, "groups")
        for (ident in idents) {
            info <- paste(
                tool, if (is.null(ident)) "ident=NULL" else paste0("ident=", ident)
            )

            rec <- run_wrapper(obj, tool, ident, TRUE, work, db)
            expect_true(inherits(rec$scores, "data.frame"), info = info)
            expect_named(rec$scores, paste0(tool, ".score"), info = info)
            expect_identical(rownames(rec$scores), cells, info = info)
            # including NaN -> NA for the unannotated cell
            expect_equal(rec$scores[[1]], c(0.5, NA, 0.25), info = info)

            label_table <- if (identical(rec$type, "cell")) rec$mapping else rec$cells
            expect_false(
                paste0(tool, "_score") %in% colnames(label_table), info = info
            )
            expect_identical(rownames(label_table), cells, info = info)
            expect_equal(
                label_table[[paste0(tool, "_celltype")]],
                c("A", "B", "A"),
                info = info
            )

            # an older wrapper without the score column yields no `scores` and
            # does not error
            rec_no_score <- run_wrapper(obj, tool, ident, FALSE, work, db)
            expect_null(rec_no_score$scores, info = paste(info, "(no score)"))
        }
    }

    # scsa cannot produce an ident-less record at all; the rejection is the
    # reason the score forwarding is checked in cluster mode only above
    expect_error(
        RunCellTypeAnnotation(
            obj, "scsa",
            args = list(
                db = db,
                python = fake_python(
                    work, file.path(work, "biopipen"),
                    wrapper_fixture(work, "scsa", cells)
                )
            ),
            ident = NULL
        ),
        "cluster-level and needs `ident`",
        fixed = TRUE
    )
})
