# Tests for the universal marker-table helpers and majority_vote

test_that("marker table: column canonicalisation and aliases", {
    df <- data.frame(
        celltype = c("A", "A", "B"), marker = c("G1", "G2", "G3"),
        sign = c("+", "-", "pos"), stringsAsFactors = FALSE
    )
    df <- canonicalize_marker_cols(df)
    expect_equal(colnames(df), c("cell_type", "gene", "direction"))
    expect_true(is_marker_canonical(df))
    expect_false(is_marker_canonical(data.frame(a = 1)))
    expect_equal(
        normalize_marker_direction(c("+", "neg", "Positive")),
        c("positive", "negative", "positive")
    )
    expect_error(normalize_marker_direction("maybe"), "Invalid `direction`")
})

test_that("apply_marker_filters honours species and errors loudly", {
    df <- data.frame(
        cell_type = c("A", "B"), gene = c("G1", "G2"),
        species = c("human", "mouse"), stringsAsFactors = FALSE
    )
    expect_equal(nrow(apply_marker_filters(df, species = "human")), 1)
    expect_equal(nrow(apply_marker_filters(df)), 2)
    expect_error(apply_marker_filters(df, tissue = "x"), "no `tissue` column")
    expect_error(apply_marker_filters(df, species = "zebrafish"), "No markers")
})

test_that("markers_to_* converters keep the documented semantics", {
    df <- data.frame(
        cell_type = c("A", "A", "B"), gene = c("G1", "G2", "G3"),
        direction = c("positive", "negative", "positive"),
        weight = c(1, -0.5, 2), stringsAsFactors = FALSE
    )

    sctype <- markers_to_sctype_df(df)
    expect_true(all(c("tissueType", "cellName", "geneSymbolmore1", "geneSymbolmore2") %in% colnames(sctype)))
    expect_equal(sctype$geneSymbolmore1[sctype$cellName == "A"], "G1")
    expect_equal(sctype$geneSymbolmore2[sctype$cellName == "A"], "G2")

    scsorter <- markers_to_scsorter_df(df)
    expect_equal(colnames(scsorter)[1:2], c("Type", "Marker"))
    expect_equal(scsorter$Weight, c(1, -0.5, 2))

    # only positive markers survive the named-list conversion
    expect_equal(markers_to_named_list(df), list(A = "G1", B = "G3"))

    # scCATCH keeps only positive markers and adds the subtype columns
    sccatch <- markers_to_sccatch_df(df)
    expect_equal(nrow(sccatch), 2)
    expect_true(all(c("celltype", "pmid", "subtype1", "subtype2", "subtype3") %in% colnames(sccatch)))
})

test_that("garnett marker file conversion and native detection", {
    df <- data.frame(
        cell_type = c("A", "A"), gene = c("G1", "G2"),
        direction = c("positive", "negative"), stringsAsFactors = FALSE
    )
    f <- tempfile(fileext = ".txt")
    markers_to_garnett_file(df, f)
    expect_true(is_garnett_native_marker(f))
    expect_equal(readLines(f), c("> A", "expressed: G1", "not expressed: G2"))
    expect_error(
        markers_to_garnett_file(
            data.frame(cell_type = "A", gene = "G1", direction = "negative"),
            tempfile(fileext = ".txt")
        ),
        "no positive markers"
    )
})

test_that("load_marker_table reads TSV/RDS and selects columns after #", {
    df <- data.frame(
        cell_type = c("A", "B"), gene = c("G1", "G2"), stringsAsFactors = FALSE
    )
    tsv <- tempfile(fileext = ".tsv")
    write.table(df, tsv, sep = "\t", quote = FALSE, row.names = FALSE)
    expect_equal(load_marker_table(tsv), df)

    rds <- tempfile(fileext = ".rds")
    saveRDS(df, rds)
    expect_equal(load_marker_table(rds), df)

    renamed <- df
    colnames(renamed) <- c("Type", "Marker")
    tsv2 <- tempfile(fileext = ".tsv")
    write.table(renamed, tsv2, sep = "\t", quote = FALSE, row.names = FALSE)
    sel <- load_marker_table(paste0(tsv2, "#2,1"))
    expect_equal(colnames(sel), c("gene", "cell_type"))
})

test_that("majority_vote: default and custom unknown sentinels", {
    expect_equal(
        majority_vote(c("T", "T", "B", NA, "unknown"), c("0", "0", "0", "1", "1")),
        list("0" = "T", "1" = "unknown")
    )
    expect_equal(
        majority_vote(c("Unknown", "B", "B"), c("0", "0", "0"), unknown = "Unknown"),
        list("0" = "B")
    )
})
