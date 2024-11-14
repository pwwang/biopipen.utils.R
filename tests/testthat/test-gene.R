test_that("gene_name_conversion", {
    genes <- c("ENSG00000157764", "ENSG00000169174", "ENSG00000169174.11")
    out <- gene_name_conversion(genes, "ensg", "symbol", species = "human")
    expect_equal(out, tibble(
        query = c("ENSG00000157764", "ENSG00000169174", "ENSG00000169174.11"),
        symbol = c("BRAF", "PCSK9", "PCSK9")
    ))
})

test_that("gene_name_conversion: notfound = 'error'", {
    genes <- c("ENSG00000157764", "ENSG00000169174", "ENSG00000169174.11", "ENSG00000000000")
    expect_error(gene_name_conversion(genes, "ensg", "symbol", species = "human", notfound = "error"))
})

test_that("gene_name_conversion: notfound = 'use-query'", {
    genes <- c("ENSG00000157764", "ENSG00000169174", "ENSG00000169174.11", "ENSG00000000000")
    out <- gene_name_conversion(genes, "ensg", "symbol", species = "human", notfound = "use-query")
    expect_equal(out, tibble(
        query = c("ENSG00000157764", "ENSG00000169174", "ENSG00000169174.11", "ENSG00000000000"),
        symbol = c("BRAF", "PCSK9", "PCSK9", "ENSG00000000000")
    ))
})

test_that("gene_name_conversion: notfound = 'skip'", {
    genes <- c("ENSG00000157764", "ENSG00000169174", "ENSG00000169174.11", "ENSG00000000000")
    out <- gene_name_conversion(genes, "ensg", "symbol", species = "human", notfound = "skip")
    expect_equal(out, tibble(
        query = c("ENSG00000157764", "ENSG00000169174", "ENSG00000169174.11"),
        symbol = c("BRAF", "PCSK9", "PCSK9")
    ))
})

test_that("gene_name_conversion: notfound = 'ignore'", {
    genes <- c("ENSG00000157764", "ENSG00000169174", "ENSG00000169174.11", "ENSG00000000000")
    out <- gene_name_conversion(genes, "ensg", "symbol", species = "human", notfound = "ignore")
    expect_equal(out, tibble(
        query = c("ENSG00000157764", "ENSG00000169174", "ENSG00000169174.11"),
        symbol = c("BRAF", "PCSK9", "PCSK9")
    ))
})

test_that("gene_name_conversion: notfound = 'na'", {
    genes <- c("ENSG00000157764", "ENSG00000169174", "ENSG00000169174.11", "ENSG00000000000")
    out <- gene_name_conversion(genes, "ensg", "symbol", species = "human", notfound = "na")
    expect_equal(out, tibble(
        query = c("ENSG00000157764", "ENSG00000169174", "ENSG00000169174.11", "ENSG00000000000"),
        symbol = c("BRAF", "PCSK9", "PCSK9", NA)
    ))

    out <- gene_name_conversion(genes, "ensg", "symbol", species = "human", notfound = NA)
    expect_equal(out, tibble(
        query = c("ENSG00000157764", "ENSG00000169174", "ENSG00000169174.11", "ENSG00000000000"),
        symbol = c("BRAF", "PCSK9", "PCSK9", NA)
    ))
})

test_that("gene_name_conversion: infmt = 'ensmusg'", {
    genes <- c("ENSMUSG00000000001", "ENSMUSG00000000003", "ENSMUSG00000000010")
    out <- gene_name_conversion(genes, "ensmusg", "symbol", species = "mouse")
    expect_equal(out, tibble(
        query = c("ENSMUSG00000000001", "ENSMUSG00000000003", "ENSMUSG00000000010"),
        symbol = c("Gnai3", "Pbsn", NA)
    ))
})

test_that("gene_name_conversion: outfmt = 'ensg'", {
    genes <- c("BRAF", "PCSK9", "PCSK9")
    out <- gene_name_conversion(genes, "symbol", "ensg", species = "human")
    expect_equal(out, tibble(
        query = c("BRAF", "PCSK9", "PCSK9"),
        ensembl.gene = c("ENSG00000157764", "ENSG00000169174", "ENSG00000169174")
    ))
})

test_that("gene_name_conversion: works with suppress_messages", {
    genes <- c("ENSG00000157764", "ENSG00000169174", "ENSG00000169174.11")
    expect_output(gene_name_conversion(genes, "ensg", "symbol", species = "human", notfound = "error", suppress_messages = FALSE))
})

test_that("gene_name_conversion: nothing found", {
    genes <- c("ENSG00000000000")
    out <- gene_name_conversion(genes, "ensg", "symbol", species = "human")
    expect_equal(out, tibble(
        query = c("ENSG00000000000"),
        symbol = c(NA_character_)
    ))
})

test_that("gene_name_conversion: works with dup = last", {
    genes <- c("ENSG00000157764", "ENSG00000169174", "ENSG00000169174.11")
    out <- gene_name_conversion(genes, "ensg", "symbol", species = "human", dup = "last")
    expect_equal(out, tibble(
        query = c("ENSG00000157764", "ENSG00000169174", "ENSG00000169174.11"),
        symbol = c("BRAF", "PCSK9", "PCSK9")
    ))
})

test_that("gene_name_conversion: works with dup = all", {
    genes <- c("ENSG00000157764", "ENSG00000169174", "ENSG00000169174.11")
    out_all <- gene_name_conversion(genes, "ensg", "symbol", species = "human", dup = "all")
    expect_equal(out_all, tibble(
        query = c("ENSG00000157764", "ENSG00000169174", "ENSG00000169174.11"),
        symbol = c("BRAF", "PCSK9", "PCSK9")
    ))
})

test_that("gene_name_conversion: works with dup = separator", {
    genes <- c("ENSG00000157764", "ENSG00000169174", "ENSG00000169174.11")
    out <- gene_name_conversion(genes, "ensg", "symbol", species = "human", dup = ",")
    expect_equal(out, tibble(
        query = c("ENSG00000157764", "ENSG00000169174", "ENSG00000169174.11"),
        symbol = c("BRAF", "PCSK9", "PCSK9")
    ))
})
