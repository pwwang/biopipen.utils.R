# Tests for the two LLM annotators (mllmcelltype, lict).
#
# No provider key is set in CI, so these tests cover what runs without one:
# the marker table the two runners build, the prompt mLLMCelltype produces, and
# the loud failures both runners give instead of returning a placeholder label.
# The key-gated tests at the bottom are skipped here and run where a key exists.

obj <- SeuratObject::pbmc_small
obj$clusters <- factor(as.character(obj$groups), levels = c("g1", "g2"))

# the markers both runners feed their tool: RunSeuratDEAnalysis() with the
# cluster column renamed to `cluster`
llm_markers <- function() {
    markers <- RunSeuratDEAnalysis(obj, group_by = "clusters")
    names(markers)[names(markers) == "clusters"] <- "cluster"
    markers
}

test_that("mllmcelltype: the runner stops rather than return a prompt", {
    err <- tryCatch(
        RunCellTypeAnnotation(
            obj, "mllmcelltype",
            args = list(tissue = "human PBMC", api_key = NA),
            ident = "clusters"
        ),
        error = function(e) conditionMessage(e)
    )
    expect_true(grepl("returned a prompt, not an annotation", err))
    expect_true(grepl("OPENAI_API_KEY", err))
})

test_that("mllmcelltype: api_key = NA makes the tool build the prompt", {
    markers <- llm_markers()
    prompt <- mLLMCelltype::annotate_cell_types(
        markers, tissue_name = "human PBMC", api_key = NA
    )
    expect_type(prompt, "character")
    expect_length(prompt, 1)
    # the prompt holds the markers of every cluster, top gene included
    expect_true(grepl("g1:", prompt, fixed = TRUE))
    expect_true(grepl("g2:", prompt, fixed = TRUE))
    top_gene <- markers$gene[which.max(markers$avg_log2FC)]
    expect_true(grepl(top_gene, prompt, fixed = TRUE))

    message(
        "mLLMCelltype prompt (first 200 characters):\n",
        substr(prompt, 1, 200)
    )
})

test_that("mllmcelltype: `tissue` is required", {
    expect_error(
        RunCellTypeAnnotation(obj, "mllmcelltype", args = list(), ident = "clusters"),
        "tissue"
    )
    expect_error(
        RunCellTypeAnnotation(
            obj, "mllmcelltype", args = list(tissue = ""), ident = "clusters"
        ),
        "tissue"
    )
})

test_that("lict: the runner names the missing provider key variables", {
    key_vars <- c(
        "openai.api_key", "openai_api_key", "Gemini_api_key",
        "ANTHROPIC_API_KEY", "ERNIE_api_key", "Llama3_api_key"
    )
    skip_if(
        any(nzchar(Sys.getenv(key_vars))),
        "provider keys are set in this environment"
    )

    err <- tryCatch(
        RunCellTypeAnnotation(
            obj, "lict", args = list(species = "Human"), ident = "clusters"
        ),
        error = function(e) conditionMessage(e)
    )
    # LICT skips every provider without a key and returns "error"
    expect_true(grepl("no provider API key", err))
    expect_true(grepl("openai_api_key", err))
    expect_true(grepl("Gemini_api_key", err))
    expect_true(grepl("ANTHROPIC_API_KEY", err))
})

test_that("both LLM annotators are registered as cluster-level tools", {
    tools <- celltype_annotation_tools()
    expect_true(all(c("mllmcelltype", "lict") %in% names(tools)))
    for (tool in c("mllmcelltype", "lict")) {
        expect_equal(tools[[tool]]$level, "cluster")
        expect_false(isTRUE(tools[[tool]]$h5ad))
    }
})

test_that("mllmcelltype: a real run labels the clusters (needs OPENAI_API_KEY)", {
    skip_if(Sys.getenv("OPENAI_API_KEY") == "", "no OPENAI_API_KEY set")

    rec <- RunCellTypeAnnotation(
        obj, "mllmcelltype",
        args = list(tissue = "human PBMC"),
        ident = "clusters"
    )
    expect_equal(rec$type, "cluster")
    expect_setequal(names(rec$mapping), c("g1", "g2"))
    expect_true(all(nzchar(unlist(rec$mapping))))
})

test_that("lict: a real run labels the clusters (needs openai_api_key)", {
    skip_if(Sys.getenv("openai_api_key") == "", "no openai_api_key set")

    rec <- RunCellTypeAnnotation(
        obj, "lict", args = list(species = "Human"), ident = "clusters"
    )
    expect_equal(rec$type, "cluster")
    expect_setequal(names(rec$mapping), c("g1", "g2"))
    expect_true(all(nzchar(unlist(rec$mapping))))
})
