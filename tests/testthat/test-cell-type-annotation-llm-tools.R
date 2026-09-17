# Tests for the two LLM annotators (mllmcelltype, lict).
#
# No provider key is set in CI, so these tests cover what runs without one:
# the marker table the two runners build, the prompt mLLMCelltype produces, and
# the loud failures both runners give instead of returning a placeholder label.
# The key-gated tests at the bottom are skipped here and run where a key exists;
# LICT's refine stage also needs all five providers, and is skipped without them.
# Each tool's package is a Suggests, not a hard dependency, so the tests below
# also skip where mLLMCelltype or LICT is not installed.

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
    skip_if_not_installed("mLLMCelltype")

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
    skip_if_not_installed("mLLMCelltype")

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
    skip_if_not_installed("mLLMCelltype")

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

test_that("mllmcelltype: a host-only base URL is completed to the request URL", {
    skip_if_not_installed("mLLMCelltype")

    # mLLMCelltype posts to `base_urls` as it is given, while the other tools
    # take a host and append the path themselves: a host-only base has to be
    # completed here, or the request goes to the host root and 404s
    seen <- NULL
    local_mocked_bindings(
        annotate_cell_types = function(input, tissue_name, model, api_key,
                                       base_urls = NULL, ...) {
            seen <<- base_urls
            stats::setNames(c("A", "B"), c("g1", "g2"))
        },
        .package = "mLLMCelltype"
    )
    call <- function(base_urls) {
        RunCellTypeAnnotation(
            obj, "mllmcelltype",
            args = list(
                tissue = "human PBMC", api_key = "key", base_urls = base_urls
            ),
            ident = "clusters"
        )
    }

    # a bare host, with or without the trailing slash, and a versioned host
    call("https://api.deepseek.com")
    expect_equal(seen, "https://api.deepseek.com/v1/chat/completions")
    call("https://api.deepseek.com/")
    expect_equal(seen, "https://api.deepseek.com/v1/chat/completions")
    call("https://api.deepseek.com/v1")
    expect_equal(seen, "https://api.deepseek.com/v1/chat/completions")
    # a URL that already is the request URL is left alone
    call("https://api.openai.com/v1/chat/completions")
    expect_equal(seen, "https://api.openai.com/v1/chat/completions")
})

# the provider key variables LICT reads; `OPENAI_API_KEY` is the one the
# OpenAI-compatible endpoint variables come with
lict_key_vars <- c(
    "OPENAI_API_KEY", "openai.api_key", "openai_api_key", "Gemini_api_key",
    "ANTHROPIC_API_KEY", "ERNIE_api_key", "Llama3_api_key"
)

# The five providers LICT combines, each with the check LICT itself makes
# before querying it: ERNIE and Llama need both their variables, the
# OpenAI-compatible endpoint answers to any of its three names. The refinement
# is gated on all five answering, so the tests below never name a provider
# themselves -- they read off this table which ones the environment holds.
lict_providers <- list(
    ERNIE  = function() all(nzchar(Sys.getenv(c("ERNIE_api_key", "ERNIE_secret_key")))),
    Gemini = function() nzchar(Sys.getenv("Gemini_api_key")),
    GPT    = function() any(nzchar(Sys.getenv(c("OPENAI_API_KEY", "openai.api_key", "openai_api_key")))),
    Llama  = function() all(nzchar(Sys.getenv(c("Llama3_api_key", "Llama3_secret_key")))),
    Claude = function() nzchar(Sys.getenv("ANTHROPIC_API_KEY"))
)

lict_providers_present <- function() {
    names(lict_providers)[vapply(lict_providers, function(check) check(), logical(1))]
}

test_that("lict: the runner names the missing provider key variables", {
    skip_if_not_installed("LICT")
    skip_if(
        any(nzchar(Sys.getenv(lict_key_vars))),
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

# the labels are free text, so only their shape is asserted: one non-empty
# string per cluster, none missing
expect_llm_mapping <- function(rec) {
    expect_equal(rec$type, "cluster")
    expect_setequal(names(rec$mapping), levels(obj$clusters))
    labels <- unlist(rec$mapping, use.names = FALSE)
    expect_type(labels, "character")
    expect_false(anyNA(labels))
    expect_true(all(nzchar(labels)))
}

test_that("mllmcelltype: a real run labels the clusters (needs OPENAI_API_KEY)", {
    skip_if(Sys.getenv("OPENAI_API_KEY") == "", "no OPENAI_API_KEY set")

    rec <- RunCellTypeAnnotation(
        obj, "mllmcelltype",
        args = list(tissue = "human PBMC"),
        ident = "clusters"
    )
    expect_llm_mapping(rec)
})

test_that("lict: a real run labels the clusters (needs a provider key)", {
    skip_if(
        !any(nzchar(Sys.getenv(lict_key_vars))),
        "no provider key set"
    )

    # the validate stage asks the model a second time per cluster -- one call
    # is enough to prove the labels come back
    rec <- RunCellTypeAnnotation(
        obj, "lict",
        args = list(species = "Human", tissue = "PBMC", validate = FALSE),
        ident = "clusters"
    )
    # `validate = FALSE` skips the second stage: the result is the first-stage
    # mapping and nothing else, no validated table and no refined labels. The
    # labels are free text, so they are compared by shape rather than against
    # another run of the model.
    expect_llm_mapping(rec)
    expect_named(rec, c("mapping", "type", "more"))
    expect_null(rec$more)
})

test_that("lict: the refinement is skipped when fewer than five providers answer", {
    present <- lict_providers_present()
    skip_if(length(present) == 0, "no provider key set")
    skip_if(
        length(present) == length(lict_providers),
        "all five provider keys are set: the refinement is not skipped"
    )

    # One real run with `validate` on (the default). `Validate()` only gets the
    # tables of the providers that answered, and `Validate_Result_to_Df()` needs
    # all five of them, so the runner skips the refinement instead of padding
    # the empty slots. The reason is logged, and the log is read back as text.
    out <- capture.output(rec <- RunCellTypeAnnotation(
        obj, "lict",
        args = list(species = "Human", tissue = "PBMC"),
        ident = "clusters"
    ))
    log_text <- paste(out, collapse = "\n")

    # the stage-1 labels come back as they are: the refinement would have
    # relabelled the clusters and said so
    expect_llm_mapping(rec)
    expect_null(rec$more$validate)
    expect_match(
        log_text,
        paste0("Annotated ", length(levels(obj$clusters)), " cluster(s) with '"),
        fixed = TRUE
    )
    expect_false(grepl("Refined", log_text, fixed = TRUE))

    # the skip names the count and the providers it is waiting on, and says why
    # -- whichever provider answered, the others are the missing ones
    missing <- setdiff(names(lict_providers), present)
    expect_match(
        log_text,
        paste0(
            "LICT refinement skipped: only ", length(present), " of 5 providers ",
            "answered (missing: ", paste(missing, collapse = ", "), ")"
        ),
        fixed = TRUE
    )
    expect_match(log_text, "Validate_Result_to_Df()", fixed = TRUE)

    # cat(), not message(): the test reporter hides messages of passing tests
    cat(
        "lict refinement skipped, log line: ",
        grep(
            "LICT refinement skipped",
            strsplit(log_text, "\n", fixed = TRUE)[[1]], value = TRUE
        ),
        "\n  stage-1 mapping: ",
        paste(names(rec$mapping), unlist(rec$mapping), sep = " = ", collapse = ", "),
        "\n  more$validate: ",
        if (is.null(rec$more$validate)) "NULL" else "not NULL",
        "\n",
        sep = ""
    )
})

test_that("lict: the refine stage returns the per-cluster table (needs all five provider keys)", {
    present <- lict_providers_present()
    skip_if(
        length(present) < length(lict_providers),
        paste0(
            "LICT's refine stage needs all five provider keys, one per table ",
            "Validate_Result_to_Df() cbind()s by name (ERNIE, Gemini, GPT, ",
            "Llama, Claude); with fewer providers the runner skips it"
        )
    )

    # one full run: every provider annotates (stage 1), then `Validate()` has
    # the model name the markers of those cell types, `Validate_Result_to_Df()`
    # turns that answer into the table `Feedback_Info()` takes, and
    # `Feedback_Info()` runs LICT's "talk-to-machine" refinement on it
    rec <- RunCellTypeAnnotation(
        obj, "lict",
        args = list(species = "Human", tissue = "PBMC"),
        ident = "clusters"
    )
    expect_llm_mapping(rec)

    # the validated table has one row per cluster: the providers' labels plus
    # the markers the model named and the reliable/unreliable flags
    validated <- rec$more$validate
    expect_s3_class(validated, "data.frame")
    expect_equal(nrow(validated), length(levels(obj$clusters)))
    # the refinement only completes with a full provider set (`Feedback_Info()`
    # asks all five); when it does, its labels are the returned annotation
    if (!is.null(rec$more$refined)) {
        expect_equal(rec$mapping, rec$more$refined)
    }
    expect_true(
        all(c("clusters", "cell_type", "reliable", "unreliable") %in% names(validated))
    )
    # every provider column belongs to a provider that answered: no slot is
    # filled with another provider's numbers under a borrowed name
    marker_cols <- grep(
        "_(positive|negative)_marker$", names(validated), value = TRUE
    )
    expect_true(length(marker_cols) > 0)
    expect_true(all(
        sub("_(positive|negative)_marker$", "", marker_cols) %in% names(lict_providers)
    ))
    # cat(), not message(): the test reporter hides messages of passing tests
    cat(
        "lict validate table: ", nrow(validated), " row(s) x ",
        ncol(validated), " column(s)\n  columns: ",
        paste(names(validated), collapse = ", "), "\n  cell_type: ",
        paste(validated$cell_type, collapse = ", "), "\n",
        sep = ""
    )
})
