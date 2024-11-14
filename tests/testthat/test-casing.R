test_that("expand_cases works with default values", {
    cases <- list(
        case1 = list(a = 1, b = 2),
        case2 = list(a = 3)
    )
    defaults <- list(a = 0, b = 0, c = 0)
    expanded_cases <- expand_cases(cases, defaults)

    expect_equal(expanded_cases$case1, list(a = 1, b = 2, c = 0))
    expect_equal(expanded_cases$case2, list(a = 3, b = 0, c = 0))
})

test_that("expand_cases works with no cases", {
    defaults <- list(a = 0, b = 0, c = 0)
    expanded_cases <- expand_cases(NULL, defaults)

    expect_equal(expanded_cases$DEFAULT, defaults)
})

test_that("expand_cases works with expand function", {
    cases <- list(
        case1 = list(a = 1, b = 2)
    )
    defaults <- list(a = 0, b = 0, c = 0)
    expand_fn <- function(name, case) {
        out <- list(case, case)
        names(out) <- c(paste0(name, "_1"), paste0(name, "_2"))
        out
    }
    expanded_cases <- expand_cases(cases, defaults, expand_fn)
    expect_equal(expanded_cases$case1_1, list(a = 1, b = 2, c = 0))
    expect_equal(expanded_cases$case1_2, list(a = 1, b = 2, c = 0))
})

test_that("case_info works with simple name", {
    name <- "testcase"
    outdir <- tempdir()
    info <- case_info(name, outdir)

    expect_equal(info$name, "testcase")
    expect_null(info$section)
    expect_equal(info$slug, "testcase")
    expect_equal(info$prefix, file.path(outdir, "testcase"))
})

test_that("case_info works with sectioned name", {
    name <- "section::testcase"
    outdir <- tempdir()
    info <- case_info(name, outdir)

    expect_equal(info$name, "testcase")
    expect_equal(info$section, "section")
    expect_equal(info$slug, "testcase")
    expect_equal(info$section_slug, "section")
    expect_equal(info$prefix, file.path(outdir, "section", "testcase"))
})

test_that("case_info creates directories when create is TRUE", {
    name <- "section::testcase"
    outdir <- tempdir()
    info <- case_info(name, outdir, create = TRUE)

    expect_true(dir.exists(info$prefix))
})

test_that("case_info creates parent directories when create is TRUE and is_dir is FALSE", {
    name <- "section::testcase"
    outdir <- tempdir()
    info <- case_info(name, outdir, is_dir = FALSE, create = TRUE)

    expect_true(dir.exists(dirname(info$prefix)))
})
