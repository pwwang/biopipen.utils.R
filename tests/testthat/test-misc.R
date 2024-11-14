test_that("bQuote: doesn't backtick quote valid variable names", {
    expect_equal(bQuote("a"), "a")
    expect_equal(bQuote(c(a = "a")), c(a = "a"))
    expect_equal(bQuote(c("a", "b", "c")), c("a", "b", "c"))
    expect_equal(bQuote("`"), "`\\``")
})

test_that("bQuote: errors when backtick in middle of string", {
    expect_error(bQuote("a`b"))
})

test_that("bQuote: backtick quotes invalid variable names", {
    expect_equal(bQuote("a 1"), "`a 1`")
    expect_equal(bQuote(c("a 1", "b 2", "c 3")), c("`a 1`", "`b 2`", "`c 3`"))
})

test_that("bQuote: doesn't backtick quote backtick quoted strings", {
    expect_equal(bQuote("`a`"), "`a`")
    expect_equal(bQuote(c("`a`", "`b`", "`c`")), c("`a`", "`b`", "`c`"))
})

test_that("bQuote: keeps names", {
    expect_equal(bQuote(c(a = "a", b = "b")), c(a = "a", b = "b"))
})

test_that("bQuote: force works", {
    expect_equal(bQuote("", force = TRUE), "``")
    expect_equal(bQuote("a", force = TRUE), "`a`")
    expect_equal(bQuote(c("a", "b", "c"), force = TRUE), c("`a`", "`b`", "`c`"))
    expect_equal(bQuote("`a`", force = TRUE), "`a`")
    expect_equal(bQuote(c("`a`", "`b`", "`c`"), force = TRUE), c("`a`", "`b`", "`c`"))
})

test_that("slugify: works", {
    expect_equal(slugify(NULL), NULL)
    expect_equal(slugify("a"), "a")
    expect_equal(slugify(c("a", "b")), c("a", "b"))
    expect_equal(slugify("a 1"), "a-1")
    expect_equal(slugify(c("a 1", "b 2", "c 3")), c("a-1", "b-2", "c-3"))
    expect_equal(slugify(c(x = "a 1", y = "b 2", z = "c 3")), c(x = "a-1", y = "b-2", z = "c-3"))
    expect_equal(slugify("a 1", tolower = TRUE), "a-1")
    expect_equal(slugify(c("a 1", "b 2", "c 3"), tolower = TRUE), c("a-1", "b-2", "c-3"))
    expect_equal(slugify("a 1", non_alphanum_replace = "_"), "a_1")
    expect_equal(slugify(c("a 1", "b 2", "c 3"), non_alphanum_replace = "_"), c("a_1", "b_2", "c_3"))
    expect_equal(slugify("a 1", collapse_replace = FALSE), "a-1")
    expect_equal(slugify(c("a 1", "b 2", "c 3"), collapse_replace = FALSE), c("a-1", "b-2", "c-3"))
    expect_equal(slugify("a 1", non_alphanum_replace = "_", collapse_replace = FALSE), "a_1")
    expect_equal(slugify(c("a 1", "b 2", "c 3"), non_alphanum_replace = "_", collapse_replace = FALSE), c("a_1", "b_2", "c_3"))
    expect_equal(slugify("a 1", tolower = TRUE, non_alphanum_replace = "_", collapse_replace = FALSE), "a_1")
    expect_equal(slugify(c("a 1", "b 2", "c 3"), tolower = TRUE, non_alphanum_replace = "_", collapse_replace = FALSE), c("a_1", "b_2", "c_3"))
    expect_equal(slugify(c("A 1", "B 2", "C 3"), tolower = TRUE, non_alphanum_replace = "_", collapse_replace = FALSE), c("a_1", "b_2", "c_3"))
})

test_that("html_escape: works", {
    expect_equal(html_escape(NULL), "")
    expect_equal(html_escape("a"), "a")
    expect_equal(html_escape(c("a", "b")), c("a", "b"))
    expect_equal(html_escape("<a>"), "&lt;a&gt;")
    expect_equal(html_escape(c("<a>", "<b>", "<c>")), c("&lt;a&gt;", "&lt;b&gt;", "&lt;c&gt;"))
    expect_equal(html_escape(c(a = "<a>", b = "<b>", c = "<c>")), c(a = "&lt;a&gt;", b = "&lt;b&gt;", c = "&lt;c&gt;"))
})

test_that("extract_vars: works", {
    x <- list(a = 1, b = 2, c = 3)
    expect_equal(extract_vars(x, "a"), list(b = 2, c = 3))
    expect_true(exists("a"))
    expect_equal(a, 1)
})

test_that("extract_vars: works with keep", {
    x <- list(a = 1, b = 2, c = 3)
    expect_equal(extract_vars(x, "a", keep = TRUE), list(a = 1, b = 2, c = 3))
    expect_true(exists("a"))
    expect_equal(a, 1)
})

test_that("extract_vars: works with env", {
    x <- list(a = 1, b = 2, c = 3)
    new_env <- new.env()
    expect_equal(extract_vars(x, "a", env = new_env), list(b = 2, c = 3))
    expect_equal(new_env$a, 1)
})

test_that("extract_vars: works with renaming", {
    x <- list(a = 1, b = 2, c = 3)
    expect_equal(extract_vars(x, b = "a"), list(b = 2, c = 3))
    expect_true(exists("b"))
    expect_equal(b, 1)
})

test_that("extract_vars: errors when var not found", {
    x <- list(a = 1, b = 2, c = 3)
    expect_error(extract_vars(x, "d"))
})

test_that("list_update: works", {
    x <- list(a = 1, b = 2, c = 3)
    y <- list(c = 4, d = 5)

    expect_equal(list_update(NULL, NULL), list())
    expect_equal(list_update(x, NULL), x)
    expect_equal(list_update(NULL, x), x)
    expect_equal(list_update(x, y), list(a = 1, b = 2, c = 4, d = 5))
    expect_equal(list_update(y, x), list(c = 3, d = 5, a = 1, b = 2))
})

test_that("list_update: keeps NULL items", {
    x <- list(a = 1, b = 2)
    y <- list(b = 3, c = NULL)
    expect_equal(list_update(x, y), list(a = 1, b = 3, c = NULL))
})

test_that("list_update: updates recursively", {
    x <- list(a = list(b = 1, c = 2), d = 3)
    y <- list(a = list(b = 4, e = 5), f = 6)
    expect_equal(list_update(x, y), list(a = list(b = 4, c = 2, e = 5), d = 3, f = 6))
})

test_that("list_update: depth works", {
    x <- list(a = list(b = 1, c = 2), d = 3)
    y <- list(a = list(b = 4, e = 5), f = 6)
    expect_equal(list_update(x, y, depth = 1), list(a = list(b = 4, c = 2, e = 5), d = 3, f = 6))
    expect_equal(list_update(x, y, depth = -1), list(a = list(b = 4, c = 2, e = 5), d = 3, f = 6))
})
