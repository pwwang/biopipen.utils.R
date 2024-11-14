test_that("repr: works for numerics", {
    expect_equal(repr(1), "1")
    expect_equal(repr(1:3), "c(1, 2, 3)")
    expect_equal(repr(1:3, newline = TRUE), "c(\n  1,\n  2,\n  3\n)")
})

test_that("repr: works for characters", {
    expect_equal(repr("a"), "\"a\"")
    expect_equal(repr(c("a", "b")), "c('a', 'b')")
    expect_equal(repr(c("a", "b"), newline = TRUE), "c(\n  'a',\n  'b'\n)")
})

test_that("repr: works for factors", {
    expect_equal(repr(factor("a")), 'factor("a", levels = "a")')
    expect_equal(repr(factor(c("a", "b"))), "factor(c('a', 'b'), levels = c('a', 'b'))")
    expect_equal(repr(factor(c("a", "b")), newline = TRUE), "factor(\n  c(\n  'a',\n  'b'\n),\n  levels = c(\n  'a',\n  'b'\n)\n)")
})

test_that("repr: works for logicals", {
    expect_equal(repr(TRUE), "TRUE")
    expect_equal(repr(c(TRUE, FALSE)), "c(TRUE, FALSE)")
    expect_equal(repr(c(TRUE, FALSE), newline = TRUE), "c(\n  TRUE,\n  FALSE\n)")
})

test_that("repr: works for lists", {
    expect_equal(repr(list(1)), "list(1)")
    expect_equal(repr(list(1, 2)), "list(1, 2)")
    expect_equal(repr(list(1, 2), newline = TRUE), "list(\n  1,\n  2\n)")
    expect_equal(repr(list(a = 1)), "list(a = 1)")
    expect_equal(repr(list(a = 1, 2)), "list(a = 1, 2)")
})

test_that("repr: works for data.frames", {
    expect_equal(repr(data.frame(a = 1)), "data.frame(\n  a = 1\n)")
    expect_equal(repr(data.frame(a = 1, b = 2)), "data.frame(\n  a = 1,\n  b = 2\n)")
    expect_equal(repr(data.frame(a = 1, b = 2), newline = TRUE), "data.frame(\n  a = 1,\n  b = 2\n)")
})

test_that("repr: works for matrices", {
    expect_equal(repr(matrix(1)), "matrix(\n  1,\n  nrow = 1\n)")
    expect_equal(repr(matrix(1:4, nrow = 2)), "matrix(\n  c(1, 2, 3, 4),\n  nrow = 2\n)")
    expect_equal(repr(matrix(1:4, nrow = 2), newline = TRUE), "matrix(\n  c(\n  1,\n  2,\n  3,\n  4\n),\n  nrow = 2\n)")
})

test_that("repr: works for NULL and NA", {
    expect_equal(repr(NULL), "NULL")
    expect_equal(repr(NA), "NA")
    expect_equal(repr(c(NA, NULL)), "NA")
    expect_equal(repr(c(NA, NULL, NA)), "c(NA, NA)")
})

test_that("repr: works for formulas", {
    expect_equal(repr(~a), "~a")
    expect_equal(repr(~a + b), "~a + b")
    expect_equal(repr(~a + b, newline = TRUE), "~a + b")
})

test_that("repr: works for environments", {
    expect_equal(repr(environment()), "rlang::env(  )")
    expect_equal(repr(new.env()), "rlang::env(  )")
    expect_equal(repr(rlang::env(a = 1)), "rlang::env( a = 1 )")
    expect_equal(repr(.GlobalEnv), "GlobalEnv")
})

test_that("repr: default", {
    x <- list(a = 1, b = 2)
    class(x) <- "foo"
    expect_equal(repr(x), "<foo: x>")
})
