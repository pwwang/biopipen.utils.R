test_that("caching: not cached by default", {
    cache <- Caching$new(1, cache_dir = tempdir(), prefix = "test", kind = "object")
    expect_null(cache$get())
})

test_that("caching: cached", {
    obj <- list(a = 1, b = 2)
    cache <- Caching$new(2, cache_dir = tempdir(), prefix = "test", kind = "object")
    expect_null(cache$get())

    cache$save(obj)
    cached <- cache$get()
    expect_equal(cached, obj)
})

test_that("caching: not cached after clear", {
    obj <- list(a = 1, b = 2)
    cache <- Caching$new(3, cache_dir = tempdir(), prefix = "test", kind = "object")
    expect_null(cache$get())

    cache$save(obj)
    cached <- cache$get()
    expect_equal(cached, obj)

    cache$clear()
    expect_null(cache$get())
})

test_that("caching: not cached after clear_all", {
    obj <- list(a = 1, b = 2)
    cache <- Caching$new(4, cache_dir = tempdir(), prefix = "test", kind = "object")
    expect_null(cache$get())

    cache$save(obj)
    cached <- cache$get()
    expect_equal(cached, obj)

    cache$clear_all()
    expect_null(cache$get())
})

test_that("caching: not cached when cache_dir is NULL or FALSE", {
    obj <- list(a = 1, b = 2)

    # Test with NULL cache_dir
    cache_null <- Caching$new(5, cache_dir = NULL, prefix = "test", kind = "object")
    expect_null(cache_null$get())

    cache_null$save(obj)
    expect_null(cache_null$get())

    # Test with FALSE cache_dir
    cache_false <- Caching$new(6, cache_dir = FALSE, prefix = "test", kind = "object")
    expect_null(cache_false$get())

    cache_false$save(obj)
    expect_null(cache_false$get())
})

test_that("caching: save and get file", {
    temp_file <- tempfile()
    writeLines("Hello, World!", temp_file)

    cache <- Caching$new(7, cache_dir = tempdir(), prefix = "test_file", kind = "file")
    expect_null(cache$get(temp_file))

    cache$save(temp_file)
    temp_file2 <- tempfile()
    cached_file <- cache$get(temp_file2)

    expect_true(file.exists(cached_file))
    expect_equal(temp_file2, cached_file)
    expect_equal(readLines(cached_file), "Hello, World!")

    unlink(temp_file)
    unlink(temp_file2)
})

test_that("caching: save and get directory", {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    writeLines("Hello, World!", file.path(temp_dir, "test.txt"))

    cache <- Caching$new(8, cache_dir = tempdir(), prefix = "test_dir", kind = "dir")
    expect_null(cache$get(temp_dir))

    cache$save(temp_dir)
    temp_dir2 <- tempfile()
    dir.create(temp_dir2)

    cached_dir <- cache$get(temp_dir2)

    expect_true(dir.exists(cached_dir))
    expect_true(file.exists(file.path(cached_dir, "test.txt")))
    expect_equal(readLines(file.path(cached_dir, "test.txt")), "Hello, World!")

    unlink(temp_dir, recursive = TRUE)
    unlink(temp_dir2, recursive = TRUE)
})
