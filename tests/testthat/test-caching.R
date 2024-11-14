test_that("caching: not cached by default", {
    obj <- list(a = 1, b = 2)
    cache_dir <- tempdir()
    cached <- get_cached(obj, "test", cache_dir)
    expect_null(cached$data)
})

test_that("caching: cached", {
    obj <- list(a = 1, b = 2)
    cache_dir <- tempdir()
    cached <- get_cached(obj, "test", cache_dir)
    cached$data <- obj
    save_to_cache(cached, "test", cache_dir)
    cached <- get_cached(obj, "test", cache_dir)
    expect_equal(cached$data, obj)
})

test_that("caching: not cached after deletion", {
    obj <- list(a = 1, b = 2)
    cache_dir <- tempdir()
    cached <- get_cached(obj, "test", cache_dir)
    cached$data <- obj
    save_to_cache(cached, "test", cache_dir)
    cached <- get_cached(obj, "test", cache_dir)
    expect_equal(cached$data, obj)
    unlink(file.path(cache_dir, paste0(cached$dig, ".test.RDS")))
    cached <- get_cached(obj, "test", cache_dir)
    expect_null(cached$data)
})

test_that("caching: returns NULL when cache_dir is NULL", {
    obj <- list(a = 1, b = 2)
    cached <- get_cached(obj, "test", NULL)
    expect_null(cached$data)
    expect_null(save_to_cache(cached, "test", NULL))
})
