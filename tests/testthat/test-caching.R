test_that("caching: not cached by default", {
    cache <- Cache$new(1, cache_dir = tempdir(), prefix = "test", kind = "object")
    expect_false(cache$is_cached())
})

test_that("caching: cached", {
    obj <- list(a = 1, b = 2)
    cache <- Cache$new(2, cache_dir = tempdir(), prefix = "test", kind = "object")
    expect_false(cache$is_cached())

    cache$save(obj)
    expect_true(cache$is_cached())
    cached <- cache$restore()
    expect_equal(cached, obj)
})

test_that("caching: not cached after clear", {
    obj <- list(a = 1, b = 2)
    cache <- Cache$new(3, cache_dir = tempdir(), prefix = "test", kind = "object")
    expect_false(cache$is_cached())

    cache$save(obj)
    expect_true(cache$is_cached())
    cached <- cache$restore()
    expect_equal(cached, obj)

    cache$clear()
    expect_false(cache$is_cached())
})

test_that("caching: not cached after clear_all", {
    obj <- list(a = 1, b = 2)
    cache <- Cache$new(4, cache_dir = tempdir(), prefix = "test", kind = "object")
    expect_false(cache$is_cached())

    cache$save(obj)
    cached <- cache$restore()
    expect_equal(cached, obj)

    cache$clear_all()
    expect_false(cache$is_cached())
})

test_that("caching: not cached when cache_dir is NULL or FALSE", {
    obj <- list(a = 1, b = 2)

    # Test with NULL cache_dir
    cache_null <- Cache$new(5, cache_dir = NULL, prefix = "test", kind = "object")
    expect_false(cache_null$is_cached())

    expect_error(cache_null$save(obj))
    expect_error(cache_null$restore())

    # Test with FALSE cache_dir
    cache_false <- Cache$new(6, cache_dir = FALSE, prefix = "test", kind = "object")
    expect_false(cache_false$is_cached())

    expect_error(cache_false$save(obj))
    expect_error(cache_false$restore())
})

test_that("caching: save and get file", {
    temp_file <- tempfile()
    writeLines("Hello, World!", temp_file)

    cache <- Cache$new(7, cache_dir = tempdir(), prefix = "test_file", kind = "file")
    expect_false(cache$is_cached())

    cache$save(temp_file)
    unlink(temp_file)  # Remove original file to test restoration
    expect_false(file.exists(temp_file))

    cache$restore()
    expect_true(file.exists(temp_file))

    cached_file <- cache$get_path()
    expect_true(file.exists(cached_file))
    expect_equal(readLines(cached_file), "Hello, World!")

    unlink(temp_file)
    cache$clear()
})

test_that("caching: save and get directory", {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    writeLines("Hello, World!", file.path(temp_dir, "test.txt"))

    cache <- Cache$new(8, cache_dir = tempdir(), prefix = "test_dir", kind = "dir")
    expect_false(cache$is_cached())

    cache$save(temp_dir)
    unlink(temp_dir, recursive = TRUE)  # Remove original directory to test restoration
    expect_false(dir.exists(temp_dir))

    cache$restore()
    expect_true(dir.exists(temp_dir))

    cached_dir <- cache$get_path()
    expect_true(dir.exists(cached_dir))
    expect_true(file.exists(file.path(cached_dir, "test.txt")))
    expect_equal(readLines(file.path(cached_dir, "test.txt")), "Hello, World!")

    unlink(temp_dir, recursive = TRUE)
    cache$clear()
})

test_that("caching: save and get directory recursively", {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    sub_dir <- file.path(temp_dir, "subdir")
    dir.create(sub_dir)
    writeLines("Hello, World!", file.path(sub_dir, "test.txt"))

    cache <- Cache$new(9, cache_dir = tempdir(), prefix = "test_recursive", kind = "dir")
    expect_false(cache$is_cached())

    cache$save(temp_dir)
    unlink(temp_dir, recursive = TRUE)  # Remove original directory to test restoration
    expect_false(dir.exists(temp_dir))
    expect_false(dir.exists(sub_dir))
    expect_false(file.exists(file.path(sub_dir, "test.txt")))

    cache$restore()
    expect_true(dir.exists(temp_dir))
    expect_true(dir.exists(sub_dir))
    expect_true(file.exists(file.path(sub_dir, "test.txt")))
    expect_equal(readLines(file.path(sub_dir, "test.txt")), "Hello, World!")

    unlink(temp_dir, recursive = TRUE)
    cache$clear()
})

test_that("caching: save and get with path prefix", {
    tmpdir <- gettempdir()
    tmpfile1 <- file.path(tmpdir, "prefix_file1.txt")
    tmpfile2 <- file.path(tmpdir, "prefix_file2.txt")
    writeLines("Hello, World!", tmpfile1)
    writeLines("Goodbye, World!", tmpfile2)

    prefix <- file.path(tmpdir, "prefix_")
    cache <- Cache$new(10, cache_dir = tempdir(), prefix = "test_prefix", kind = "prefix")
    cache$clear()  # Ensure cache is empty before starting
    expect_false(cache$is_cached())

    cache$save(prefix)
    expect_true(cache$is_cached())

    unlink(tmpfile1)  # Remove original files to test restoration
    unlink(tmpfile2)
    expect_false(file.exists(tmpfile1))
    expect_false(file.exists(tmpfile2))

    cache$restore()
    expect_true(file.exists(tmpfile1))
    expect_true(file.exists(tmpfile2))
    expect_equal(readLines(tmpfile1), "Hello, World!")
    expect_equal(readLines(tmpfile2), "Goodbye, World!")

    cache_prefix <- cache$get_path()
    expect_true(file.exists(paste0(cache_prefix, ".prefix_file1.txt")))
    expect_true(file.exists(paste0(cache_prefix, ".prefix_file2.txt")))
    expect_equal(readLines(paste0(cache_prefix, ".prefix_file1.txt")), "Hello, World!")
    expect_equal(readLines(paste0(cache_prefix, ".prefix_file2.txt")), "Goodbye, World!")

    unlink(tmpfile1)
    unlink(tmpfile2)
    cache$clear()
})

test_that("caching: save and restore with path prefix recursively", {
    tmpdir <- gettempdir()
    tmpdir1 <- file.path(tmpdir, "prefix_rec_dir1")
    tmpdir2 <- file.path(tmpdir, "prefix_rec_dir2")
    dir.create(tmpdir1, showWarnings = FALSE)
    dir.create(tmpdir2, showWarnings = FALSE)
    writeLines("Hello, World!", file.path(tmpdir1, "test1.txt"))
    subdir <- file.path(tmpdir2, "subdir")
    dir.create(subdir, showWarnings = FALSE)
    writeLines("Goodbye, World!", file.path(subdir, "test2.txt"))

    prefix <- file.path(tmpdir, "prefix_rec_")
    cache <- Cache$new(11, cache_dir = tempdir(), prefix = "test_prefix_recursive", kind = "prefix")
    cache$clear()  # Ensure cache is empty before starting
    expect_false(cache$is_cached())

    cache$save(prefix)
    expect_true(cache$is_cached())

    unlink(tmpdir1, recursive = TRUE)  # Remove original directories to test restoration
    unlink(tmpdir2, recursive = TRUE)
    expect_false(dir.exists(tmpdir1))
    expect_false(dir.exists(tmpdir2))
    expect_false(file.exists(file.path(tmpdir1, "test1.txt")))
    expect_false(dir.exists(subdir))
    expect_false(file.exists(file.path(subdir, "test2.txt")))

    cache$restore()
    expect_true(dir.exists(tmpdir1))
    expect_true(dir.exists(tmpdir2))
    expect_true(file.exists(file.path(tmpdir1, "test1.txt")))
    expect_true(dir.exists(subdir))
    expect_equal(readLines(file.path(tmpdir1, "test1.txt")), "Hello, World!")
    expect_equal(readLines(file.path(subdir, "test2.txt")), "Goodbye, World!")

    cache_prefix <- cache$get_path()
    expect_true(dir.exists(paste0(cache_prefix, ".prefix_rec_dir1")))
    expect_true(dir.exists(paste0(cache_prefix, ".prefix_rec_dir2")))
    expect_true(file.exists(file.path(paste0(cache_prefix, ".prefix_rec_dir1"), "test1.txt")))
    expect_true(file.exists(file.path(paste0(cache_prefix, ".prefix_rec_dir2"), "subdir", "test2.txt")))
    expect_equal(readLines(file.path(paste0(cache_prefix, ".prefix_rec_dir1"), "test1.txt")), "Hello, World!")
    expect_equal(readLines(file.path(paste0(cache_prefix, ".prefix_rec_dir2"), "subdir", "test2.txt")), "Goodbye, World!")

    unlink(tmpdir1, recursive = TRUE)
    unlink(tmpdir2, recursive = TRUE)
    cache$clear()
})

test_that("caching: cannot save twice", {
    obj <- list(a = 1, b = 2)
    cache <- Cache$new(12, cache_dir = tempdir(), prefix = "test_double_save", kind = "object")
    expect_false(cache$is_cached())

    cache$save(obj)
    expect_true(cache$is_cached())

    expect_error(cache$save(obj))

    cached <- cache$restore()
    expect_equal(cached, obj)

    cache$clear()
})

test_that("caching: error when partially restored", {
    tmpdir <- gettempdir()
    tmpdir1 <- file.path(tmpdir, "prefix_part_dir1")
    tmpdir2 <- file.path(tmpdir, "prefix_part_dir2")
    dir.create(tmpdir1, showWarnings = FALSE)
    dir.create(tmpdir2, showWarnings = FALSE)
    writeLines("Hello, World!", file.path(tmpdir1, "test1.txt"))
    subdir <- file.path(tmpdir2, "subdir")
    dir.create(subdir, showWarnings = FALSE)
    writeLines("Goodbye, World!", file.path(subdir, "test2.txt"))

    prefix <- file.path(tmpdir, "prefix_part_")
    cache <- Cache$new(11, cache_dir = tempdir(), prefix = "test_prefix_part", kind = "prefix")
    cache$clear()  # Ensure cache is empty before starting
    expect_false(cache$is_cached())

    cache$save(prefix)
    expect_true(cache$is_cached())

    cache_prefix <- cache$get_path()
    unlink(paste0(cache_prefix, ".prefix_part_dir2"), recursive = TRUE)  # Remove subdir to simulate partial restoration

    unlink(tmpdir1, recursive = TRUE)  # Remove original directories to test restoration
    unlink(tmpdir2, recursive = TRUE)

    expect_error(cache$restore())

    unlink(tmpdir1, recursive = TRUE)
    unlink(tmpdir2, recursive = TRUE)
    cache$clear()
})
