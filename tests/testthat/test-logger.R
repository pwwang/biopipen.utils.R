test_that("get_logger: returns a list with logging functions", {
    logger <- get_logger()
    expect_is(logger, "list")
    expect_is(logger$info, "function")
    expect_is(logger$debug, "function")
    expect_is(logger$warn, "function")
    expect_is(logger$error, "function")
    expect_is(logger$fatal, "function")
    expect_is(logger$success, "function")
    expect_is(logger$trace, "function")
})

test_that("get_logger: works with custom format", {
    logger <- get_logger(format = "{msg}")
    expect_output(logger$info("hello"), "hello")
})

test_that("get_logger: works with custom appender", {
    logfile <- tempfile()
    logger <- get_logger(appender = logger::appender_file(logfile))
    logger$info("hello")
    expect_true(file.exists(logfile))
    logged <- readLines(logfile)[1]
    expect_true(grepl("INFO", logged))
    expect_true(grepl("hello", logged))
})

test_that("get_logger: errors when invalid level", {
    expect_error(get_logger(level = "invalid"))
})

test_that("get_logger: errors when invalid appender", {
    expect_error(get_logger(appender = "invalid"))
})
