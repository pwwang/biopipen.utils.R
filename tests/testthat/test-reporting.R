
test_that("Reporter initializes correctly", {
    reporter <- Reporter$new()
    expect_equal(reporter$report, list())
})

test_that("Reporter adds content correctly", {
    reporter <- Reporter$new()
    reporter$add("content1", h1 = "Heading1", h2 = "Subheading1", h3 = "Subsubheading1", ui = "ui1")
    expected_report <- list(
        Heading1 = list(
            Subheading1 = list(
                Subsubheading1 = list(
                    ui1 = list("content1")
                )
            )
        )
    )
    expect_equal(reporter$report, expected_report)
})

test_that("Reporter adds multiple contents correctly", {
    reporter <- Reporter$new()
    reporter$add("content1", "content2", h1 = "Heading1", h2 = "Subheading1", h3 = "Subsubheading1", ui = "ui1")
    expected_report <- list(
        Heading1 = list(
            Subheading1 = list(
                Subsubheading1 = list(
                    ui1 = list("content1", "content2")
                )
            )
        )
    )
    expect_equal(reporter$report, expected_report)
})

test_that("Reporter saves report correctly", {
    reporter <- Reporter$new()
    reporter$add(list(x = "content1"), h1 = "Heading1", h2 = "Subheading1", h3 = "Subsubheading1", ui = "ui1")
    temp_file <- tempfile(fileext = ".json")
    reporter$save(temp_file, clear = FALSE)
    saved_report <- jsonlite::fromJSON(temp_file)
    saved_report$Heading1$Subheading1$Subsubheading1$ui1 <- list(list(x = "content1"))
    expected_report <- list(
        Heading1 = list(
            Subheading1 = list(
                Subsubheading1 = list(
                    ui1 = list(list(x = "content1"))
                )
            )
        )
    )
    expect_equal(saved_report, expected_report)
    expect_equal(reporter$report, expected_report)
    unlink(temp_file)
})

test_that("Reporter clears report after saving if clear is TRUE", {
    reporter <- get_reporter()
    reporter$add("content1", h1 = "Heading1", h2 = "Subheading1", h3 = "Subsubheading1", ui = "ui1")
    temp_file <- tempfile(fileext = ".json")
    reporter$save(temp_file, clear = TRUE)
    expect_equal(reporter$report, list())
    unlink(temp_file)
})
