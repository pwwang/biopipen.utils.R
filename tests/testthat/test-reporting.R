
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

test_that("Reporter adds case content correctly", {
    reporter <- Reporter$new()

    reporter$add2("content1", hs = "Case1", ui = "flat")
    expect_equal(reporter$report, list(Case1 = list(`#` = list(`#` = list(flat = list("content1"))))))
    reporter$clear()
    expect_equal(length(reporter$report), 0)

    reporter$add2("content1", hs = "Case1", hs2 = c("A"), ui = "flat")
    expect_equal(reporter$report, list(Case1 = list(A = list(`#` = list(flat = list("content1"))))))
    reporter$clear()

    reporter$add2("content1", hs = "Case1", hs2 = c("A", "B"), ui = "flat")
    expect_equal(reporter$report, list(Case1 = list(A = list(B = list(flat = list("content1"))))))
    reporter$clear()

    reporter$add2("content1", hs = c("Section1", "Case1"), ui = "flat")
    expect_equal(reporter$report, list(Section1 = list(Case1 = list(`#` = list(flat = list("content1"))))))
    reporter$clear()

    reporter$add2("content1", hs = c("Section1", "Case1"), hs2 = c("A"), ui = "flat")
    expect_equal(reporter$report, list(Section1 = list(Case1 = list(A = list(flat = list("content1"))))))
    reporter$clear()

    reporter$add2("content1", hs = c("Section1", "Case1"), hs2 = c("A", "B"), ui = "flat")
    expect_equal(reporter$report, list(`Section1: Case1` = list(A = list(B = list(flat = list("content1"))))))
    reporter$clear()

    reporter$add2("content1", hs = c("Section1", "Section2", "Case1"), ui = "flat")
    expect_equal(reporter$report, list(Section1 = list(Section2 = list(Case1 = list(flat = list("content1"))))))
    reporter$clear()

    reporter$add2("content1", hs = c("Section1", "Section2", "Section3", "Case1"), ui = "flat")
    expect_equal(reporter$report, list(Section1 = list(Section2 = list(`Section3: Case1` = list(flat = list("content1"))))))
    reporter$clear()

    reporter$add2("content1", hs = c("Section1", "Section2", "Section3", "Section4", "Case1"), ui = "flat")
    expect_equal(reporter$report, list(Section1 = list(Section2 = list(`Section3: Section4: Case1` = list(flat = list("content1"))))))
})

test_that("Reporter generates image report correctly", {
    reporter <- Reporter$new()
    image_report <- reporter$image("prefix", c("pdf", "svg"), TRUE)

    expected <- list(
        kind = "image",
        src = "prefix.png",
        download = list("prefix.pdf", "prefix.svg", list(
            src = "prefix.code.zip",
            tip = "Download the code to reproduce the plot",
            icon = "Code"
        ))
    )
    expect_equal(image_report, expected)
})

test_that("Reporter generates image report works with extra arguments", {
    reporter <- Reporter$new()
    image_report <- reporter$image("prefix", c("pdf", "svg"), TRUE, kind = "table_image", descr = "blah")

    expected <- list(
        kind = "table_image",
        src = "prefix.png",
        descr = "blah",
        download = list("prefix.pdf", "prefix.svg", list(
            src = "prefix.code.zip",
            tip = "Download the code to reproduce the plot",
            icon = "Code"
        ))
    )
    expect_equal(image_report, expected)
})
