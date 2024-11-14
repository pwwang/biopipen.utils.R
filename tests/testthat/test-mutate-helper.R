
test_that("more_than function works correctly", {
    df <- data.frame(
        id = c("A", "A", "B", "B", "C", "C", "D", "D"),
        group = c("G1", "G2", "G1", "G2", "G1", "G2", "G1", "G2"),
        count = c(10, 5, 3, 8, 7, 7, 2, 2)
    )

    result <- more_than(df, group_by = group, idents = c("G1", "G2"), id = id, compare = count, return_type = "uids")
    expect_equal(result, c("A"))

    result2 <- more_than(df, group_by = "group", idents = c("G1", "G2"), id = "id", compare = "count", return_type = "uids")
    expect_equal(result, result2)
})

test_that("more_than works with compare = .n", {
    df <- data.frame(
        id = c("A", "A", "A", "B", "B", "B", "C", "C", "D", "D"),
        group = c("G1", "G1", "G2", "G1", "G2", "G2", "G1", "G2", "G1", "G2"),
        count = rep(1, 10)
    )

    result <- more_than(df, group_by = group, idents = c("G1", "G2"), id = id, compare = .n, return_type = "uids")
    expect_equal(result, c("A"))
})

test_that("more_than works with split_by", {
    df <- data.frame(
        id = c("A", "A", "B", "B", "A", "A", "B", "B"),
        group = c("G1", "G2", "G1", "G2", "G1", "G2", "G1", "G2"),
        count = c(10, 5, 3, 8, 7, 7, 3, 2),
        split = c("S1", "S1", "S1", "S1", "S2", "S2", "S2", "S2")
    )

    result <- more_than(df, group_by = group, split_by = split, idents = c("G1", "G2"), id = id, compare = count, return_type = "ids")
    expect_equal(result, c("A", "A", NA, NA, NA, NA, "B", "B"))

    result2 <- more_than(df, group_by = "group", split_by = "split", idents = c("G1", "G2"), id = "id", compare = "count", return_type = "ids")
    expect_equal(result, result2)
})

test_that("more_than works with return_type = subdf", {
    df <- data.frame(
        id = c("A", "A", "B", "B", "C", "C", "D", "D"),
        group = c("G1", "G2", "G1", "G2", "G1", "G2", "G1", "G2"),
        count = c(10, 5, 3, 8, 7, 7, 2, 2)
    )

    result <- more_than(df, group_by = group, idents = c("G1", "G2"), id = id, compare = count, return_type = "subdf")
    expect_equal(as.data.frame(result), data.frame(
        id = c("A", "A"),
        group = c("G1", "G2"),
        count = c(10, 5)
    ))

    result2 <- more_than(df, group_by = "group", idents = c("G1", "G2"), id = "id", compare = "count", return_type = "subdf")
    expect_equal(result, result2)
})

test_that("more_than function works with return_type = 'ids'", {
    df <- data.frame(
        id = c("A", "A", "B", "B", "C", "C", "D", "D"),
        group = c("G1", "G2", "G1", "G2", "G1", "G2", "G1", "G2"),
        count = c(10, 5, 3, 8, 7, 7, 2, 2)
    )

    result <- more_than(df, group_by = group, idents = c("G1", "G2"), id = id, compare = count, return_type = "ids")
    expect_equal(result, c("A", "A", NA, NA, NA, NA, NA, NA))

    result2 <- more_than(df, group_by = "group", idents = c("G1", "G2"), id = "id", compare = "count", return_type = "ids")
    expect_equal(result, result2)
})

test_that("more_than function works with return_type = 'interdf'", {
    df <- data.frame(
        id = c("A", "A", "B", "B", "C", "C", "D", "D"),
        group = c("G1", "G2", "G1", "G2", "G1", "G2", "G1", "G2"),
        count = c(10, 5, 3, 8, 7, 7, 2, 2)
    )

    result <- more_than(df, group_by = group, idents = c("G1", "G2"), id = id, compare = count, return_type = "interdf")
    expect_equal(as.data.frame(result), data.frame(
        id = c("A", "C", "B", "D"),
        ident_1 = c(10, 7, 3, 2),
        ident_2 = c(5, 7, 8, 2),
        predicate = c(TRUE, FALSE, FALSE, FALSE),
        sum = c(15, 14, 11, 4),
        diff = c(5, 0, -5, 0)
    ))

    result2 <- more_than(df, group_by = "group", idents = c("G1", "G2"), id = "id", compare = "count", return_type = "interdf")
    expect_equal(result, result2)
})

test_that("more_than function works with return_type = 'df'", {
    df <- data.frame(
        id = c("A", "A", "B", "B", "C", "C", "D", "D"),
        group = c("G1", "G2", "G1", "G2", "G1", "G2", "G1", "G2"),
        count = c(10, 5, 3, 8, 7, 7, 2, 2)
    )

    result <- more_than(df, group_by = group, idents = c("G1", "G2"), id = id, compare = count, return_type = "df")
    expect_equal(as.data.frame(result), data.frame(
        id = c("A", "A", "B", "B", "C", "C", "D", "D"),
        group = c("G1", "G2", "G1", "G2", "G1", "G2", "G1", "G2"),
        count = c(10, 5, 3, 8, 7, 7, 2, 2),
        .selected = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
    ))

    result2 <- more_than(df, group_by = "group", idents = c("G1", "G2"), id = "id", compare = "count", return_type = "df")
    expect_equal(result, result2)
})

test_that("more_than works with subset", {
    df <- data.frame(
        id = c("A", "A", "B", "B", "C", "C", "D", "D"),
        group = c("G1", "G2", "G1", "G2", "G1", "G2", "G1", "G2"),
        count = c(10, 5, 8, 3, 7, 7, 2, 2)
    )

    result <- more_than(df, group_by = group, idents = c("G1", "G2"), id = id, compare = count, return_type = "uids", subset = count >= 5)
    expect_equal(result, c("A"))

    result2 <- more_than(df, group_by = "group", idents = c("G1", "G2"), id = "id", compare = "count", return_type = "uids", subset = "count >= 5")
    expect_equal(result, result2)
})

test_that("more_than works with mutate", {
    df <- data.frame(
        id = c("A", "A", "B", "B", "C", "C", "D", "D"),
        group = c("G1", "G2", "G1", "G2", "G1", "G2", "G1", "G2"),
        count = c(10, 5, 3, 8, 7, 7, 2, 2)
    )
    df <- dplyr::mutate(df, selected = more_than(group_by = group, idents = c("G1", "G2"), id = id, compare = count, return_type = "ids"))
    expect_equal(df$selected, c("A", "A", NA, NA, NA, NA, NA, NA))
})

test_that("less_than function works correctly", {
    df <- data.frame(
        id = c("A", "A", "B", "B", "C", "C", "D", "D"),
        group = c("G1", "G2", "G1", "G2", "G1", "G2", "G1", "G2"),
        count = c(10, 5, 3, 8, 7, 7, 2, 2)
    )

    result <- less_than(df, group_by = group, idents = c("G1", "G2"), id = id, compare = count, return_type = "uids")
    expect_equal(result, c("B"))

    result2 <- less_than(df, group_by = "group", idents = c("G1", "G2"), id = "id", compare = "count", return_type = "uids")
    expect_equal(result, result2)
})

test_that("less_than function works with mutate", {
    df <- data.frame(
        id = c("A", "A", "B", "B", "C", "C", "D", "D"),
        group = c("G1", "G2", "G1", "G2", "G1", "G2", "G1", "G2"),
        count = c(10, 5, 3, 8, 7, 7, 2, 2)
    )
    df <- dplyr::mutate(df, selected = less_than(group_by = group, idents = c("G1", "G2"), id = id, compare = count, return_type = "ids"))
    expect_equal(df$selected, c(NA, NA, "B", "B", NA, NA, NA, NA))
})

test_that("emerged function works correctly", {
    df <- data.frame(
        id = c("A", "A", "B", "B", "C", "C", "D", "D"),
        group = c("G1", "G2", "G1", "G2", "G1", "G2", "G1", "G2"),
        count = c(10, 0, 3, 8, 7, 0, 2, 2)
    )

    result <- emerged(df, group_by = group, idents = c("G1", "G2"), id = id, compare = count, return_type = "uids", order = sum)
    expect_equal(result, c("C", "A"))

    result2 <- emerged(df, group_by = "group", idents = c("G1", "G2"), id = "id", compare = "count", return_type = "uids", order = "sum")
    expect_equal(result, result2)
})

test_that("emerged function works with mutate", {
    df <- data.frame(
        id = c("A", "A", "B", "B", "C", "C", "D", "D"),
        group = c("G1", "G2", "G1", "G2", "G1", "G2", "G1", "G2"),
        count = c(10, 0, 3, 8, 7, 0, 2, 2)
    )
    df <- dplyr::mutate(df, selected = emerged(group_by = group, idents = c("G1", "G2"), id = id, compare = count, return_type = "ids", order = sum))
    expect_equal(df$selected, c("A", "A", NA, NA, "C", "C", NA, NA))
})

test_that("vanished function works correctly", {
    df <- data.frame(
        id = c("A", "A", "B", "B", "C", "C", "D", "D"),
        group = c("G1", "G2", "G1", "G2", "G1", "G2", "G1", "G2"),
        count = c(0, 5, 3, 8, 0, 7, 2, 2)
    )

    result <- vanished(df, group_by = group, idents = c("G1", "G2"), id = id, compare = count, return_type = "uids")
    expect_equal(result, c("C", "A"))

    result2 <- vanished(df, group_by = "group", idents = c("G1", "G2"), id = "id", compare = "count", return_type = "uids")
    expect_equal(result, result2)
})

test_that("vanished function works with mutate", {
    df <- data.frame(
        id = c("A", "A", "B", "B", "C", "C", "D", "D"),
        group = c("G1", "G2", "G1", "G2", "G1", "G2", "G1", "G2"),
        count = c(0, 5, 3, 8, 0, 7, 2, 2)
    )
    df <- dplyr::mutate(df, selected = vanished(group_by = group, idents = c("G1", "G2"), id = id, compare = count, return_type = "ids"))
    expect_equal(df$selected, c("A", "A", NA, NA, "C", "C", NA, NA))
})

test_that("paired function works correctly", {
    df <- tibble(
        id = c("A", "A", "B", "B", "C", "C", "D", "D"),
        compare = c(1, 2, 1, 1, 1, 2, 1, 2)
    )

    result <- paired(df, id, compare, 2)
    expect_equal(result, c("A", "B", "C", "D"))

    result <- paired(df, id, compare, c(1, 2))
    expect_equal(result, c("A", "C", "D"))

    result <- paired(df, "id", "compare", c(1, 2), uniq = FALSE)
    expect_equal(result, c("A", "A", NA, NA, "C", "C", "D", "D"))
})

test_that("paired: errors when id not found", {
    df <- tibble(
        id = c("A", "A", "B", "B", "C", "C", "D", "D"),
        compare = c(1, 2, 1, 1, 1, 2, 1, 2)
    )

    expect_error(paired(df, "not_id", "compare", 2))
})

test_that("paired: errors when compare not found", {
    df <- tibble(
        id = c("A", "A", "B", "B", "C", "C", "D", "D"),
        compare = c(1, 2, 1, 1, 1, 2, 1, 2)
    )

    expect_error(paired(df, "id", "not_compare", 2))
})

test_that("paired: errors when idents is empty or not index", {
    df <- tibble(
        id = c("A", "A", "B", "B", "C", "C", "D", "D"),
        compare = c(1, 2, 1, 1, 1, 2, 1, 2)
    )

    expect_error(paired(df, "id", "compare", 0))
    expect_error(paired(df, "id", "compare", NULL))
})

test_that("paired: works with mutate", {
    df <- tibble(
        id = c("A", "A", "B", "B", "C", "C", "D", "D"),
        compare = c(1, 2, 1, 1, 1, 2, 1, 2)
    )

    df <- dplyr::mutate(df, selected = paired(id = id, compare = compare, idents = 2, uniq = FALSE))
    expect_equal(df$selected, c("A", "A", "B", "B", "C", "C", "D", "D"))
})

test_that("top function returns correct unique ids", {
    df <- tibble(
        id = c("A", "B", "C", "D", "E", "F", "G", "H"),
        value = c(10, 20, 30, 40, 50, 60, 70, 80)
    )
    result <- top(df, id, n = 2, compare = value, return_type = "uids")
    expect_equal(result, c("G", "H"))
})

test_that("top function returns correct ids with ties", {
    df <- tibble(
        id = c("A", "B", "C", "D", "E", "F", "G", "H"),
        value = c(10, 20, 30, 40, 50, 60, 80, 80)
    )
    result <- top(df, id, n = 1, compare = value, with_ties = TRUE, return_type = "uids")
    expect_equal(result, c("G", "H"))
})

test_that("top function returns correct subset of data frame", {
    df <- tibble(
        id = c("A", "B", "C", "D", "E", "F", "G", "H"),
        value = c(10, 20, 30, 40, 50, 60, 80, 80)
    )
    result <- top(df, "id", n = 2, compare = "value", return_type = "subdf")
    expect_equal(as.data.frame(result), data.frame(
        id = c("G", "H"),
        value = c(80, 80)
    ))
})

test_that("top function returns correct data frame with .selected column", {
    df <- tibble(
        id = c("A", "B", "C", "D", "E", "F", "G", "H"),
        value = c(10, 20, 30, 40, 50, 60, 80, 80)
    )
    result <- top(df, "id", n = 2, compare = "value", return_type = "df")
    expect_equal(as.data.frame(result), data.frame(
        id = c("A", "B", "C", "D", "E", "F", "G", "H"),
        value = c(10, 20, 30, 40, 50, 60, 80, 80),
        .selected = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
    ))
})

test_that("top function returns correct intermediate data frame", {
    df <- tibble(
        id = c("A", "B", "C", "D", "E", "F", "G", "H"),
        value = c(10, 20, 30, 40, 50, 60, 80, 80)
    )
    result <- top(df, "id", n = 2, compare = "value", return_type = "interdf")
    expect_equal(as.data.frame(result), data.frame(
        id = c("A", "B", "C", "D", "E", "F", "G", "H"),
        value = c(10, 20, 30, 40, 50, 60, 80, 80),
        predicate = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
    ))
})

test_that("top function handles split_by correctly", {
    df <- tibble(
        id = c("A", "B", "C", "D", "E", "F", "G", "H"),
        value = c(10, 20, 30, 40, 50, 60, 80, 80),
        split = c("S1", "S1", "S1", "S1", "S2", "S2", "S2", "S2")
    )
    result <- top(df, id, n = 2, compare = value, split_by = split, return_type = "uids")
    expect_equal(result, c("C", "D", "G", "H"))

    result <- top(df, id, n = 2, compare = value, split_by = "split", return_type = "ids")
    expect_equal(result, c(NA, NA, "C", "D", NA, NA, "G", "H"))
})

test_that("top function handles subset correctly", {
    df <- tibble(
        id = c("A", "B", "C", "D", "E", "F", "G", "H"),
        value = c(10, 20, 30, 40, 50, 60, 80, 80)
    )
    result <- top(df, id, n = 2, compare = value, subset = value < 50, return_type = "uids")
    expect_equal(result, c("C", "D"))

    result2 <- top(df, id, n = 2, compare = value, subset = "value < 50", return_type = "uids")
    expect_equal(result, result2)
})

test_that("top function handles percentage correctly", {
    df <- tibble(
        id = c("A", "B", "C", "D", "E", "F", "G", "H"),
        value = c(10, 20, 30, 40, 50, 60, 80, 80)
    )
    result <- top(df, id, n = 0.25, compare = value, return_type = "uids")
    expect_equal(result, c("G", "H"))
})

test_that("top function returns all entities when n is 0", {
    df <- tibble(
        id = c("A", "B", "C", "D", "E", "F", "G", "H"),
        value = c(10, 20, 30, 40, 50, 60, 80, 80)
    )
    result <- top(df, id, n = 0, compare = value, return_type = "uids")
    expect_equal(result, c("A", "B", "C", "D", "E", "F", "G", "H"))
})

test_that("top: errors when columns not found", {
    df <- tibble(
        id = c("A", "B", "C", "D", "E", "F", "G", "H"),
        value = c(10, 20, 30, 40, 50, 60, 80, 80)
    )

    expect_error(top(df, "not_id", n = 2, compare = "value"))
    expect_error(top(df, "id", n = 2, compare = "not_value"))
    expect_error(top(df, "id", n = 2, compare = "value", split_by = "not_split"))
})

test_that("top: works with count", {
    df <- tibble(id = c("A", "A", "B", "B", "B", "C", "C", "C", "D", "D", "D", "D"))
    result <- top(df, id, n = 2, compare = .n, return_type = "uids", with_ties = TRUE)
    expect_equal(result, c("B", "C", "D"))
})

test_that("top: works with mutate", {
    df <- tibble(id = c("A", "A", "B", "B", "B", "C", "C", "C", "D", "D", "D", "D"))
    df <- dplyr::mutate(df, selected = top(id = id, n = 2, compare = .n, return_type = "ids", with_ties = TRUE))
    expect_equal(df$selected, c(NA, NA, "B", "B", "B", "C", "C", "C", "D", "D", "D", "D"))
})
