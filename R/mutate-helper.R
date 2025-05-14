#' Internal helper function to compare the size of two groups.
#'
#' @inheritParams more_than
#' @param fun The way to compare between groups. Either `"more_than"`,
#' `"less_than"`, `"emerged"` or `"vanished"`.
#' @return A vector of ids or a data frame with the selected entities.
#' @keywords internal
.size_compare <- function(
    df, group_by, idents, id, fun, compare = ".n", split_by = NULL, order = "desc(sum)",
    subset = NULL, return_type = c("uids", "ids", "subdf", "df", "interdf"),
    include_zeros = FALSE
) {
    stopifnot("'idents' must be a vector of length 1 or 2" = length(idents) %in% 1:2)
    stopifnot("'group_by' must be a column name in 'df'" = group_by %in% colnames(df))
    stopifnot("'id' must be a column name in 'df'" = id %in% colnames(df))
    if (!identical(compare, ".n")) {
        stopifnot("'compare' must be a column name in 'df'" = compare %in% colnames(df))
    }
    ident_1 <- idents[1]
    ident_2 <- ifelse(length(idents) == 2, idents[2], "<NULL>")
    compare_is_count <- identical(compare, ".n")
    fun <- match.arg(fun, c("more_than", "less_than", "emerged", "vanished"))
    return_type <- match.arg(return_type)

    .predicate <- function(size1, size2) {
        if (fun == "more_than") {
            size1 > size2 && (include_zeros || size2 > 0)
        } else if (fun == "less_than") {
            size1 < size2 && (include_zeros || size1 > 0)
        } else if (fun == "emerged") {
            size1 > 0 && size2 == 0
        } else if (fun == "vanished") {
            size1 == 0 && size2 > 0
        }
    }
    if (!is.null(subset) && !isTRUE(subset)) {
        interdf <- df %>% filter(!!parse_expr(subset))
    } else {
        interdf <- df
    }
    interdf <- interdf %>%
        drop_na(!!sym(id)) %>%
        mutate(.group = if_else(
            !!sym(group_by) == ident_1,
            "ident_1",
            if_else(ident_2 != "<NULL>" & !!sym(group_by) != ident_2, NA, "ident_2")
        )) %>%
        drop_na(!!sym(".group"))

    if (is.null(split_by)) {
        interdf <- interdf %>% dplyr::group_by(!!sym(id), !!sym(".group"))
    } else {
        interdf <- interdf %>% dplyr::group_by(!!sym(split_by), !!sym(id), !!sym(".group"))
    }

    if (compare_is_count) {
        interdf <- interdf %>% summarise(.n = n(), .groups = "drop")
    } else {
        interdf <- interdf %>% summarise(.n = first(!!sym(compare)), .groups = "drop")
    }

    interdf <- interdf %>%
        pivot_wider(names_from = !!sym(".group"), values_from = !!sym(".n")) %>%
        replace_na(list(ident_1 = 0, ident_2 = 0)) %>%
        rowwise() %>%
        # add the predicates, sums and diffs
        mutate(
            predicate = .predicate(ident_1, ident_2),
            sum = ident_1 + ident_2,
            diff = ident_1 - ident_2
        ) %>%
        ungroup()

    if (!is.null(order)) {
        interdf <- interdf %>% arrange(!!parse_expr(order))
    }

    if (return_type == "interdf") {
        return(interdf)
    }
    interdf <- interdf %>% filter(!!sym("predicate"))
    if (return_type == "uids") {
        return(unique(interdf[[id]]))
    }
    interdf <- interdf %>% select(!!!syms(c(split_by, id))) %>% mutate(.selected = TRUE)
    out <- left_join(df, interdf, by = c(split_by, id), multiple = "first")
    if (return_type == "ids") {
        ids <- out[[id]]
        ids[is.na(out$.selected)] <- NA
        ids
    } else if (return_type == "subdf") {
        out %>% filter(!is.na(!!sym(".selected"))) %>% select(-!!sym(".selected"))
    } else {  # df
        out %>% replace_na(list(.selected = FALSE))
    }
}

#' Get more_than, less_than, emerged, vanished, paired or top entities from a data frame.
#'
#' * `more_than`: Select entities that have more counts in the first group than the second group.
#' * `less_than`: Select entities that have less counts in the first group than the second group.
#' * `emerged`: Select entities that have counts in the first group but not in the second group.
#' * `vanished`: Select entities that have counts in the second group but not in the first group.
#' * `paired`: Select entities that have the same counts in both groups.
#' * `top`: Select the top entities from a data frame based on the number of entities in each group.
#'
#' @rdname mutate-helper-1
#'
#' @param df The data frame
#' @param group_by The column name in the data frame to group the entities.
#' It could be a quoted string or a bare variable, and defines the groups of entities
#' for comparison.
#' @param idents The groups of entities to compare (values in `group_by` column).
#' Either length 1 (`ident_1`) or length 2 (`ident_1` and `ident_2`).
#' If length 1, the rest of the cells with non-NA values in `group_by` will
#' be used as `ident_2`.
#' @param subset An expression to subset the cells, will be passed to
#' `dplyr::filter()`. Default is `NULL` (no filtering).
#' @param split_by A column name in data frame to split the entities.
#' Each comparison will be done for each split in this column.
#' @param id The column name in data frame to mark the entities for the same group.
#' @param compare Either a (numeric) column name (i.e. `Count`)
#' in data frame to compare between groups, or `.n` to compare the
#' number (count) of entities in each group.
#' If a column name is given, only the first value of the entities from the same `id`
#' will be used. So make sure that the values are the same for each group (`id`).
#' @param return_type The type of the returned value. Default is `uids`.
#' It could be one of
#' * `uids`: return the unique ids of the selected entities
#' * `ids`: return the ids of all entities in the same order as in `df`, where the
#'    non-selected ids will be `NA`
#' * `subdf`: return a subset of `df` with the selected entities
#' * `df`: return the original `df` with a new logical column `.selected` to mark
#'    the selected entities
#' * `interdf`: return the intermediate data frame with the id column, `ident_1`,
#'   `ident_2`, `predicate`, `sum`, `diff` and the split_by column if provided.
#' @param order An expression to order the intermediate data frame before returning
#' the final result. Default is `NULL`. It does not work for `subdf` and `df`.
#' @param include_zeros Whether to include the zero entities in the other group for
#' `more_than` and `less_than` comparisons. Default is `FALSE`.
#' By default, the zero entities will be excluded, meaning that the entities must
#' exist in both groups to be selected.
#'
#' @return Depending on the `return_type`, the function will return different values.
#' * `uids`: a vector of unique ids of the selected entities
#' * `ids`: a vector of ids of all entities in the same order as in `df`, where the
#'   non-selected ids will be `NA`
#' * `subdf`: a subset of `df` with the selected entities
#' * `df`: the original `df` with a new logical column `.selected` to mark the selected
#'    entities
#' * `interdf`: the intermediate data frame with the id column, `ident_1`, `ident_2`,
#'   `predicate`, `sum`, and `diff` and the split_by column if provided.
#'
#' @importFrom rlang parse_expr sym
#' @importFrom dplyr filter mutate n first if_else rowwise arrange left_join select
#' @importFrom tidyr drop_na pivot_wider replace_na
#' @examples
#' df <- data.frame(
#'     id = c("A", "A", "A", "B", "B", "B", "C", "C", "D", "D"),
#'     group = c("G1", "G1", "G2", "G1", "G2", "G2", "G1", "G2", "G1", "G2"),
#'     count = rep(1, 10),
#'     split = c("S1", "S2", "S1", "S1", "S2", "S1", "S1", "S2", "S1", "S2")
#' )
#' more_than(df, group_by = group, idents = c("G1", "G2"), id = id, compare = count,
#'   return_type = "uids")
#' more_than(df, group_by = group, idents = c("G1", "G2"), id = id, compare = ".n",
#'   return_type = "uids")
#' more_than(df, group_by = group, split_by = split, idents = c("G1", "G2"), id = id,
#'   compare = count, return_type = "ids")
#' more_than(df, group_by = group, idents = c("G1", "G2"), id = id, compare = count,
#'   return_type = "subdf")
#' more_than(df, group_by = group, idents = c("G1", "G2"), id = id, compare = count,
#'   return_type = "ids")
#' more_than(df, group_by = group, idents = c("G1", "G2"), id = id, compare = count,
#'   return_type = "interdf")
#' more_than(df, group_by = group, idents = c("G1", "G2"), id = id, compare = count,
#'   return_type = "df")
#' more_than(df, group_by = group, idents = c("G1", "G2"), id = id,
#'   return_type = "uids", subset = id %in% c("A", "B"))
#' dplyr::mutate(df, selected = more_than(group_by = group, idents = c("G1", "G2"),
#'   id = id, compare = count, return_type = "ids"))
#' less_than(df, group_by = group, idents = c("G1", "G2"), id = id, compare = count,
#'   return_type = "uids")
#' emerged(df, group_by = group, idents = c("G1", "G2"), id = id, compare = count,
#'   return_type = "uids", order = sum)
#' vanished(df, group_by = group, idents = c("G1", "G2"), id = id, compare = count,
#'   return_type = "uids")
#' @export
#' @importFrom rlang as_name as_label enquo enexpr expr_deparse
#' @importFrom dplyr across everything
more_than <- function(
    df = ".", group_by, idents, id, subset = NULL, split_by = NULL,
    compare = ".n", return_type = c("uids", "ids", "subdf", "df", "interdf"),
    order = "desc(sum)", include_zeros = FALSE
) {
    lbl <- as_label(enquo(df))
    if (identical(lbl, ".") || identical(lbl, '"."')) {
        df <- across(everything())
    }
    subset_expr <- deparse(substitute(subset))
    subset <- tryCatch({
        if (!is.character(subset) && !is.null(subset)) stop()  # nocov
        subset
    }, error = function(e) {
        sub('^"|"$', '', subset_expr)
    })
    order_expr <- deparse(substitute(order))
    order <- tryCatch({
        if (!is.character(order) && !is.null(order)) stop()  # nocov
        order  # nocov
    }, error = function(e) {
        sub('^"|"$', '', order_expr)
    })
    split_by <- tryCatch(as_name(enquo(split_by)), error = function(e) NULL)

    .size_compare(
        df = df,
        group_by = as_name(enquo(group_by)),
        idents = idents,
        id = as_name(enquo(id)),
        fun = "more_than",
        compare = as_name(enquo(compare)),
        split_by = split_by,
        order = order,
        subset = subset,
        return_type = return_type,
        include_zeros = include_zeros
    )
}

#' @export
#' @rdname mutate-helper-1
#' @importFrom rlang as_name as_label enquo enexpr expr_deparse
#' @importFrom dplyr across everything
less_than <- function(
    df = ".", group_by, idents, id, subset = NULL, split_by = NULL,
    compare = ".n", return_type = c("uids", "ids", "subdf", "df", "interdf"),
    order = "desc(sum)", include_zeros = FALSE
) {
    lbl <- as_label(enquo(df))
    if (identical(lbl, ".") || identical(lbl, '"."')) {
        df <- across(everything())
    }
    subset_expr <- deparse(substitute(subset))
    subset <- tryCatch({
        if (!is.character(subset) && !is.null(subset)) stop()  # nocov
        subset
    }, error = function(e) {
        sub('^"|"$', '', subset_expr)  # nocov
    })
    order_expr <- deparse(substitute(order))
    order <- tryCatch({
        if (!is.character(order) && !is.null(order)) stop()  # nocov
        order  # nocov
    }, error = function(e) {
        sub('^"|"$', '', order_expr)
    })
    split_by <- tryCatch(as_name(enquo(split_by)), error = function(e) NULL)

    .size_compare(
        df = df,
        group_by = as_name(enquo(group_by)),
        idents = idents,
        id = as_name(enquo(id)),
        fun = "less_than",
        compare = as_name(enquo(compare)),
        split_by = split_by,
        order = order,
        subset = subset,
        return_type = return_type,
        include_zeros = include_zeros
    )
}

#' @export
#' @rdname mutate-helper-1
#' @importFrom rlang as_name as_label enquo enexpr
#' @importFrom dplyr across everything
emerged <- function(
    df = ".", group_by, idents, id, subset = NULL, split_by = NULL,
    compare = ".n", return_type = c("uids", "ids", "subdf", "df", "interdf"),
    order = "desc(sum)", include_zeros = FALSE
) {
    lbl <- as_label(enquo(df))
    if (identical(lbl, ".") || identical(lbl, '"."')) {
        df <- across(everything())
    }
    subset_expr <- deparse(substitute(subset))
    subset <- tryCatch({
        if (!is.character(subset) && !is.null(subset)) stop()  # nocov
        subset
    }, error = function(e) {
        sub('^"|"$', '', subset_expr)  # nocov
    })
    order_expr <- deparse(substitute(order))
    order <- tryCatch({
        if (!is.character(order) && !is.null(order)) stop()
        order
    }, error = function(e) {
        sub('^"|"$', '', order_expr)
    })
    split_by <- tryCatch(as_name(enquo(split_by)), error = function(e) NULL)

    .size_compare(
        df = df,
        group_by = as_name(enquo(group_by)),
        idents = idents,
        id = as_name(enquo(id)),
        fun = "emerged",
        compare = as_name(enquo(compare)),
        split_by = split_by,
        order = order,
        subset = subset,
        return_type = return_type,
        include_zeros = include_zeros
    )
}

#' @export
#' @rdname mutate-helper-1
#' @importFrom rlang as_name as_label enquo enexpr
#' @importFrom dplyr across everything
vanished <- function(
    df = ".", group_by, idents, id, subset = NULL, split_by = NULL,
    compare = ".n", return_type = c("uids", "ids", "subdf", "df", "interdf"),
    order = "desc(sum)", include_zeros = FALSE
) {
    lbl <- as_label(enquo(df))
    if (identical(lbl, ".") || identical(lbl, '"."')) {
        df <- across(everything())
    }
    subset_expr <- deparse(substitute(subset))
    subset <- tryCatch({
        if (!is.character(subset) && !is.null(subset)) stop()  # nocov
        subset
    }, error = function(e) {
        sub('^"|"$', '', subset_expr)  # nocov
    })
    order_expr <- deparse(substitute(order))
    order <- tryCatch({
        if (!is.character(order) && !is.null(order)) stop()  # nocov
        order  # nocov
    }, error = function(e) {
        sub('^"|"$', '', order_expr)
    })
    split_by <- tryCatch(as_name(enquo(split_by)), error = function(e) NULL)

    .size_compare(
        df = df,
        group_by = as_name(enquo(group_by)),
        idents = idents,
        id = as_name(enquo(id)),
        fun = "vanished",
        compare = as_name(enquo(compare)),
        split_by = split_by,
        order = order,
        subset = subset,
        return_type = return_type,
        include_zeros = include_zeros
    )
}

#' @rdname mutate-helper-1
#' @param df The data frame.
#' @param id The column name in `df` for the ids to be returned in the
#'   final output
#' @param compare The column name in `df` to compare the values for each
#'   id in `id`.
#' @param idents The values in `compare` to compare. It could be either an
#'   an integer or a vector. If it is an integer, the number of values in
#'   `compare` must be the same as the integer for the `id` to be regarded
#'   as paired. If it is a vector, the values in `compare` must be the same
#'   as the values in `idents` for the `id` to be regarded as paired.
#' @param uniq Whether to return unique ids or not. Default is `TRUE`.
#'   If `FALSE`, you can mutate the meta data frame with the returned ids.
#'   Non-paired ids will be `NA`.
#' @return A vector of paired ids (in `id` column)
#' @examples
#' df <- data.frame(
#'     id = c("A", "A", "B", "B", "C", "C", "D", "D"),
#'     compare = c(1, 2, 1, 1, 1, 2, 1, 2)
#' )
#' paired(df, id, compare, 2)
#' paired(df, id, compare, c(1, 2))
#' paired(df, id, compare, c(1, 2), uniq = FALSE)
#' @export
#' @importFrom stats na.omit
#' @importFrom rlang as_label as_name enquo sym is_empty
#' @importFrom dplyr across everything add_count mutate if_else group_by ungroup
paired <- function(
    df = ".",
    id,
    compare,
    idents = 2,
    uniq = TRUE) {
    lbl <- as_label(enquo(df))
    if (identical(lbl, ".") || identical(lbl, '"."')) {
        df <- across(everything())
    }

    id <- enquo(id)
    compare <- enquo(compare)
    if (is_empty(attr(id, ".Environment"))) {
        id <- sym(as_name(id))
    }
    if (is_empty(attr(compare, ".Environment"))) {
        compare <- sym(as_name(compare))
    }
    if (!as_name(id) %in% colnames(df)) {
        stop(paste0(
            '`id` must be a column name in df. Got "',
            as_name(id),
            '"'
        ))
    }
    if (!as_name(compare) %in% colnames(df)) {
        stop(paste0(
            '`compare` must be a column name in df. Got "',
            as_name(compare),
            '"'
        ))
    }

    if (is.numeric(idents) && length(idents) == 1) {
        if (idents <= 1) {
            stop(paste0(
                "`idents` must be greater than 1. Got ",
                idents
            ))
        }
        out <- df %>%
            add_count(!!id, name = "..count") %>%
            mutate(..paired = if_else(!!sym("..count") == idents, !!id, NA))
    } else {
        if (length(idents) <= 1) {
            stop(paste0(
                "`idents` must be a vector with length greater than 1. Got ",
                length(idents)
            ))
        }
        out <- df %>%
            group_by(!!id) %>%
            mutate(
                ..paired = if_else(
                    rep(setequal(!!compare, idents), n()),
                    !!id,
                    NA
                )
            ) %>%
            ungroup()
    }

    out <- out %>% pull("..paired")
    if (uniq) {
        return(out %>% na.omit() %>% unique() %>% as.vector())
    } else {
        return(out)
    }
}

#' @rdname mutate-helper-1
#' @param df The data frame. Use `.` if the function is called in a dplyr pipe.
#' @param id The column name in `df` for the groups.
#' @param compare The column name in `df` to compare the values for each group.
#'   It could be either a numeric column or `.n` to compare the number of
#'   entities in each group. If a column is passed, the values in the column
#'   must be numeric and the same in each group. This won't be checked.
#' @param n The number of top entities to return. if `n` < 1, it will be
#'  regarded as the percentage of the total number of entities in each group
#'  (after subsetting or each applied).
#'  Specify 0 to return all entities.
#' @param subset An expression to subset the entities, will be passed to
#'   `dplyr::filter()`. Default is `TRUE` (no filtering).
#' @param with_ties Whether to return all entities with the same size as the
#'  last entity in the top list. Default is `FALSE`.
#' @param split_by A column name (without quotes) in metadata to split the cells.
#' @param return_type The type of the returned value. Default is `uids`.
#' It could be one of
#' * `uids`: return the unique ids of the selected entities
#' * `ids`: return the ids of all entities in the same order as in `df`, where the
#'   non-selected ids will be `NA`
#' * `subdf`: return a subset of `df` with the selected entities
#' * `df`: return the original `df` with a new logical column `.out` to mark
#'  the selected entities
#' * `interdf`: return the intermediate data frame with the id column, `<compare>`,
#'  `predicate` and the split_by column if provided.
#' @return Depending on the `return_type`, the function will return different values.
#' @export
#' @examples
#' df <- data.frame(
#'     id = c("A", "B", "C", "D", "E", "F", "G", "H"),
#'     value = c(10, 20, 30, 40, 50, 60, 80, 80)
#' )
#' top(df, id, n = 1, compare = value, with_ties = TRUE, return_type = "uids")
#' top(df, "id", n = 2, compare = "value", return_type = "subdf")
#' top(df, "id", n = 2, compare = "value", return_type = "df")
#' top(df, "id", n = 2, compare = "value", return_type = "interdf")
#' top(df, id, n = 0.25, compare = value, return_type = "uids")
#' top(df, id, n = 0, compare = value, return_type = "uids")
#'
#' df <- data.frame(id = c("A", "A", "B", "B", "B", "C", "C", "C", "D", "D", "D", "D"))
#' top(df, id, n = 2, compare = ".n", return_type = "uids", with_ties = TRUE)
#' dplyr::mutate(df, selected = top(id = id, n = 2, compare = ".n", return_type = "ids",
#'   with_ties = TRUE))
#' @importFrom rlang parse_expr
#' @importFrom dplyr filter group_by summarise n first slice_max left_join select pull
#' @importFrom tidyr drop_na
top <- function(
    df = ".", id, n = 10, compare = ".n", subset = NULL, with_ties = FALSE,
    split_by = NULL, return_type = c("uids", "ids", "subdf", "df", "interdf")
){
    return_type <- match.arg(return_type)
    lbl <- as_label(enquo(df))
    if (identical(lbl, ".") || identical(lbl, '"."')) {
        df <- across(everything())
    }

    id <- enquo(id)
    compare <- enquo(compare)
    subset_expr <- deparse(substitute(subset))
    subset <- tryCatch({
        if (!is.character(subset) && !is.null(subset)) stop()  # nocov
        subset
    }, error = function(e) {
        sub('^"|"$', '', subset_expr)  # nocov
    })

    split_by <- tryCatch(enquo(split_by), error = function(e) NULL)
    if (is_empty(attr(id, ".Environment"))) {
        id <- sym(as_name(id))
    }
    if (is_empty(attr(compare, ".Environment"))) {
        compare <- sym(as_name(compare))
    }

    if (!as_name(id) %in% colnames(df)) {
        stop(paste0(
            '`id` must be a column name in df. Got "',
            as_name(id),
            '"'
        ))
    }
    if (!as_name(compare) %in% colnames(df) && as_name(compare) != ".n") {
        stop(paste0(
            '`compare` must be a column name in df. Got "',
            as_name(compare),
            '"'
        ))
    }
    if (is_empty(attr(split_by, ".Environment"))) {
        if (as_label(split_by) == "NULL") {
            split_by <- NULL
        } else {
            split_by <- sym(as_name(split_by))
        }
    }
    if (!is.null(split_by) && !as_name(split_by) %in% colnames(df)) {
        stop(paste0(
            '`split_by` must be a column name in df. Got "',
            as_name(split_by),
            '"'
        ))
    }

    if (is.null(subset)) {
        interdf <- df %>% drop_na(!!id)
    } else {
        interdf <- df %>% filter(!!parse_expr(subset)) %>% drop_na(!!id)
    }

    if (is.null(split_by)) {
        interdf <- interdf %>% group_by(!!id)
    } else {
        interdf <- interdf %>% group_by(!!split_by, !!id)
    }
    if (as_name(compare) == ".n") {
        interdf <- interdf %>% summarise(!!compare := n(), .groups = "drop")
    } else {
        interdf <- interdf %>% summarise(!!compare := first(!!compare), .groups = "drop")
    }
    if (!is.null(split_by)) {
        interdf <- interdf %>% group_by(!!split_by)
    }
    if (n > 0 && n < 1) {
        interdf2 <- interdf %>% slice_max(prop = n, order_by = !!compare, with_ties = with_ties)
    } else if (n >= 1) {
        interdf2 <- interdf %>% slice_max(n = n, order_by = !!compare, with_ties = with_ties)
    } else {
        interdf2 <- interdf
    }
    interdf2$predicate <- TRUE
    if (is.null(split_by)) {
        interdf2 <- interdf2 %>% select(!!id, !!sym("predicate"))
        interdf <- left_join(df, interdf2, by = as_name(id))
    } else {
        interdf2 <- interdf2 %>% select(!!split_by, !!id, !!sym("predicate"))
        interdf <- left_join(df, interdf2, by = c(as_name(split_by), as_name(id)))
    }
    interdf$predicate <- !is.na(interdf$predicate)

    # <split_by>, <id>, n, predicate
    if (return_type == "interdf") {
        return(interdf)
    }

    interdf <- interdf %>% filter(!!sym("predicate"))
    if (return_type == "uids") {
        return(interdf %>% pull(!!id) %>% unique())
    }

    if (is.null(split_by)) {
        interdf <- interdf %>% select(!!id) %>% mutate(.selected = TRUE)
        out <- left_join(df, interdf, by = as_name(id), multiple = "first")
    } else {
        interdf <- interdf %>% select(!!split_by, !!id) %>% mutate(.selected = TRUE)
        out <- left_join(df, interdf, by = c(as_name(split_by), as_name(id)), multiple = "first")
    }
    if (return_type == "ids") {
        ids <- pull(out, !!id)
        ids[is.na(out$.selected)] <- NA
        ids
    } else if (return_type == "subdf") {
        out %>% filter(!is.na(!!sym(".selected"))) %>% select(-!!sym(".selected"))
    } else {  # df
        out %>% replace_na(list(.selected = FALSE))
    }
}
