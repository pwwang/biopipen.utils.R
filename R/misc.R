#' backtick quoting
#'
#' @param x a character vector
#' @param force whether to force backtick quoting
#' @return backtick quoted character vector
#' @export
#' @examples
#' bQuote(c("a", "b", "c"))
#' # [1] "a" "b" "c"
#' bQuote(c("a 1", "b 2", "c 3"))
#' # [1] "`a 1`" "`b 2`" "`c 3`"
bQuote <- function(x, force = FALSE) {
    if (length(x) == 1) {
        if (nchar(x) == 0) {
            out <- ifelse(force, "``", "")
        } else if (grepl("^`.*`$", x)) {
            out <- x
        } else if (grepl("^[a-zA-Z_][a-zA-Z0-9._]*$", x)) {
            out <- ifelse(force, paste0("`", x, "`"), x)
        } else if (nchar(x) == 1) {
            out <- ifelse(identical(x, "`"), paste0("`\\", x, "`"), paste0("`", x, "`"))
        } else if (grepl("`", x, fixed = TRUE)) {
            stop("Cannot backtick quote a string containing backticks for now")
        } else {
            out <- paste0("`", x, "`")
        }
        if (!is.null(names(x))) {
            names(out) <- names(x)
        }
        return(out)
    } else {
        out <- sapply(x, bQuote, force = force)
        if (is.null(names(x))) {
            unname(out)
        } else {
            names(out) <- names(x)
            out
        }
    }
}

#' Slugify strings
#'
#' @param x strings to slugify
#' @param non_alphanum_replace Replace non-alphanumeric characters
#' @param collapse_replace Collapse consecutive non-alphanumeric character replacements
#' @param tolower Convert to lowercase
#' @return slugified strings
#' @export
#' @importFrom stats setNames
slugify <- function(x, non_alphanum_replace = "-", collapse_replace = TRUE, tolower = FALSE) {
    if (length(x) == 0) {
        return(x)
    }
    if (length(x) == 1) {
        xnames <- names(x)
        subs <- list(
            "\u0161" = "s", "\u0153" = "oe", "\u017e" = "z", "\u00df" = "ss", "\u00fe" = "y", "\u00e0" = "a",
            "\u00e1" = "a", "\u00e2" = "a", "\u00e3" = "a", "\u00e4" = "a", "\u00e5" = "a", "\u00e6" = "ae",
            "\u00e7" = "c", "\u00e8" = "e", "\u00e9" = "e", "\u00ea" = "e", "\u00eb" = "e", "\u00ec" = "i",
            "\u00ed" = "i", "\u00ee" = "i", "\u00ef" = "i", "\u00f0" = "d", "\u00f1" = "n", "\u00f2" = "o",
            "\u00f3" = "o", "\u00f4" = "o", "\u00f5" = "o", "\u00f6" = "o", "\u00f8" = "oe", "\u00f9" = "u",
            "\u00fa" = "u", "\u00fb" = "u", "\u00fc" = "u", "\u00fd" = "y", "\u00ff" = "y", "\u011f" = "g",
            "\u0131" = "i", "\u0133" = "ij", "\u013e" = "l", "\u0148" = "n", "\u0159" = "r", "\u015f" = "s",
            "\u0165" = "t", "\u0173" = "u", "\u016f" = "u", "\u00fd" = "y", "\u017a" = "z", "\u017c" = "z",
            "\u017f" = "s", "\u03b1" = "a", "\u03b2" = "b", "\u03b3" = "g", "\u03b4" = "d", "\u03b5" = "e",
            "\u03b6" = "z", "\u03b7" = "h", "\u03b8" = "th", "\u03b9" = "i", "\u03ba" = "k", "\u03bb" = "l",
            "\u03bc" = "m", "\u03bd" = "n", "\u03be" = "x", "\u03bf" = "o", "\u03c0" = "p", "\u03c1" = "r",
            "\u03c3" = "s", "\u03c4" = "t", "\u03c5" = "u", "\u03c6" = "ph", "\u03c7" = "ch", "\u03c8" = "ps",
            "\u03c9" = "o", "\u03ac" = "a", "\u03ad" = "e", "\u03ae" = "h", "\u03af" = "i", "\u03cc" = "o",
            "\u03cd" = "u", "\u03ce" = "o", "\u03d0" = "b", "\u03d1" = "th", "\u03d2" = "y", "\u03d5" = "ph",
            "\u03d6" = "p", "\u03da" = "st", "\u03db" = "st", "\u03dc" = "f", "\u03dd" = "f", "\u03de" = "k",
            "\u03df" = "k", "\u03e0" = "k", "\u03e1" = "k", "\u03e2" = "k", "\u03e3" = "r", "\u03e4" = "s",
            "\u03e5" = "j", "\u03e6" = "th", "\u03e7" = "e", "\u03e8" = "p"
        )
        # replace latin and greek characters to the closest english character
        for (k in names(subs)) {
            x <- gsub(k, subs[[k]], x)
        }
        x <- gsub("[^[:alnum:]_]", non_alphanum_replace, x)
        if (collapse_replace) {
            x <- gsub(
                paste0(gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", non_alphanum_replace), "+"),
                non_alphanum_replace,
                x
            )
        }
        if (tolower) x <- tolower(x)
        ifelse(!is.null(xnames), setNames(x, xnames), x)
    } else {
        out <- sapply(
            x, slugify,
            non_alphanum_replace = non_alphanum_replace, collapse_replace = collapse_replace, tolower = tolower
        )
        if (is.null(names(x))) {
            unname(out)
        } else {
            names(out) <- names(x)
            out
        }
    }
}


#' Escape HTML entities
#'
#' @param text Text to escape
#' @return Escaped text
#' @export
#' @importFrom stats setNames
html_escape <- function(text) {
    if (is.null(text)) { return("") }
    if (length(text) == 1) {
        xnames <- names(text)
        text <- gsub("&", "&amp;", text)
        text <- gsub("<", "&lt;", text)
        text <- gsub(">", "&gt;", text)
        text <- gsub("\"", "&quot;", text)
        text <- gsub("'", "&#039;", text)
        ifelse(!is.null(xnames), setNames(text, xnames), text)
    } else {
        out <- sapply(text, html_escape)
        if (is.null(names(text))) {
            unname(out)
        } else {
            names(out) <- names(text)
            out
        }
    }
}



#' Extract variables from a named list
#'
#' @param x A named list
#' @param ... The names of the variables
#' named arguments are allowed to rename the variables. `b = "a"` will extract `a` and assign it to `b`
#' @param keep Keep the extracted variables in the list
#' @param env The environment to assign the extracted variables
#' @return The list with/ithout the extracted variables
#'
#' @export
extract_vars <- function(x, ..., keep = FALSE, env = parent.frame()) {
    stopifnot("extract_vars: 'x' must be a named list" = is.list(x) && !is.null(names(x)))
    vars <- list(...)
    if (is.null(names(vars))) {
        names(vars) <- unlist(vars)
    } else {
        noname_vars <- nchar(names(vars)) == 0
        names(vars)[noname_vars] <- unlist(vars[noname_vars])
    }
    nonexist_vars <- setdiff(unlist(vars), names(x))
    if (length(nonexist_vars) > 0) {
        stop(sprintf("Variable(s) '%s' not found in the list", paste(nonexist_vars, collapse = "', '")))
    }
    xvars <- x[unlist(vars)]
    names(xvars) <- names(vars)

    list2env(xvars, envir = env)
    if (!isTRUE(keep)) {
        x[unlist(vars)] <- NULL
    }

    x
}


#' Update a list with another list
#'
#' @param x A list
#' @param y A list
#' @param depth The depth to update, -1 means update all
#' @return The updated list
#' @keywords internal
list_update2 <- function(x, y, depth = -1L) {
    # Update the value in x from y
    x <- x %||% list()
    y <- y %||% list()

    for (k in names(y)) {
        if (is.null(y[[k]])) {
            x[[k]] <- NULL
            x <- c(x, y[k])
        } else if (is.list(x[[k]]) && is.list(y[[k]]) && depth != 0L) {
            x[[k]] <- list_update2(x[[k]], y[[k]], depth - 1L)
        } else {
            x[[k]] <- y[[k]]
        }
    }
    x
}

#' Update the first list based on other lists
#'
#' @param x The first list
#' @param ... The other lists
#' @param depth The depth to update, -1 means update all
#' @return The updated list
#' @export
list_update <- function(x, ..., depth = -1L) {
    for (y in list(...)) {
        x <- list_update2(x, y, depth)
    }
    x
}


#' Rename to names of a list
#'
#' @param x A list
#' @param fn A function to rename the names.
#' If the function returns NULL or FALSE, the name will be removed.
#' If the function returns TRUE, the name will be kept.
#' If the function returns a string, the name will be changed to the string.
#' The function can take 1 or 2 arguments, the first is the name, the second is the value.
#' @return The list with renamed names
#' @export
list_rename <- function(x, fn) {
    out <- list()
    for (k in names(x)) {
        # based on number of arguments of fn
        if (length(formals(fn)) == 1) {
            o <- fn(k)
        } else {
            o <- fn(k, x[[k]])
        }
        if (is.null(o) || isFALSE(o)) {
        } else if (isTRUE(o)) {
            out[[k]] <- x[[k]]
        } else {
            out[[o]] <- x[[k]]
        }
    }
    out
}


#' Get the temporary directory, without suffix.
#' It works like `tempfile.gettempdir()` in Python.
#'
#' @return The temporary directory
#' @export
gettempdir <- function() {
    ret <- tempdir()  # get the temporary directory with suffix
    # file.remove(cache)
    dirname(ret)
}

#' Format the arguments for debugging
#' @param args The arguments to format
#' @return The formatted arguments
#' @keywords internal
format_args <- function(args) {
    paste(capture.output(str(args)), collapse = ", ")
}

#' Monkey patch a function from a namespace
#' @keywords internal
#' @param namespace The namespace where the function is located
#' @param function_name The name of the function to be patched
#' @param new_function The new function to be patched
#' @export
monkey_patch <- function(namespace, function_name, new_function) {
    ns <- getNamespace(namespace)
    unlockBinding(function_name, ns)
    assign(function_name, new_function, envir = ns)
    lockBinding(function_name, ns)
}
