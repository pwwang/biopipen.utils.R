
#' Expand the cases with default values
#'
#' @param cases A list of cases
#' @param defaults A list of default values
#' @param post A function for post-handling each case, returning the a list with the
#' name and the case. One can also return multiple cases based on the demand.
#' All cases returned will be finally merged.
#' @param default_case The name of the default case. If a function is provided, it will
#' take the defaults as argument and return the name of the default case.
#' @return A list of expanded cases
#' @importFrom stats setNames
#' @export
expand_cases <- function(cases, defaults, post = NULL, default_case = "DEFAULT") {
    if (is.null(cases) || length(cases) == 0) {
        if (is.function(default_case)) {
            default_case <- default_case(defaults)
        }
        filled_cases <- setNames(list(defaults), default_case)
    } else {
        filled_cases <- lapply(cases, function(case) {
            list_update(defaults, case)
        })
    }

    if (is.null(post)) {
        return(filled_cases)
    }

    stopifnot("'post' must be a function" = is.function(post))

    outcases <- list()
    for (name in names(filled_cases)) {
        case <- filled_cases[[name]]
        each_cases <- post(name, case)
        outcases <- c(outcases, each_cases)
    }
    outcases
}


#' Create information for a casename
#'
#' @param name A casename
#' Sections are separated by `::` in the casename
#' When section is specified, the case will be created in the section directory
#' @param outdir The output directory, where the case directory will be created or prefix will be under
#' @param is_dir Whether to create a directory for the case
#' Otherwise, a prefix will be returned.
#' @param create Create the directory if not exists when `is_dir` is TRUE, otherwise, create the parent directory
#' of the prefix.
#' @return A list of information, including:
#' * `name`: The name of the case, without the sections
#' * `section`: The sections of the case as a vector
#' * `slug`: The slugified name
#' * `section_slug`: The slugified sections
#' * `prefix`: The prefix of the case, either the directory or the file path prefix
#' @export
case_info <- function(name, outdir, is_dir = TRUE, create = FALSE) {
    if (!grepl("::", name)) {
        section <- NULL
    } else {
        parts <- strsplit(name, "::")[[1]]
        name <- parts[length(parts)]
        section <- parts[-length(parts)]
    }

    out <- list(
        name = name,
        section = section,
        slug = slugify(name),
        section_slug = slugify(section)
    )

    if (is.null(section)) {
        out$prefix <- file.path(outdir, out$slug)
    } else {
        out$prefix <- file.path(outdir, out$section_slug, out$slug)
    }
    if (isTRUE(create)) {
        if (isTRUE(is_dir)) {
            dir.create(out$prefix, showWarnings = FALSE, recursive = TRUE)
        } else {
            dir.create(dirname(out$prefix), showWarnings = FALSE, recursive = TRUE)
        }
    }

    out
}
