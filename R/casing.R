
#' Expand the cases with default values
#'
#' @param cases A list of cases
#' @param defaults A list of default values
#' @param expand A function to expand each case, if NULL, then the `each` key will be ignored.
#'   The function should take two arguments, `name` and `case`, and return a list of expanded cases.
#' @param default_case The name of the default case
#' @return A list of expanded cases
#' @export
expand_cases <- function(cases, defaults, expand = NULL, default_case = "DEFAULT") {
    if (is.null(cases) || length(cases) == 0) {
        filled_cases <- list(DEFAULT = defaults)
    } else {
        filled_cases <- lapply(cases, function(case) {
            list_update(defaults, case)
        })
    }

    if (is.null(expand)) {
        return(filled_cases)
    }

    stopifnot("'expand' must be a function" = is.function(expand))

    outcases <- list()
    for (name in names(filled_cases)) {
        case <- filled_cases[[name]]
        each_cases <- expand(name, case)
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
#' @return A list of information, including `casedir`, `section`, `case`,
#'   `section_slug`, `case_slug` and the original `casename`.
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
