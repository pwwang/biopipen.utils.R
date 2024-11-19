#' Add and save report
#'
#' @description
#' Add report to a report.json to pipen-report to build a report
#'
#' @importFrom R6 R6Class

Reporter <- R6Class(
    "Reporter",
    public = list(
        #' @field report The report
        report = NULL,

        #' @description
        #' Initialize the reporter
        initialize = function() {
            # h1 => list(
            #   h2 => list(
            #       h3#1 => list(ui1 => list(content11, content12)),
            #       h3#2 => list(ui2 => list(content21, content22))
            #   )
            # )
            self$report <- list()
        },

        #' @description
        #' Add a content to the report
        #' @param ... The content to add
        #' @param h1 The first level heading of the report
        #' @param h2 The second level heading of the report
        #' @param h3 The third level heading of the report
        #' @param ui The user interface of the report
        add = function(..., h1, h2 = "#", h3 = "#", ui = "flat") {
            if (is.null(self$report[[h1]])) {
                self$report[[h1]] <- list()
            }
            if (is.null(self$report[[h1]][[h2]])) {
                self$report[[h1]][[h2]] <- list()
            }
            if (is.null(self$report[[h1]][[h2]][[h3]])) {
                self$report[[h1]][[h2]][[h3]] <- list()
            }
            if (is.null(self$report[[h1]][[h2]][[h3]][[ui]])) {
                self$report[[h1]][[h2]][[h3]][[ui]] <- list()
            }
            content <- list(...)
            for (i in seq_along(content)) {
                self$report[[h1]][[h2]][[h3]][[ui]] <- c(
                    self$report[[h1]][[h2]][[h3]][[ui]],
                    list(content[[i]])
                )
            }
        },

        #' @description
        #' Add a content to the report, but infer the headings from a case info returned by [case_info]
        #' @param ... The content to add
        #' @param caseinfo The case info returned by [case_info]
        #' @param ui The user interface of the report
        #' @param max_level The maximum level of the headings. Default is 3
        add_case = function(..., caseinfo, ui = "flat", max_level = 3) {
            stopifnot("'max_level' must be an integer between 1 than 3" = max_level <= 3 && max_level > 0)

            all_levels <- c(caseinfo$section, caseinfo$name)
            if (max_level == 1) {
                h1 <- paste(all_levels, collapse = ": ")
                h2 <- h3 <- "#"
            } else if (max_level == 2) {
                h1 <- all_levels[1]
                h2 <- ifelse(length(all_levels) > 1, paste(all_levels[-1], collapse = ": "), "#")
                h3 <- "#"
            } else {
                h1 <- all_levels[1]
                h2 <- ifelse(length(all_levels) > 1, all_levels[2], "#")
                h3 <- ifelse(length(all_levels) > 2, paste(all_levels[-(1:2)], collapse = ": "), "#")
            }

            self$add(..., h1 = h1, h2 = h2, h3 = h3, ui = ui)
        },

        #' @description
        #' Generate a report for an image to be added
        #' @param name The name of the image
        #' @param prefix The prefix of the image
        #' @param more_formats More formats of the image available
        #' @param save_code Whether to save the code to reproduce the plot
        #' @return a list of the report for the image
        #' @examples
        #' reporter <- get_reporter()
        #' reporter$add(
        #'   reporter$image("image1", "Image 1", "pdf", save_code = TRUE),
        #'   h1 = "Images",
        #'   h2 = "Image 1"
        #' )
        image = function(name, prefix, more_formats, save_code) {
            out <- list(
                name = name,
                contents = list(
                    list(
                        kind = "image",
                        src = paste0(prefix, ".png")
                    )
                )
            )
            if (length(more_formats) > 0 || save_code) {
                out$contents[[1]]$download <- list()
            }
            if (length(more_formats) > 0) {
                out$contents[[1]]$download <- c(
                    out$contents[[1]]$download,
                    lapply(more_formats, function(format) {
                        paste0(prefix, ".", format)
                    })
                )
            }
            if (save_code) {
                out$contents[[1]]$download <- c(
                    out$contents[[1]]$download,
                    list(
                        list(
                            src = paste0(prefix, ".code.zip"),
                            tip = "Download the code to reproduce the plot",
                            icon = "Code"
                        )
                    )
                )
            }

            out
        },

        #' @description
        #' Clear the report
        clear = function() {
            self$report <- list()
        },

        #' @description
        #' Save the report to a file
        #' @param path The path to save the report
        #' If the path is a directory, the report will be saved as `report.json` in the directory
        #' Otherwise, the report will be saved to the path
        #' @param clear Whether to clear the report after saving
        #' @importFrom jsonlite toJSON
        save = function(path, clear = TRUE) {
            if (dir.exists(path)) {
                path <- file.path(path, "report.json")
            }

            writeLines(toJSON(self$report, pretty = TRUE, auto_unbox = TRUE), path)
            if (clear) {
                self$clear()
            }
        }
    )
)


#' Get a reporter object
#'
#' A shortcut to create a reporter object `Reporter$new()`
#'
#' @return A reporter object
#' @export
get_reporter <- function() {
    Reporter$new()
}
