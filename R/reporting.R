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
                self$report <- list()
            }
        }
    )
)
