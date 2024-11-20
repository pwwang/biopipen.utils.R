#' Setup and return the logger
#'
#' @param format The format of the log message.
#' This format will be passed to [logger::layout_glue_generator]. The default format will generate a log message like:
#' `INFO    [2021-01-01 00:00:00] Hello world`
#' @param appender The appender to use. Default is "stdout"
#' Either a string or a logger appender object. If string is provided, it will be converted to the corresponding logger appender object.
#' For example, "stdout" will be converted to [logger::appender_stdout]
#' @param level The log level. Default is "INFO"
#' Either a string or a logger level object. If string is provided, it will be converted to the corresponding logger level object.
#' For example, "INFO" will be converted to [logger::INFO]
#' @returns A list of logging functions
#' @export
#' @importFrom utils getFromNamespace
#' @importFrom logger layout_glue_generator log_threshold log_layout log_appender log_errors
#' @importFrom logger appender_stdout appender_stderr
#' @importFrom logger log_info log_debug log_warn log_error log_fatal log_success log_trace
get_logger <- function(
    format = '{sprintf("%-7s", level)} [{format(time, "%Y-%m-%d %H:%M:%S")}] {msg}',
    appender = "stdout",
    level = "INFO"
) {
    if (is.character(level)) {
        tryCatch({
            level <- getFromNamespace(toupper(level), "logger")
        }, error = function(e) {
            stop(sprintf("Invalid log level: %s", level))
        })
    }
    if (is.character(appender)) {
        tryCatch({
            appender <- getFromNamespace(paste0("appender_", appender), "logger")
        }, error = function(e) {
            stop(sprintf("Invalid log appender: %s", appender))
        })
    }
    .logger_layout <- layout_glue_generator(format = format)
    # print also debug messages, let pipen-poplog to filter
    log_threshold(level)
    log_layout(.logger_layout)
    log_appender(appender)
    tryCatch(log_errors(), error = function(e) {})

    list(
        info = log_info,
        debug = log_debug,
        warn = log_warn,
        warning = log_warn,
        error = log_error,
        fatal = log_fatal,
        success = log_success,
        trace = log_trace
    )
}
