#' Call a function with a list of arguments
#'
#' Different from `do.call`, this is a faster version, especially when there are
#' big objects in the list.
#'
#' @param fn A function to call
#' @param args A list of arguments to pass to the function
#' @param quote Whether to quote the arguments
#' @param envir The environment to evaluate the function in
#' @return The result of the function call
#' @export
do_call <- function(fn, args, quote = FALSE, envir = parent.frame()) {
    # source: Gmisc
    # author: Max Gordon <max@gforge.se>

    if (quote) {
        args <- lapply(args, enquote)  # nocov
    }

    if (is.null(names(args)) ||
        is.data.frame(args)) {
        argn <- args
        args <- list()
    } else {
        # Add all the named arguments
        argn <- lapply(names(args)[names(args) != ""], as.name)
        names(argn) <- names(args)[names(args) != ""]
        # Add the unnamed arguments
        argn <- c(argn, args[names(args) == ""])
        args <- args[names(args) != ""]
    }

    if (inherits(fn, "character")) {
        if (is.character(fn)) {
            fn <- strsplit(fn, "[:]{2,3}")[[1]]
            fn <- if (length(fn) == 1) {
                get(fn[[1]], envir = envir, mode = "function")
            } else {
                get(fn[[2]], envir = asNamespace(fn[[1]]), mode = "function")
            }
        }
        call <- as.call(c(list(fn), argn))
    } else if (inherits(fn, "function")) {
        f_name <- deparse(substitute(fn))
        call <- as.call(c(list(as.name(f_name)), argn))
        args[[f_name]] <- fn
    } else if (inherits(fn, "name")) {  # nocov
        call <- as.call(c(list(fn, argn)))  # nocov
    }

    eval(call,
        envir = args,
        enclos = envir
    )
}

#' Run a command
#'
#' @param cmd A command to run
#' @param fg Whether to run the command in the foreground
#' @param wait Whether to wait for the command to finish
#' @param print_command The handler to print the command
#' @param stdout See [base::system2]
#' @param stderr See [base::system2]
#' @param ... Additional arguments to pass to `system2`
#' @return The output of the command
#' If `stdout` or `stderr` is `TRUE`, the output is returned as a character vector
#' Otherwise, the output is returned as an integer exit code
#' @export
run_command <- function(
    cmd,
    fg = FALSE,
    wait = TRUE,
    print_command = cat,
    stdout = "",
    stderr = "",
    ...
) {
    if (!is.null(print_command)) {
        print_command("RUNNING COMMAND:\n")
        print_command(paste0("  ", paste(cmd, collapse = " "), "\n\n"))
    }

    kwargs <- list(...)
    stdin <- kwargs$stdin %||% ""
    input <- kwargs$input %||% NULL
    k_env <- kwargs$env %||% list()
    env <- ""
    if (is.list(k_env)) {
        for (k in names(k_env)) { env <- paste0(env, k, "=", k_env[[k]], ";")}
    } else {
        env <- k_env
    }
    if (fg) {
        stdout <- ""
        stderr <- ""
    } else {
        if (stdout == "" && isTRUE(stderr)) { stdout <- TRUE }
        if (stdout == "") { stdout <- FALSE }
    }

    command = cmd[1]
    args = cmd[-1]
    out <- system2(
        command,
        args = args,
        stdout = stdout,
        stderr = stderr,
        stdin = stdin,
        env = env,
        wait = wait,
        input = input
    )

    if (!isTRUE(stdout) && !isTRUE(stderr)) {
        if(out != 0) stop(sprintf("Command failed with exit code %s", out))
        if (!fg) { return(out) }
    } else {
        status <- attr(out, "status")
        if (is.integer(status) && status != 0) {
            stop(sprintf("Command failed: code (%s): %s", status, out))
        }
        return(out)
    }
}
