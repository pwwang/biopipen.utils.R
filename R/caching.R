#' Copy a directory to another location
#'
#' @description Unlike `file.copy`, this function copies a directory and its contents
#' to another location, instead of copying into another location.
#' It creates the target directory if it does not exist.
#' @param from Source directory path
#' @param to Target directory path
#' @param overwrite Whether to overwrite existing files in the target directory
#' @param copy.date Whether to copy the file modification date
#' @return a logical vector indicating which operation succeeded for each of the files in `from` attempted
#' @keywords internal
.dir.copy <- function(from, to, overwrite = FALSE, copy.date = TRUE) {
    if (!dir.exists(from)) {
        stop(paste("Source directory does not exist:", from))
    }
    dir.create(to, showWarnings = FALSE, recursive = TRUE)
    files <- list.files(from, full.names = TRUE, include.dirs = TRUE)
    file.copy(
        files, to,
        recursive = TRUE, overwrite = overwrite, copy.date = copy.date
    )
}

#' Cache class for object, file or directory caching
#'
#' @description
#' A class to handle caching of objects, files, or directories.
#' It generates a signature from the provided object and creates cached versions
#' based on that signature.
#'
#' @export
Cache <- R6::R6Class(
    "Cache",

    private = list(
        kind = NULL,
        prefix = NULL,
        source_path = NULL
    ),

    public = list(

        #' @field cache_dir Directory where cached files are stored
        cache_dir = NULL,

        #' @description
        #' Initialize a new Cache object
        #' @param sig_object Object used to generate the cache signature
        #' @param prefix Prefix for the cache filename
        #' @param cache_dir Directory where cached files are stored
        #' @param save_sig Whether to save the signature to a file
        #' @param kind Type of cache: "object", "file", "dir" or "prefix"
        #' * object: cache an R object
        #' * file: cache a file
        #' * dir: cache a directory
        #' * prefix: cache files/directories with the same prefix
        #' @return A new Cache object
        initialize = function(sig_object, prefix, cache_dir, save_sig = FALSE, kind = c("object", "file", "dir", "prefix")) {
            private$kind <- match.arg(kind)
            self$cache_dir <- cache_dir
            if (!is.null(cache_dir) && is.character(cache_dir) && nzchar(cache_dir)) {
                dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
                full_sig <- capture.output(str(sig_object))
                sig <- substr(digest(capture.output(str(sig_object)), algo = "md5"), 1, 8)
                if (save_sig) {
                    sig_file <- file.path(cache_dir, paste0(prefix, ".", sig, ".signature.txt"))
                    writeLines(c(as.character(Sys.time()), "", full_sig), sig_file)
                }
                private$prefix <- paste0(prefix, ".", sig)
            }
        },

        #' @description
        #' Clear the current cached object/file/directory
        clear = function() {
            if (!is.null(self$cache_dir) && is.character(self$cache_dir) && nzchar(self$cache_dir)) {
                cached_path <- file.path(self$cache_dir, private$prefix)
                if (private$kind == "file" && file.exists(cached_path)) {
                    unlink(cached_path, force = TRUE)
                } else if (private$kind == "dir" && dir.exists(cached_path)) {
                    unlink(cached_path, recursive = TRUE, force = TRUE)
                } else if (private$kind == "prefix") {
                    # remove all files with the same prefix
                    files <- Sys.glob(file.path(self$cache_dir, paste0(private$prefix, ".*")))
                    unlink(files, recursive = TRUE, force = TRUE)
                } else {
                    cached_file <- paste0(cached_path, ".qs")
                    unlink(cached_file, force = TRUE)
                }

                # also remove the signature file if it exists
                sig_file <- file.path(self$cache_dir, paste0(private$prefix, ".signature.txt"))
                if (file.exists(sig_file)) {
                    unlink(sig_file, force = TRUE)
                }
            }
        },

        #' @description
        #' Clear all cached objects in the cache directory
        #' Be careful with this operation as it will remove all cached files in the cache directory
        clear_all = function() {
            if (!is.null(self$cache_dir) && is.character(self$cache_dir) && nzchar(self$cache_dir)) {
                unlink(self$cache_dir, recursive = TRUE)
            }
        },

        #' @description
        #' Retrieve the cached object/file/directory
        #' When NULL is returned, it means the cache does not exist
        #' @return The cached object if kind is "object", otherwise NULL
        restore = function() {
            cache_dir <- self$cache_dir
            if (!is.null(cache_dir) && is.character(cache_dir) && nzchar(cache_dir) > 0) {
                cached_path <- file.path(cache_dir, private$prefix)
                if (private$kind == "file") {
                    if (!file.exists(cached_path)) {
                        stop(paste0("[Cache$restore()] Cached file does not exist: ", cached_path))
                    }
                    file.copy(cached_path, private$source_path, overwrite = TRUE, copy.date = TRUE)
                } else if (private$kind == "dir") {
                    if (!dir.exists(cached_path)) {
                        stop(paste0("[Cache$restore()] Cached directory does not exist: ", cached_path))
                    }
                    .dir.copy(
                        from = cached_path,
                        to = private$source_path,
                        overwrite = TRUE,
                        copy.date = TRUE
                    )
                } else if (private$kind == "prefix") {
                    files <- Sys.glob(paste0(cached_path, ".*"))
                    if (length(files) == 0) {
                        stop(paste0("[Cache$restore()] No cached files found with prefix: ", private$prefix))
                    }
                    prefix <- private$source_path[1]
                    src_files <- private$source_path[-1]
                    dir.create(dirname(prefix), showWarnings = FALSE, recursive = TRUE)
                    target_files <- c()
                    for (file in files) {
                        bname <- substring(basename(file), nchar(private$prefix) + 2)
                        target_file <- file.path(dirname(prefix), bname)
                        target_files <- c(target_files, target_file)
                        if (dir.exists(file)) {
                            .dir.copy(
                                from = file,
                                to = target_file,
                                overwrite = TRUE,
                                copy.date = TRUE
                            )
                        } else {
                            file.copy(
                                from = file,
                                to = target_file,
                                overwrite = TRUE,
                                copy.date = TRUE
                            )
                        }
                    }
                    unrestored <- setdiff(src_files, target_files)
                    if (length(unrestored) > 0) {
                        stop(paste0("[Cache$restore()] Some files were not restored: \n- ", paste(unrestored, collapse = "\n- ")))
                    }
                } else {
                    cached_path <- paste0(cached_path, ".qs")
                    if (!file.exists(cached_path)) {
                        stop(paste0("[Cache$restore()] Cached object does not exist: ", cached_path))
                    }
                    return(read_obj(cached_path))
                }
            } else {
                stop("[Cache$restore()] 'cache$cache_dir' must be a valid directory path.")
            }
        },

        #' @description
        #' Check if we have a cached object/file/directory
        #' @return TRUE if the cached object/file/directory exists, FALSE otherwise
        is_cached = function() {
            cache_dir <- self$cache_dir
            if (!is.null(cache_dir) && is.character(cache_dir) && nzchar(cache_dir) > 0) {
                cached_path <- file.path(cache_dir, private$prefix)
                if (private$kind == "file") {
                    return(file.exists(cached_path))
                } else if (private$kind == "dir") {
                    return(dir.exists(cached_path))
                } else if (private$kind == "prefix") {
                    # check if there are any files with the same prefix
                    files <- Sys.glob(file.path(cache_dir, paste0(private$prefix, ".*")))
                    return(length(files) > 0)
                } else {
                    cached_path <- paste0(cached_path, ".qs")
                    return(file.exists(cached_path))
                }
            }
            FALSE
        },

        #' @description
        #' Get the path to the cached object/file/directory
        #' @return The path to the cached object/file/directory
        get_path = function() {
            if (!is.null(self$cache_dir) && is.character(self$cache_dir) && nzchar(self$cache_dir)) {
                return(file.path(self$cache_dir, private$prefix))
            }
            NULL
        },

        #' @description
        #' Save an object/file/directory/prefix to cache
        #' @param data The object to cache, or path to file/directory to cache
        save = function(data) {
            if (is.null(self$cache_dir) || !is.character(self$cache_dir) || !nzchar(self$cache_dir)) {
                stop("[Cache$save()] 'cache$cache_dir' must be a valid directory path.")
            }

            if (!is.null(private$source_path)) {
                stop("[Cache$save()] Only one save is allowed per Cache object.")
            }

            cached_path <- file.path(self$cache_dir, private$prefix)
            if (private$kind == "file") {
                private$source_path <- data
                stopifnot("[Cache$save()] 'data' must be a valid file path." = is.character(data) && nzchar(data) && file.exists(data))
                file.copy(
                    from = data,
                    to = cached_path,
                    overwrite = TRUE,
                    copy.date = TRUE
                )
            } else if (private$kind == "dir") {
                private$source_path <- data
                stopifnot("[Cache$save()] 'data' must be a valid directory path." = is.character(data) && nzchar(data) && dir.exists(data))
                .dir.copy(
                    from = data,
                    to = cached_path,
                    overwrite = TRUE,
                    copy.date = TRUE
                )
            } else if (private$kind == "prefix") {
                stopifnot("[Cache$save()] 'data' must be a valid file path." = is.character(data) && nzchar(data))
                files <- Sys.glob(paste0(data, "*"))
                private$source_path <- c(data, files)
                for (file in files) {
                    if (dir.exists(file)) {
                        .dir.copy(
                            from = file,
                            to = paste0(cached_path, ".", basename(file)),
                            overwrite = TRUE,
                            copy.date = TRUE
                        )
                    } else {
                        file.copy(
                            from = file,
                            to = paste0(cached_path, ".", basename(file)),
                            overwrite = TRUE,
                            copy.date = TRUE
                        )
                    }
                }
            } else {
                private$source_path <- "<object>"
                save_obj(data, file.path(self$cache_dir, paste0(private$prefix, ".qs")))
            }
            invisible()
        }
    )
)
