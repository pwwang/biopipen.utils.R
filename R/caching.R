#' Get signatures and cached data
#'
#' @param x An object to infer signature from
#' @param kind A string indicating the kind of the object
#'   Used as part of the filename of the cached file
#' @param cache_dir A string indicating the directory to store cached files
#' @export
#' @return A list containing the signature, digested signature and cached data
#' @importFrom utils capture.output str
#' @importFrom digest digest
#' @details
#' This function is used to get the signature of an object and the cached data
#' if it exists. The signature is used to identify the object and the cached data
#' is used to avoid recomputing the object.
get_cached <- function(x, kind, cache_dir) {
    if (is.null(cache_dir) || isFALSE(cache_dir)) {
        return(list(sig = NULL, dig = NULL, data = NULL))
    }
    # Get signature of an object
    sig <- capture.output(str(x))
    dig <- digest(sig, algo = "md5")
    dig <- substr(dig, 1, 8)
    cached_file <- file.path(cache_dir, paste0(dig, ".", kind, ".RDS"))
    if (!file.exists(cached_file)) {
        return(list(sig = sig, dig = dig, data = NULL))
    }

    list(sig = sig, dig = dig, data = readRDS(cached_file))
}

#' Save an object to cache
#'
#' @param to_cache An list to cache,
#'   including the signature, digested signature and data
#' @param kind A string indicating the kind of the object
#'   Used as part of the filename of the cached file
#' @param cache_dir A string indicating the directory to store cached files
#' @export
save_to_cache <- function(to_cache, kind, cache_dir) {
    if (is.null(cache_dir) || isFALSE(cache_dir)) { return() }
    dig <- to_cache$dig
    sig <- to_cache$sig
    data <- to_cache$data
    # Save an object to cache
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    sig_file <- file.path(cache_dir, paste0(dig, ".", kind , ".signature.txt"))
    writeLines(c(as.character(Sys.time()), "", sig), sig_file)
    cached_file <- file.path(cache_dir, paste0(dig, ".", kind, ".RDS"))
    saveRDS(data, cached_file)
}

#' Caching class for object, file or directory caching
#'
#' @description
#' A class to handle caching of objects, files, or directories.
#' It generates a signature from the provided object and creates cached versions
#' based on that signature.
#'
#' @export
Caching <- R6::R6Class(
    "Caching",

    private = list(
        kind = NULL,
        prefix = NULL
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
        #' @param kind Type of cache: "object", "file", or "dir"
        #' @return A new Cache object
        initialize = function(sig_object, prefix, cache_dir, save_sig = FALSE, kind = c("object", "file", "dir")) {
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
                    file.remove(cached_path)
                } else if (private$kind == "dir" && dir.exists(cached_path)) {
                    unlink(cached_path, recursive = TRUE)
                } else {
                    cached_file <- paste0(cached_path, ".qs")
                    if (file.exists(cached_file)) {
                        file.remove(cached_file)
                    }
                }

                # also remove the signature file if it exists
                sig_file <- file.path(self$cache_dir, paste0(private$prefix, ".signature.txt"))
                if (file.exists(sig_file)) {
                    file.remove(sig_file)
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
        #' @param target Target path for file/directory cache (required for file/dir kind)
        #' @return For objects: the cached object or NULL if not found
        #' For files/dirs: the target path or NULL if not found
        get = function(target = NULL) {
            cache_dir <- self$cache_dir
            if (!is.null(cache_dir) && is.character(cache_dir) && nzchar(cache_dir) > 0) {
                cached_path <- file.path(cache_dir, private$prefix)
                if (private$kind == "file") {
                    stopifnot("[Caching$get()] file path is needed for 'taget' to copy cached file." = !is.null(target) && is.character(target) && nzchar(target))
                    if (!file.exists(cached_path)) { return(NULL) }
                    file.copy(cached_path, target, overwrite = TRUE)
                    return(target)
                } else if (private$kind == "dir") {
                    stopifnot("[Caching$get()] directory path is needed for 'taget' to copy cached directory." = !is.null(target) && is.character(target) && nzchar(target))
                    if (!dir.exists(cached_path)) { return(NULL) }
                    dir.create(target, showWarnings = FALSE, recursive = TRUE)
                    file.copy(
                        list.files(cached_path, full.names = TRUE),
                        target,
                        recursive = TRUE,
                        overwrite = TRUE
                    )
                    return(target)
                } else {
                    cached_path <- paste0(cached_path, ".qs")
                    if (!file.exists(cached_path)) { return(NULL) }
                    return(read_obj(cached_path))
                }
            }

            NULL
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
        #' Save an object/file/directory to cache
        #' @param data The object to cache, or path to file/directory to cache
        save = function(data) {
            if (is.null(self$cache_dir) || !is.character(self$cache_dir) || !nzchar(self$cache_dir)) {
                return()
            }
            if (private$kind == "file") {
                stopifnot("[Caching$save()] 'data' must be a valid file path." = is.character(data) && nzchar(data) && file.exists(data))
                file.copy(
                    from = data,
                    to = file.path(self$cache_dir, private$prefix),
                    overwrite = TRUE
                )
            } else if (private$kind == "dir") {
                stopifnot("[Caching$save()] 'data' must be a valid directory path." = is.character(data) && nzchar(data) && dir.exists(data))
                dir.create(file.path(self$cache_dir, private$prefix), showWarnings = FALSE, recursive = TRUE)
                file.copy(
                    list.files(data, full.names = TRUE),
                    file.path(self$cache_dir, private$prefix),
                    recursive = TRUE,
                    overwrite = TRUE
                )
            } else {
                save_obj(data, file.path(self$cache_dir, paste0(private$prefix, ".qs")))
            }
            invisible()
        }
    )
)
