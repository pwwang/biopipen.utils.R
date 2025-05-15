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
                    stopifnot("[Cache$get()] file path is needed for 'taget' to copy cached file." = !is.null(target) && is.character(target) && nzchar(target))
                    if (!file.exists(cached_path)) { return(NULL) }
                    file.copy(cached_path, target, overwrite = TRUE)
                    return(target)
                } else if (private$kind == "dir") {
                    stopifnot("[Cache$get()] directory path is needed for 'taget' to copy cached directory." = !is.null(target) && is.character(target) && nzchar(target))
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
        #' Save an object/file/directory to cache
        #' @param data The object to cache, or path to file/directory to cache
        save = function(data) {
            if (is.null(self$cache_dir) || !is.character(self$cache_dir) || !nzchar(self$cache_dir)) {
                return()
            }
            if (private$kind == "file") {
                stopifnot("[Cache$save()] 'data' must be a valid file path." = is.character(data) && nzchar(data) && file.exists(data))
                file.copy(
                    from = data,
                    to = file.path(self$cache_dir, private$prefix),
                    overwrite = TRUE
                )
            } else if (private$kind == "dir") {
                stopifnot("[Cache$save()] 'data' must be a valid directory path." = is.character(data) && nzchar(data) && dir.exists(data))
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
