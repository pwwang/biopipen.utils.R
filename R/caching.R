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
    sig_file <- file.path(cache_dir, paste0(dig, ".", kind , ".signature.txt"))
    writeLines(c(as.character(Sys.time()), "", sig), sig_file)
    cached_file <- file.path(cache_dir, paste0(dig, ".", kind, ".RDS"))
    saveRDS(data, cached_file)
}