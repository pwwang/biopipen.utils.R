#' Mutate scRepertorie object
#'
#' @param screp The scRepertorie object. It is either a Seurat object or a list of data.frames
#' @param mutaters A named list of mutater expressions, where the names are the new column names
#' and the values are the expressions to mutate the columns
#' The name with the suffix `:ident` will be used as the new identity column (only for Seurat object)
#' The values can be either character strings of expressions to be parsed
#' @param log Logger object to log the messages. If NULL, the default logger will be used.
#' @return The mutated scRepertorie object
#' @export
#' @importFrom rlang parse_expr
#' @importFrom dplyr mutate
#' @seealso [MutateSeuratMeta()]
#' @examples
#' \donttest{
#' data(contig_list, package = "scRepertoire")
#' screp <- scRepertoire::combineTCR(contig_list)
#' head(MutateScRep(screp, list(CTaa_len = "nchar(CTaa)"))[[1]])
#' }
MutateScRep <- function(screp, mutaters, log = NULL) {
    if (length(mutaters) == 0 || is.null(mutaters)) {
        return (screp)
    }

    if (inherits(screp, "Seurat")) {
        screp <- MutateSeuratMeta(screp, mutaters, log = log)
    } else {
        mutaters <- lapply(mutaters, as.character)
        mutaters <- lapply(mutaters, parse_expr)
        screp <- sapply(names(screp), function(x) {
            y <- screp[[x]]
            y$Sample <- x
            mutate(y, !!!mutaters)
        }, simplify = FALSE, USE.NAMES = TRUE)
    }

    screp
}

#' Alias of MutateScRep
#' @export
#' @rdname MutateScRep
ScRepMutate <- MutateScRep

#' Subset scRepertorie object
#'
#' @param screp The scRepertorie object. It is either a Seurat object or a list of data.frames
#' @param subset The subset expression (in characters)
#' @return The subsetted scRepertorie object
#' @export
#' @importFrom rlang parse_expr
#' @importFrom dplyr filter
#' @import tidyseurat
#' @examples
#' \donttest{
#' data(contig_list, package = "scRepertoire")
#' screp <- scRepertoire::combineTCR(contig_list,
#'    samples = c("P17B", "P17L", "P18B", "P18L", "P19B","P19L", "P20B", "P20L"))
#'
#' head(ScRepSubset(screp, "nchar(CTaa) < 20")[[1]])
#' names(ScRepSubset(screp, "Sample %in% c('P17B', 'P17L')"))
#' }
ScRepSubset <- function(screp, subset) {
    if (inherits(screp, "Seurat")) {
        filter(screp, !!parse_expr(subset))
    } else {
        screp <- sapply(names(screp), function(x) {
            y <- screp[[x]]
            y$Sample <- x
            dplyr::filter(y, !!parse_expr(subset))
        }, simplify = FALSE, USE.NAMES = TRUE)
        screp[sapply(screp, nrow) > 0]
    }
}
