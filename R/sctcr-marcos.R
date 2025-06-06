#' Mutate scRepertorie object
#'
#' @param screp The scRepertorie object. It is either a Seurat object or a list of data.frames
#' @param mutaters A list of expressions (in characters) to mutate the data
#' @return The mutated scRepertorie object
#' @export
#' @importFrom rlang parse_expr
#' @importFrom dplyr mutate
#' @examples
#' \donttest{
#' data(contig_list, package = "scRepertoire")
#' screp <- scRepertoire::combineTCR(contig_list)
#' head(ScRepMutate(screp, list(CTaa_len = "nchar(CTaa)"))[[1]])
#' }
ScRepMutate <- function(screp, mutaters) {
    if (length(mutaters) == 0 || is.null(mutaters)) {
        return (screp)
    }

    mutaters <- lapply(mutaters, parse_expr)
    if (inherits(screp, "Seurat")) {
        screp@meta.data <- mutate(screp@meta.data, !!!mutaters)
    } else {
        screp <- sapply(names(screp), function(x) {
            y <- screp[[x]]
            y$Sample <- x
            mutate(y, !!!mutaters)
        }, simplify = FALSE, USE.NAMES = TRUE)
    }

    screp
}

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
