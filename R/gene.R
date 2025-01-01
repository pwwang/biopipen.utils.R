#' Convert gene names between different formats
#'
#' @param genes A character/integer vector of gene names/ids
#' @param species A character vector of species names
#' @param infmt A character vector of input gene name formats
#'   See the available scopes at
#'   https://docs.mygene.info/en/latest/doc/data.html#available-fields
#'   You can use ensg as a shortcut for ensembl.gene
#' @param outfmt A character vector of output gene name formats
#' @param dup How to deal with duplicate gene names found.
#'   * "first": keep the first one (default), sorted by score descendingly
#'   * "last": keep the last one, sorted by score descendingly
#'   * "all": keep all of them, each will be a separate row
#'   * "`<X>`": combine them into a single string, separated by `X`
#' @param notfound How to deal with gene names that are not found
#'   * "error": stop with an error message
#'   * "use-query": use the query gene name as the converted gene name
#'   * "skip": skip the gene names that are not found
#'   * "ignore": Same as "skip"
#'   * "na"/"NA": use NA as the converted gene name (default)
#' @param suppress_messages Whether to suppress the warning messages
#' @return A tibble with the query gene names and the converted gene names
#'   When a gene name is not found, the converted name will be NA
#'   When duplicate gene names are found, the one with the highest score will be kept
#' @export
#' @importFrom utils capture.output
#' @importFrom rlang sym :=
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr ungroup slice_max arrange group_by summarise mutate filter coalesce
#' @importFrom dplyr left_join select slice_min pull everything any_of all_of desc %>%
gene_name_conversion <- function(
    genes,
    infmt,
    outfmt,
    dup = "first",
    species = "human",
    notfound = "na",
    suppress_messages = TRUE) {
    notfound <- as.character(notfound)
    notfound <- match.arg(notfound, c("error", "use-query", "skip", "ignore", "na", "NA"))

    if (infmt %in% c("ensg", "ensmusg")) {
        infmt <- "ensembl.gene"
    }
    if (outfmt %in% c("ensg", "ensmusg")) {
        outfmt <- "ensembl.gene"
    }

    orig_genes <- genes
    if (infmt == "ensembl.gene") {
        # Remove version numbers from ensembl gene ids
        genes <- gsub("\\..*", "", genes)
    }
    query_df <- tibble(query = genes, orig = orig_genes)
    out <- tryCatch(
        {
            if (suppress_messages) {
                capture.output(suppressWarnings(suppressMessages({
                    out <- mygene::queryMany(genes, scopes = infmt, fields = outfmt, species = species) %>%
                        as_tibble()
                })))
            } else {
                out <- mygene::queryMany(genes, scopes = infmt, fields = outfmt, species = species) %>%
                    as_tibble()
            }

            if (nrow(out) == 0) {
                out <- tibble(query = orig_genes, X_id = NA_character_, X_score = 0, converted = NA_character_)  # nocov
                names(out)[4] <- outfmt  # nocov
            }
            out
        },
        error = function(e) {
            out <- tibble(query = orig_genes, X_id = NA_character_, X_score = 0, converted = NA_character_)
            names(out)[4] <- outfmt
            out
        }
    )

    if (dup == "first") {
        out <- out %>%
            group_by(!!sym("query")) %>%
            slice_max(!!sym("X_score"), n = 1, with_ties = FALSE) %>%
            ungroup() %>%
            select(all_of(c("query", outfmt)))
    } else if (dup == "last") {
        out <- out %>%
            group_by(!!sym("query")) %>%
            slice_min(!!sym("X_score"), n = 1, with_ties = FALSE) %>%
            ungroup() %>%
            select(all_of(c("query", outfmt)))
    } else if (dup != "all") {
        out <- out %>%
            group_by(!!sym("query")) %>%
            arrange(desc(!!sym("X_score"))) %>%
            summarise(!!sym(outfmt) := paste(unique(!!sym(outfmt)), collapse = dup))
    }

    out <- query_df %>%
        left_join(out, by = "query", multiple = "first") %>%
        select(-any_of(c("query", "X_id", "X_score"))) %>%
        select(query = !!sym("orig"), everything())

    if (notfound == "error") {
        if (any(is.na(out[[outfmt]]))) {
            nagenes <- out %>%
                filter(is.na(!!sym(outfmt))) %>%
                pull("query")
            stop(paste("Query genes not found:", paste(nagenes, collapse = ",")))
        }
    } else if (notfound == "use-query") {
        out <- out %>% mutate(!!sym(outfmt) := coalesce(!!sym(outfmt), query = !!sym("query")))
    } else if (notfound == "skip" || notfound == "ignore") {
        out <- out %>% filter(!is.na(!!sym(outfmt)))
    }

    return(out)
}
