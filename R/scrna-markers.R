# scrna-markers.R — universal marker-table helpers
#
# Universal marker format: a long table (TSV/CSV/RDS/qs/qs2 data.frame) with
# columns `cell_type` and `gene` (required), plus optional `direction`
# (positive/negative), `weight`, `species`, `cancer`, `tissue`, and `level`.
# Column aliases are auto-detected. `file://path#col1,col2,...` selects
# columns by name (aliases allowed) or 1-based index. Files without
# `cell_type`/`gene` columns are returned as-is (native-format passthrough).

.MARKER_ALIASES <- list(
    cell_type = c("cell_type", "celltype", "type"),
    gene = c("gene", "marker", "gene_symbol"),
    direction = c("direction", "sign"),
    weight = c("weight"),
    species = c("species"),
    cancer = c("cancer"),
    tissue = c("tissue", "tissueType"),
    level = c("level")
)

#' Canonicalize the marker table column names
#'
#' Rename the columns of a marker table to their canonical names, using the
#' aliases in `.MARKER_ALIASES` (matched case-insensitively).
#'
#' @param df A marker table. Non-data.frames are returned as-is.
#' @return The marker table with canonical column names.
#' @export
canonicalize_marker_cols <- function(df) {
    if (!is.data.frame(df)) { return(df) }
    cn <- colnames(df)
    low_cn <- tolower(cn)
    for (canonical in names(.MARKER_ALIASES)) {
        if (canonical %in% cn) { next }
        hit <- which(low_cn %in% tolower(.MARKER_ALIASES[[canonical]]))[1]
        if (!is.na(hit)) { cn[hit] <- canonical }
    }
    colnames(df) <- cn
    df
}

#' Check whether a marker table has the canonical columns
#'
#' @description A canonical marker table has both the `cell_type` and `gene`
#' columns.
#'
#' @param df A marker table.
#' @return `TRUE` if `df` is a data.frame with `cell_type` and `gene` columns,
#' `FALSE` otherwise.
#' @export
is_marker_canonical <- function(df) {
    is.data.frame(df) &&
        "cell_type" %in% colnames(df) &&
        "gene" %in% colnames(df)
}

#' Normalize the marker directions
#'
#' Map the direction aliases (case-insensitive `pos`/`+` and `neg`/`-`) to
#' `positive`/`negative`, and error on any other value.
#'
#' @param x A vector of direction values.
#' @return A character vector of `positive`/`negative` values.
#' @export
normalize_marker_direction <- function(x) {
    x <- tolower(trimws(as.character(x)))
    x[x %in% c("pos", "positive", "+")] <- "positive"
    x[x %in% c("neg", "negative", "-")] <- "negative"
    bad <- !x %in% c("positive", "negative")
    if (any(bad)) {
        stop(paste0(
            "Invalid `direction` value(s) in marker table: ",
            paste(unique(x[bad]), collapse = ", "),
            ". Accepted values: positive/negative (aliases: pos/neg/+/-)."
        ))
    }
    x
}

# Canonicalize columns and select a subset by name (exact, case-insensitive,
# alias) or 1-based index
#' Canonicalize the marker table columns and select a subset of them
#'
#' @param df A marker table.
#' @param cols The columns to select, by name (exact, case-insensitive or
#' alias) or by 1-based index. If NULL, all columns are kept.
#' @param path The path of the marker file, used in error messages.
#' @return The marker table with canonical column names and the selected
#' columns.
#' @export
apply_marker_cols <- function(df, cols, path) {
    if (!is.data.frame(df)) {
        stop(paste0("Cannot select columns from a non-table marker file: ", path))
    }
    df <- canonicalize_marker_cols(df)
    if (is.null(cols)) { return(df) }
    cn <- colnames(df)
    lookup <- setNames(cn, tolower(cn))
    for (canonical in names(.MARKER_ALIASES)) {
        if (canonical %in% cn) {
            for (alias in .MARKER_ALIASES[[canonical]]) {
                key <- tolower(alias)
                if (!key %in% names(lookup)) { lookup[[key]] <- canonical }
            }
        }
    }
    sel <- vapply(cols, function(col) {
        key <- tolower(col)
        if (key %in% names(lookup)) {
            lookup[[key]]
        } else if (grepl("^[0-9]+$", col)) {
            idx <- as.integer(col)
            if (idx < 1 || idx > length(cn)) {
                stop(paste0(
                    "Column index out of range in marker file: ", path, "#", col
                ))
            }
            cn[idx]
        } else {
            stop(paste0("Column not found in marker file: ", path, "#", col))
        }
    }, character(1))
    df[, sel, drop = FALSE]
}

# Read a marker file and return either a canonical marker data.frame
# (with `cell_type` and `gene` columns) or the raw object (a named list /
# matrix for RDS files, or the path string for xlsx files).
#' Load a marker table
#'
#' @description Read a marker file and return either a canonical marker table
#' or the raw object (a named list / matrix for RDS files, or the path string
#' for xlsx files).
#'
#' @param path The path of the marker file. A `file://` prefix is stripped, and
#' a `#col1,col2,...` suffix selects columns (by name, alias or 1-based index).
#' @param cols The columns to select. If NULL, they are taken from the `#`
#' suffix of `path` when there is one.
#' @return A canonical marker data.frame, or the raw object for native-format
#' files.
#' @export
load_marker_table <- function(path, cols = NULL) {
    log <- get_logger()
    if (is.null(path)) { stop("The marker file path is not set") }

    if (startsWith(path, "file://")) {
        path <- sub("^file://", "", path)
    }
    if (is.null(cols) && grepl("#", path, fixed = TRUE)) {
        parts <- strsplit(path, "#", fixed = TRUE)[[1]]
        path <- parts[1]
        cols <- trimws(strsplit(parts[2], ",", fixed = TRUE)[[1]])
        cols <- cols[cols != ""]
        if (length(cols) == 0) { cols <- NULL }
    }

    if (!file.exists(path)) {
        stop(paste0("Marker file does not exist: ", path))
    }

    ext <- tolower(tools::file_ext(path))
    if (ext %in% c("xlsx", "xls")) {
        return(path)  # native ScType xlsx passthrough
    }
    if (ext %in% c("rds", "qs", "qs2")) {
        obj <- read_obj(path)
        if (is.data.frame(obj)) {
            return(apply_marker_cols(obj, cols, path))
        }
        if (!is.null(cols)) {
            stop(paste0("Cannot select columns from a non-table marker file: ", path))
        }
        return(obj)
    }
    log$info("Loading marker table from {basename(path)} ...")
    df <- read.table(
        path,
        header = TRUE,
        sep = if (ext == "csv") "," else "\t",
        stringsAsFactors = FALSE
    )
    apply_marker_cols(df, cols, path)
}

# Filter a canonical marker table by the optional `tissue`/`cancer`/`species`
# envs. A filter that cannot be honored (the column is missing, or no rows
# match the value) is an error: silently running the tool on unfiltered or
# empty markers would produce wrong annotations.
#' Filter a marker table by tissue/cancer/species
#'
#' @description A filter that cannot be honored (the column is missing, or no
#' rows match the value) is an error: silently running the tool on unfiltered
#' or empty markers would produce wrong annotations.
#'
#' @param df A canonical marker table.
#' @param tissue The tissue to filter by, against the `tissue` column. If NULL,
#' no filtering is done.
#' @param cancer The cancer to filter by, against the `cancer` column. If NULL,
#' no filtering is done.
#' @param species The species to filter by, against the `species` column. If
#' NULL, no filtering is done.
#' @return The filtered marker table.
#' @export
apply_marker_filters <- function(df, tissue = NULL, cancer = NULL, species = NULL) {
    for (col in c("tissue", "cancer", "species")) {
        val <- switch(col,
            tissue = tissue,
            cancer = cancer,
            species = species
        )
        if (is.null(val)) next
        if (!col %in% colnames(df)) {
            stop(paste0(
                "The marker table has no `", col, "` column, ",
                "but it is asked to be filtered by `", col, "`."
            ))
        }
        if (!val %in% df[[col]]) {
            stop(paste0(
                "No markers in the marker table match `", col, " = ", val, "`."
            ))
        }
        df <- df[!is.na(df[[col]]) & df[[col]] == val, , drop = FALSE]
    }
    df
}

# Native (non-universal) marker formats have no `tissue`/`cancer`/`species`
# columns to filter by. Stop when the tool envs ask for filtering anyway;
# no-op when no filter envs are given.
#' Stop on filtering a native (non-universal) marker database
#'
#' @description Native marker formats have no `tissue`/`cancer`/`species`
#' columns to filter by, so asking for such a filter is an error. It is a no-op
#' when no filter is given.
#'
#' @param tissue The tissue filter. If not NULL, an error is raised.
#' @param cancer The cancer filter. If not NULL, an error is raised.
#' @param species The species filter. If not NULL, an error is raised.
#' @return `NULL`, invisibly.
#' @export
stop_on_filtering_native_db <- function(tissue = NULL, cancer = NULL, species = NULL) {
    given <- c(
        if (!is.null(tissue)) "tissue",
        if (!is.null(cancer)) "cancer",
        if (!is.null(species)) "species"
    )
    if (length(given) > 0) {
        stop(paste0(
            "`", paste(given, collapse = "`, `"),
            "` filter(s) only apply to a universal marker table ",
            "(a table with `cell_type` and `gene` columns)."
        ))
    }
    invisible(NULL)
}

# Convert a canonical marker table to the ScType format (one row per cell type)
#' Convert a marker table to the ScType format
#'
#' @description One row per cell type, with the positive and negative markers
#' collapsed into comma-separated `geneSymbolmore1`/`geneSymbolmore2` columns.
#'
#' @param df A canonical marker table.
#' @param tissue The tissue to filter by. If NULL, no filtering is done.
#' @param cancer The cancer to filter by. If NULL, no filtering is done.
#' @param species The species to filter by. If NULL, no filtering is done.
#' @return A data.frame in the ScType format.
#' @export
markers_to_sctype_df <- function(df, tissue = NULL, cancer = NULL, species = NULL) {
    df <- apply_marker_filters(df, tissue, cancer, species)
    direction <- if ("direction" %in% colnames(df)) {
        normalize_marker_direction(df$direction)
    } else {
        rep("positive", nrow(df))
    }
    do.call(rbind, lapply(unique(df$cell_type), function(ct) {
        rows <- df$cell_type == ct
        pos <- sort(unique(df$gene[rows & direction == "positive"]))
        neg <- sort(unique(df$gene[rows & direction == "negative"]))
        tissue <- if ("tissue" %in% colnames(df)) {
            tv <- unique(df$tissue[rows])
            tv[!is.na(tv)][1] %||% NA
        } else {
            NA
        }
        level <- if ("level" %in% colnames(df)) {
            lv <- unique(df$level[rows])
            lv[!is.na(lv)][1] %||% 1
        } else {
            1
        }
        data.frame(
            tissueType = tissue,
            cellName = ct,
            geneSymbolmore1 = paste0(pos, collapse = ","),
            geneSymbolmore2 = paste0(neg, collapse = ","),
            Level = level,
            level = level,
            stringsAsFactors = FALSE
        )
    }))
}

# Convert a canonical marker table to the scSorter format
# Negative-direction markers become negative weights
#' Convert a marker table to the scSorter format
#'
#' @description Negative-direction markers become negative weights.
#'
#' @param df A canonical marker table.
#' @param tissue The tissue to filter by. If NULL, no filtering is done.
#' @param cancer The cancer to filter by. If NULL, no filtering is done.
#' @param species The species to filter by. If NULL, no filtering is done.
#' @return A data.frame with `Type`, `Marker` and (when the marker table has a
#' `direction` or `weight` column) `Weight` columns.
#' @export
markers_to_scsorter_df <- function(df, tissue = NULL, cancer = NULL, species = NULL) {
    df <- apply_marker_filters(df, tissue, cancer, species)
    anno <- data.frame(
        Type = df$cell_type,
        Marker = df$gene,
        stringsAsFactors = FALSE
    )
    has_dir <- "direction" %in% colnames(df)
    has_w <- "weight" %in% colnames(df)
    if (has_dir || has_w) {
        w <- if (has_w) as.numeric(df$weight) else rep(1, nrow(df))
        if (has_dir) {
            neg <- normalize_marker_direction(df$direction) == "negative"
            w[neg] <- -abs(w[neg])
            w[!neg] <- abs(w[!neg])
        }
        anno$Weight <- w
    }
    anno
}

# Keep only positive markers of a canonical marker table
#' Keep only the positive markers of a marker table
#'
#' @param df A canonical marker table.
#' @return The marker table with the negative-direction markers removed.
#' @export
filter_positive_markers <- function(df) {
    if ("direction" %in% colnames(df)) {
        df <- df[
            normalize_marker_direction(df$direction) == "positive",
            , drop = FALSE
        ]
    }
    df
}

# Convert a canonical marker table to a named list (cell type → genes)
# Only positive markers are kept (negative ones cannot be represented)
#' Convert a marker table to a named list
#'
#' @description The names are the cell types and the values are the genes. Only
#' positive markers are kept (negative ones cannot be represented).
#'
#' @param df A canonical marker table.
#' @param tissue The tissue to filter by. If NULL, no filtering is done.
#' @param cancer The cancer to filter by. If NULL, no filtering is done.
#' @param species The species to filter by. If NULL, no filtering is done.
#' @return A named list of genes, named by the cell types.
#' @export
markers_to_named_list <- function(df, tissue = NULL, cancer = NULL, species = NULL) {
    df <- apply_marker_filters(df, tissue, cancer, species)
    df <- filter_positive_markers(df)
    split(as.character(df$gene), df$cell_type)
}

# Convert a canonical marker table to the scCATCH format
# Only positive markers are kept (negative ones cannot be represented)
#' Convert a marker table to the scCATCH format
#'
#' @description Only positive markers are kept (negative ones cannot be
#' represented). The `cell_type` column is renamed to `celltype` and the
#' `pmid`/`subtype1`/`subtype2`/`subtype3` columns are added when missing.
#'
#' @param df A canonical marker table.
#' @param tissue The tissue to filter by. If NULL, no filtering is done.
#' @param cancer The cancer to filter by. If NULL, no filtering is done.
#' @param species The species to filter by. If NULL, no filtering is done.
#' @return A data.frame in the scCATCH format.
#' @export
markers_to_sccatch_df <- function(df, tissue = NULL, cancer = NULL, species = NULL) {
    df <- apply_marker_filters(df, tissue, cancer, species)
    df <- filter_positive_markers(df)
    colnames(df)[colnames(df) == "cell_type"] <- "celltype"
    if (!"pmid" %in% colnames(df)) {
        df$pmid <- NA_character_
    }
    if (!"subtype3" %in% colnames(df)) {
        df$subtype3 <- NA_character_
    }
    if (!"subtype2" %in% colnames(df)) {
        df$subtype2 <- NA_character_
    }
    if (!"subtype1" %in% colnames(df)) {
        df$subtype1 <- NA_character_
    }
    df
}

# Detect a garnett-native marker file (blocks headed by `> <cell type>`) vs a
# universal marker table. The universal binary formats (rds/qs/qs2 data.frame,
# xlsx/xls) and a missing file are never native. Text files are decided by
# content: the first non-blank, non-`#` line of a native file is a `>` header.
#' Detect a garnett-native marker file
#'
#' @description A garnett-native marker file has blocks headed by
#' `> <cell type>`. The universal binary formats (rds/qs/qs2 data.frame,
#' xlsx/xls) and a missing file are never native. Text files are decided by
#' content: the first non-blank, non-`#` line of a native file is a `>` header.
#'
#' @param path The path of the marker file.
#' @return `TRUE` if the file is garnett-native, `FALSE` otherwise.
#' @export
is_garnett_native_marker <- function(path) {
    if (!file.exists(path)) { return(FALSE) }
    ext <- tolower(tools::file_ext(path))
    if (ext %in% c("rds", "qs", "qs2", "xlsx", "xls")) { return(FALSE) }
    con <- file(path, "r")
    on.exit(close(con))
    while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
        line <- trimws(line)
        if (line == "" || startsWith(line, "#")) { next }
        return(startsWith(line, ">"))
    }
    FALSE
}

# Convert a canonical marker table to a garnett-native marker file
# (`> <cell type>` blocks with `expressed:` / `not expressed:` rules).
# Negative-direction markers become `not expressed:` rules. Returns the path.
#' Convert a marker table to a garnett-native marker file
#'
#' @description Write `> <cell type>` blocks with `expressed:` /
#' `not expressed:` rules. Negative-direction markers become
#' `not expressed:` rules.
#'
#' @param df A canonical marker table.
#' @param file The path of the garnett marker file to write.
#' @param tissue The tissue to filter by. If NULL, no filtering is done.
#' @param cancer The cancer to filter by. If NULL, no filtering is done.
#' @param species The species to filter by. If NULL, no filtering is done.
#' @return The path of the written garnett marker file.
#' @export
markers_to_garnett_file <- function(df, file, tissue = NULL, cancer = NULL, species = NULL) {
    df <- apply_marker_filters(df, tissue, cancer, species)
    direction <- if ("direction" %in% colnames(df)) {
        normalize_marker_direction(df$direction)
    } else {
        rep("positive", nrow(df))
    }
    con <- file(file, "w")
    on.exit(close(con))
    for (ct in unique(df$cell_type)) {
        rows <- df$cell_type == ct
        pos <- sort(unique(df$gene[rows & direction == "positive"]))
        neg <- sort(unique(df$gene[rows & direction == "negative"]))
        if (length(pos) == 0) {
            stop(paste0(
                "Cell type `", ct, "` has no positive markers; garnett requires ",
                "at least one `expressed:` marker per cell type."
            ))
        }
        writeLines(paste0("> ", ct), con)
        writeLines(paste0("expressed: ", paste(pos, collapse = ", ")), con)
        if (length(neg) > 0) {
            writeLines(paste0("not expressed: ", paste(neg, collapse = ", ")), con)
        }
    }
    file
}

# Convert a canonical marker table to the UCell signature format
# Negative-direction markers get a `-` appended (UCell reads the direction from
# the gene name); UCell has no per-gene weights
#' Convert a marker table to the UCell signature format
#'
#' @description A named list of gene vectors, one per cell type, in the UCell
#' signature format.
#'
#' @details UCell reads the marker direction from the gene name: a gene whose
#' name ends in `-` is a negative marker (it contributes `w_neg` times its
#' score, subtracted), while a trailing `+` is stripped. Negative-direction
#' rows therefore get a `-` appended to the gene symbol and positive rows pass
#' through unchanged. UCell has no per-gene weights, so a `weight` column is
#' deliberately ignored.
#'
#' @param df A canonical marker table.
#' @return A named list of gene vectors, named by the cell types.
#' @export
markers_to_ucell_list <- function(df) {
    direction <- if ("direction" %in% colnames(df)) {
        normalize_marker_direction(df$direction)
    } else {
        rep("positive", nrow(df))
    }
    gene <- as.character(df$gene)
    cell_types <- unique(df$cell_type)
    sets <- lapply(cell_types, function(ct) {
        rows <- df$cell_type == ct
        pos <- sort(unique(gene[rows & direction == "positive"]))
        neg <- sort(unique(gene[rows & direction == "negative"]))
        # paste0(character(0), "-") is "-", not empty
        c(pos, if (length(neg) > 0) paste0(neg, "-"))
    })
    names(sets) <- cell_types
    sets
}

# Convert a canonical marker table to the singscore format
# singscore takes the up and the down sets natively, so the direction is kept
#' Convert a marker table to the singscore format
#'
#' @description A named list of `up`/`down` gene sets, one per cell type, for
#' [singscore::simpleScore()].
#'
#' @details The direction is native here, so both marker directions are kept:
#' positive-direction rows become `up` and negative-direction rows become
#' `down`.
#'
#' @param df A canonical marker table.
#' @return A named list of `list(up = <genes>, down = <genes>)`, named by the
#' cell types.
#' @export
markers_to_singscore_list <- function(df) {
    direction <- if ("direction" %in% colnames(df)) {
        normalize_marker_direction(df$direction)
    } else {
        rep("positive", nrow(df))
    }
    gene <- as.character(df$gene)
    cell_types <- unique(df$cell_type)
    sets <- lapply(cell_types, function(ct) {
        rows <- df$cell_type == ct
        list(
            up = sort(unique(gene[rows & direction == "positive"])),
            down = sort(unique(gene[rows & direction == "negative"]))
        )
    })
    names(sets) <- cell_types
    sets
}

# Majority-vote helper for cell-level tools with ident
#' Majority vote the labels of each cluster
#'
#' @description For every cluster, the label held by the most cells is picked.
#' Labels equal to `unknown` (and `NA`s) are ignored while counting; clusters
#' without any known label are mapped to `unknown`.
#'
#' @param labels A vector of cell-level labels.
#' @param clusters A vector of cell-level clusters, of the same length as
#' `labels`.
#' @param unknown The sentinel label to ignore while counting. Default is
#' `"unknown"`.
#' @return A named list mapping each cluster to its voted label; a cluster with
#' no known label is mapped to `unknown`.
#' @export
majority_vote <- function(labels, clusters, unknown = "unknown") {
    mapping <- list()
    for (cl in unique(as.character(clusters))) {
        cl_labels <- labels[as.character(clusters) == cl]
        cl_labels <- cl_labels[!is.na(cl_labels)]
        known <- cl_labels[cl_labels != unknown]
        mapping[[cl]] <- if (length(known) > 0) {
            names(which.max(table(known)))
        } else {
            unknown
        }
    }
    mapping
}
