# Tool runners for RunCellTypeAnnotation(). Each runner takes
# (object, args, ident, ctx) and returns list(mapping, type, cells, more):
#   type = "cluster": `mapping` is a named list cluster -> cell type
#   type = "cell":    `mapping` is a data.frame of per-cell labels
#                     (rownames = cells, first column = the annotation)
#   cells: per-cell data.frame returned alongside type = "cluster" when the tool
#          also produced per-cell labels ("both-mode")
#   more:  extra cluster mappings (direct's more_cell_types)
# ctx: list(cache = <dir>, h5ad = <h5ad path or NULL>, scratch = <per-run dir>)

.run_celltypeannotation_direct <- function(object, args, ident, ctx) {
    log <- get_logger()
    cell_types <- args$cell_types
    more_cell_types <- args$more_cell_types

    if (is.null(cell_types) || length(cell_types) == 0) {
        log$warn("No cell types are given!")
        if (!is.null(more_cell_types) && length(more_cell_types) > 0) {
            log$warn(
                "`cell_types` is not given, won't process `more_cell_types`!"
            )
        }
        return(list(mapping = list(), type = "cluster"))
    }

    idents <- object@meta.data[[ident]]
    if (is.factor(idents)) {
        idents <- levels(idents)
    } else {
        idents <- as.character(unique(idents))
    }

    process_celltypes <- function(ct, key = NULL) {
        if (is.list(ct)) {
            nonexisting <- setdiff(names(ct), idents)
            if (length(nonexisting) > 0) {
                if (is.null(key)) {
                    log$warn(paste0(
                        "The following clusters do not exist: ",
                        paste(nonexisting, collapse = ", ")
                    ))
                } else {
                    log$warn(paste0(
                        "The following clusters for '", key,
                        "' do not exist: ",
                        paste(nonexisting, collapse = ", ")
                    ))
                }
                ct <- ct[setdiff(names(ct), nonexisting)]
            }
            # Fill in missing clusters with their own names
            missing <- setdiff(idents, names(ct))
            for (m in missing) {
                ct[[m]] <- m
            }
            # Handle special values: "NA" -> NA, "-" or "" -> cluster name
            for (n in names(ct)) {
                if (is.na(ct[[n]])) next
                if (ct[[n]] == "-" || ct[[n]] == "") {
                    ct[[n]] <- n
                } else if (ct[[n]] == "NA") {
                    ct[[n]] <- NA
                }
            }
            return(ct)
        }
        if (length(ct) < length(idents)) {
            ct <- c(ct, idents[(length(ct) + 1):length(idents)])
        } else if (length(ct) > length(idents)) {
            ct <- ct[1:length(idents)]
            if (is.null(key)) {
                log$warn(
                    "The length of cell types is longer than the number of clusters!"
                )
            } else {
                log$warn(paste0(
                    "The length of cell types for '", key,
                    "' is longer than the number of clusters!"
                ))
            }
        }
        for (i in seq_along(ct)) {
            if (is.na(ct[i])) next
            if (ct[i] == "-" || ct[i] == "") {
                ct[i] <- idents[i]
            } else if (ct[i] == "NA") {
                ct[i] <- NA
            }
        }
        ct <- as.list(ct)
        names(ct) <- idents
        return(ct)
    }

    more <- NULL
    if (!is.null(more_cell_types) && length(more_cell_types) > 0) {
        more <- list()
        for (key in names(more_cell_types)) {
            ct <- more_cell_types[[key]]
            ct <- process_celltypes(ct, key)
            more[[key]] <- ct
        }
    }

    celltypes <- process_celltypes(cell_types)

    list(mapping = celltypes, type = "cluster", more = more)
}

.run_celltypeannotation_cell <- function(object, args, ident, ctx) {
    log <- get_logger()
    cell_types_file <- args$cell_types

    if (!is.character(cell_types_file)) {
        stop(paste(
            "`cell_types` must be path to a TSV file",
            "with cell-level annotations."
        ))
    }

    if (startsWith(cell_types_file, "file://")) {
        cell_types_file <- sub("^file://", "", cell_types_file)
    }

    log$info(paste0("Loading cell type file: ", cell_types_file))

    file_path_parts <- strsplit(cell_types_file, "#")[[1]]
    ct_file <- file_path_parts[1]
    if (length(file_path_parts) > 1) {
        ct_cols <- trimws(strsplit(file_path_parts[2], ",")[[1]])
    } else {
        ct_cols <- c("1", "2")
    }

    if (!file.exists(ct_file)) {
        stop(paste0("Cell type file does not exist: ", ct_file))
    }

    celltypes <- read.table(
        ct_file, header = TRUE, sep = "\t", stringsAsFactors = FALSE
    )
    ct_cols <- sapply(
        ct_cols,
        function(col) {
            if (grepl("^[+-]?[0-9]+$", col)) {
                col <- as.integer(col)
                if (col < 0) {
                    col <- ncol(celltypes) + col + 1
                }
                if (col < 1 || col > ncol(celltypes)) {
                    stop(paste0(
                        "Column index '", col,
                        "' is out of bounds for cell type file: ", ct_file
                    ))
                }
                col <- colnames(celltypes)[col]
            }
            if (!col %in% colnames(celltypes)) {
                stop(paste0(
                    "Column '", col, "' not found in cell type file: ", ct_file
                ))
            }
            return(col)
        }
    )
    celltypes <- celltypes[, ct_cols, drop = FALSE]
    cell_id_col <- ct_cols[1]
    cell_type_cols <- ct_cols[-1]

    existing_cells <- intersect(
        rownames(object@meta.data), celltypes[[cell_id_col]]
    )
    if (length(existing_cells) == 0) {
        stop(paste0(
            "No cells in the Seurat object match the cell IDs ",
            "in the cell type file. \n",
            "Cell IDs in the Seurat object look like: ",
            paste(head(rownames(object@meta.data)), collapse = ", "),
            " ... \n",
            "Cell IDs in the cell type file look like: ",
            paste(head(celltypes[[cell_id_col]]), collapse = ", "),
            " ... \n"
        ))
    }

    existing_cols <- intersect(cell_type_cols, colnames(object@meta.data))
    if (length(existing_cols) > 0) {
        log$warn(paste0(
            "Column(s) '", paste(existing_cols, collapse = ", "),
            "' already exist(s) in Seurat object metadata, will be overwritten."
        ))
        object@meta.data[, existing_cols] <- NULL
    }

    missing_cells <- setdiff(
        rownames(object@meta.data), celltypes[[cell_id_col]]
    )
    if (length(missing_cells) > 0) {
        log$warn(paste0(
            "The following cells are missing in the cell type file: ",
            paste(head(missing_cells), collapse = ", "),
            if (length(missing_cells) > 6)
                paste0(", ... (", length(missing_cells) - 6, " more)"),
            "."
        ))
    }
    # Add NAs for missing cells
    missing_celltypes <- data.frame(
        matrix(NA, nrow = length(missing_cells), ncol = ncol(celltypes))
    )
    colnames(missing_celltypes) <- colnames(celltypes)
    missing_celltypes[[cell_id_col]] <- missing_cells
    celltypes <- rbind(celltypes, missing_celltypes)

    # Reorder to match Seurat object cell order
    celltypes <- celltypes[
        match(rownames(object@meta.data), celltypes[[cell_id_col]]),
        cell_type_cols,
        drop = FALSE
    ]

    if (is.null(ident)) {
        list(mapping = celltypes, type = "cell")
    } else {
        # Aggregate per-cell results to cluster-level mapping (majority vote)
        log$info("Aggregating cell type annotations by cluster...")
        mapping <- majority_vote(
            celltypes[[1]],
            as.character(object@meta.data[[ident]])
        )
        list(mapping = mapping, type = "cluster", cells = celltypes)
    }
}
