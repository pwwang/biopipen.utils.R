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

# ---- Vendored ScType functions ----------------------------------------------
# Copied verbatim from sc-type (https://github.com/IanevskiAleksandr/sc-type),
# as vendored by biopipen's scripts/scrna/sctype.R. Kept internal: the `sctype`
# runner sources its gene sets and scores through these.

# GNU General Public License v3.0 (https://github.com/IanevskiAleksandr/sc-type/blob/master/LICENSE)
# Written by Aleksandr Ianevski <aleksandr.ianevski@helsinki.fi>, June 2021
#
# Functions on this page:
# gene_sets_prepare: prepare gene sets and calculate marker sensitivity from input Cell Type excel file
#
# @params: path_to_db_file - DB file with cell types
# @cell_type - cell type (e.g. Immune system, Liver, Pancreas, Kidney, Eye, Brain)
#

gene_sets_prepare_ <- function(cell_markers) {
  cell_markers$geneSymbolmore1 = gsub(" ","",cell_markers$geneSymbolmore1); cell_markers$geneSymbolmore2 = gsub(" ","",cell_markers$geneSymbolmore2)

  # correct gene symbols from the given DB (up-genes)
  cell_markers$geneSymbolmore1 = sapply(1:nrow(cell_markers), function(i){

    markers_all = gsub(" ", "", unlist(strsplit(cell_markers$geneSymbolmore1[i],",")))
    markers_all = toupper(markers_all[markers_all != "NA" & markers_all != ""])
    markers_all = sort(markers_all)

    if(length(markers_all) > 0){
      suppressMessages({markers_all = unique(na.omit(checkGeneSymbols(markers_all)$Suggested.Symbol))})
      paste0(markers_all, collapse=",")
    } else {
      ""
    }
  })

  # correct gene symbols from the given DB (down-genes)
  cell_markers$geneSymbolmore2 = sapply(1:nrow(cell_markers), function(i){

    markers_all = gsub(" ", "", unlist(strsplit(cell_markers$geneSymbolmore2[i],",")))
    markers_all = toupper(markers_all[markers_all != "NA" & markers_all != ""])
    markers_all = sort(markers_all)

    if(length(markers_all) > 0){
      suppressMessages({markers_all = unique(na.omit(checkGeneSymbols(markers_all)$Suggested.Symbol))})
      paste0(markers_all, collapse=",")
    } else {
      ""
    }
  })

  cell_markers$geneSymbolmore1 = gsub("///",",",cell_markers$geneSymbolmore1);cell_markers$geneSymbolmore1 = gsub(" ","",cell_markers$geneSymbolmore1)
  cell_markers$geneSymbolmore2 = gsub("///",",",cell_markers$geneSymbolmore2);cell_markers$geneSymbolmore2 = gsub(" ","",cell_markers$geneSymbolmore2)

  gs = lapply(1:nrow(cell_markers), function(j) gsub(" ","",unlist(strsplit(toString(cell_markers$geneSymbolmore1[j]),",")))); names(gs) = cell_markers$cellName
  gs2 = lapply(1:nrow(cell_markers), function(j) gsub(" ","",unlist(strsplit(toString(cell_markers$geneSymbolmore2[j]),",")))); names(gs2) = cell_markers$cellName

  list(gs_positive = gs, gs_negative = gs2)
}

gene_sets_prepare <- function(path_to_db_file, cell_type = NULL){
  if (tolower(endsWith(path_to_db_file, ".xlsx"))) {
    cell_markers = openxlsx::read.xlsx(path_to_db_file)
  } else {
    cell_markers = read.csv(path_to_db_file, sep = "\t", header = T)
  }
  if (!is.null(cell_type)) {
    cell_markers = cell_markers[cell_markers$tissueType == cell_type,]
  }
  if (!is.null(cell_markers$Level)) {
    ulevels = sort(unique(cell_markers$Level))
    out = list()
    for (ul in ulevels) {
      cm = cell_markers[cell_markers$Level == ul,]
      out[[ul]] = gene_sets_prepare_(cm)
    }
  } else {
    out = list(gene_sets_prepare_(cell_markers))
  }
  out
}

# GNU General Public License v3.0 (https://github.com/IanevskiAleksandr/sc-type/blob/master/LICENSE)
# Written by Aleksandr Ianevski <aleksandr.ianevski@helsinki.fi>, June 2021
#
# Functions on this page:
# sctype_score: calculate ScType scores and assign cell types
#
# @params: scRNAseqData - input scRNA-seq matrix (rownames - genes, column names - cells),
# @params: scale - indicates whether the matrix is scaled (TRUE by default)
# @params: gs - list of gene sets positively expressed in the cell type
# @params: gs2 - list of gene sets that should not be expressed in the cell type (NULL if not applicable)

sctype_score <- function(scRNAseqData, scaled = !0, gs, gs2 = NULL, gene_names_to_uppercase = !0, ...){

  # check input matrix
  print("  sctype_score: Checking input matrix ...")
  if(!is.matrix(scRNAseqData)){
    warning("scRNAseqData doesn't seem to be a matrix")
  } else {
    if(sum(dim(scRNAseqData))==0){
       warning("The dimension of input scRNAseqData matrix equals to 0, is it an empty matrix?")
    }
  }

  # marker sensitivity
  print("  sctype_score: Calculating marker sensitivity ...")
  marker_stat = sort(table(unlist(gs)), decreasing = T);
  marker_sensitivity = data.frame(score_marker_sensitivity = scales::rescale(as.numeric(marker_stat), to = c(0,1), from = c(length(gs),1)),
                                      gene_ = names(marker_stat), stringsAsFactors = !1)

  # convert gene names to Uppercase
  print("  sctype_score: Converting gene names to Uppercase ...")
  if(gene_names_to_uppercase){
    rownames(scRNAseqData) = toupper(rownames(scRNAseqData));
  }

  # subselect genes only found in data
  print("  sctype_score: Subselecting genes only found in data ...")
  names_gs_cp = names(gs); names_gs_2_cp = names(gs2);
  gs = lapply(1:length(gs), function(d_){
    GeneIndToKeep = rownames(scRNAseqData) %in% as.character(gs[[d_]]); rownames(scRNAseqData)[GeneIndToKeep]})
  gs2 = lapply(1:length(gs2), function(d_){
    GeneIndToKeep = rownames(scRNAseqData) %in% as.character(gs2[[d_]]); rownames(scRNAseqData)[GeneIndToKeep]})
  names(gs) = names_gs_cp; names(gs2) = names_gs_2_cp;
  cell_markers_genes_score = marker_sensitivity[marker_sensitivity$gene_ %in% unique(unlist(gs)),]

  # z-scale if not
  print("  sctype_score: Z-scaling ...")
  if(!scaled) Z <- t(scale(t(scRNAseqData))) else Z <- scRNAseqData

  # multiple by marker sensitivity
  print("  sctype_score: Multiplying by marker sensitivity ...")
  for(jj in 1:nrow(cell_markers_genes_score)){
    Z[cell_markers_genes_score[jj,"gene_"], ] = Z[cell_markers_genes_score[jj,"gene_"], ] * cell_markers_genes_score[jj, "score_marker_sensitivity"]
  }

  # subselect only with marker genes
  print("  sctype_score: Subselecting only with marker genes ...")
  Z = Z[unique(c(unlist(gs),unlist(gs2))), ]

  # combine scores
  print("  sctype_score: Combining scores ...")
  gfun <- function(gss_) {
    gs_z = Z[gs[[gss_]], , drop=FALSE]
    len_z = sqrt(nrow(gs_z))
    s_z = colSums(gs_z) / len_z
    if (length(gs2[[gss_]]) == 0) {
      s_2 = 0
    } else {
      gs_2 = Z[gs2[[gss_]], , drop=FALSE] * -1
      len_2 = sqrt(nrow(gs_2))
      s_2 = colSums(gs_2) / len_2
      s_2[is.na(s_2)] = 0
    }
    s_z + s_2
  }

  es = data.frame(t(matrix(unlist(lapply(names(gs), gfun)), ncol=length(gs))))

  dimnames(es) = list(names(gs), colnames(Z))
  es.max <- es[!apply(is.na(es) | es == "", 1, all),] # remove na rows

  es.max
}

# ---- hitype ----------------------------------------------------------------

.run_celltypeannotation_hitype <- function(object, args, ident, ctx) {
    library(hitype)

    log <- get_logger()

    tissue <- args$tissue
    cancer <- args$cancer
    species <- args$species
    db <- args$db
    norm <- args$norm %||% "sqrt"
    use_sensitivity <- args$use_sensitivity %||% TRUE
    threshold <- args$threshold %||% 0.0

    if (is.null(db)) { stop("`envs.hitype.db` is not set") }

    # prepare gene sets
    log$info("Preparing gene sets...")
    if (startsWith(db, "hitypedb_") && !grepl(".", db, fixed = TRUE)) {
        # Built-in databases have no cancer/species columns
        stop_on_filtering_native_db(NULL, cancer, species)
        gs_list <- gs_prepare(eval(as.symbol(db)), tissue)
    } else {
        db_markers <- load_marker_table(db)
        if (!is.data.frame(db_markers) || !is_marker_canonical(db_markers)) {
            # Native ScType xlsx/TSV/RDS formats have no cancer/species columns
            # (tissue is still handled via gs_prepare below)
            stop_on_filtering_native_db(NULL, cancer, species)
        }
        if (is.character(db_markers)) {
            # native ScType xlsx passthrough
            gs_list <- gs_prepare(db_markers, tissue)
        } else {
            if (!is.data.frame(db_markers)) {
                stop("Cannot recognize the hitype database format. ",
                     "Use a ScType xlsx/TSV, RDS data.frame, or a universal marker table.")
            }
            if (is_marker_canonical(db_markers)) {
                # A universal marker table — consumed natively by hitype
                # (>= 0.0.6). Filter the rows by
                # `envs.tissue`/`envs.cancer`/`envs.species` here and keep
                # the table as is: notably a numeric `weight` column must
                # survive (markers_to_sctype_df(), the sctype route, would
                # drop it).
                if (packageVersion("hitype") < "0.0.6") {
                    stop(paste0(
                        "Universal marker tables with `tool = 'hitype'` ",
                        "require hitype >= 0.0.6 (installed: ",
                        as.character(packageVersion("hitype")), "). ",
                        "Install the latest hitype or use a native ",
                        "db-format file."
                    ))
                }
                db_markers <- apply_marker_filters(
                    db_markers, tissue = tissue, cancer = cancer, species = species
                )
                # Tissues already filtered above; gs_prepare accepts a
                # data.frame directly
                gs_list <- gs_prepare(db_markers, NULL)
            } else {
                # Native hitype/ScType db-format data.frame
                gs_list <- gs_prepare(db_markers, tissue)
            }
        }
    }

    # RunHitype() supports both annotation levels through its `ident`
    # argument: `NULL` assigns a cell type to each cell (cell-level), a
    # metadata column assigns one cell type per cluster of that column
    # (cluster-level). `case$ident` arrives as `NULL` or a column name
    # (`"ident"` was already resolved to the identity column in the main
    # script), so it maps 1:1. Older hitype builds without the `ident`
    # argument fail here with an "unused argument" error — update hitype.
    log$info("Running RunHitype...")
    object <- RunHitype(
        object, gs_list, ident = ident, threshold = threshold, make_unique = TRUE,
        norm = norm, use_sensitivity = use_sensitivity
    )

    if (is.null(ident)) {
        # cell-level: one label per cell
        log$info("Extracting per-cell labels...")
        list(
            mapping = data.frame(
                hitype = object@meta.data$hitype,
                row.names = colnames(object),
                stringsAsFactors = FALSE
            ),
            type = "cell"
        )
    } else {
        log$info("Extracting cell type labels...")
        hitype_labels <- object@meta.data %>%
            distinct(!!sym(ident), hitype)
        hitype_labels <- stats::setNames(
            as.list(hitype_labels$hitype),
            hitype_labels[[ident]]
        )

        list(mapping = hitype_labels, type = "cluster")
    }
}

# ---- sctype ----------------------------------------------------------------

.run_celltypeannotation_sctype <- function(object, args, ident, ctx) {
    library(HGNChelper)

    log <- get_logger()

    tissue <- args$tissue
    cancer <- args$cancer
    species <- args$species
    db <- args$db

    if (is.null(db)) { stop("`envs.sctype.db` is not set") }

    # prepare gene sets
    log$info("Preparing gene sets...")
    db_markers <- load_marker_table(db)
    if (!is.data.frame(db_markers) || !is_marker_canonical(db_markers)) {
        # Native ScType xlsx/TSV/RDS formats have no cancer/species columns
        # (tissue is still handled via gene_sets_prepare below)
        stop_on_filtering_native_db(NULL, cancer, species)
    }
    if (is.character(db_markers)) {
        # native ScType xlsx passthrough
        gs_list <- gene_sets_prepare(db_markers, tissue)
    } else {
        if (!is.data.frame(db_markers)) {
            stop("Cannot recognize the sctype database format. ",
                 "Use a ScType xlsx/TSV or a universal marker table.")
        }
        if (is_marker_canonical(db_markers)) {
            if (!is.null(tissue) && !"tissue" %in% colnames(db_markers)) {
                stop(paste0(
                    "`envs.sctype.tissue` is set to `", tissue,
                    "` but the marker table has no `tissue` column."
                ))
            }
            db_markers <- markers_to_sctype_df(db_markers, tissue, cancer, species)
        }
        # gene_sets_prepare only takes a path
        tmp_db <- tempfile(fileext = ".tsv")
        write.table(
            db_markers, tmp_db,
            sep = "\t", quote = FALSE, row.names = FALSE
        )
        gs_list <- gene_sets_prepare(tmp_db, tissue)
    }

    scRNAseqData <- GetAssayData(object, layer = "scale.data")
    idents <- as.character(unique(object@meta.data[[ident]]))
    idents <- idents[order(as.numeric(idents))]

    log$info("Working on different levels of cell type labels ...")
    cell_types_list <- list()
    for (i in seq_along(gs_list)) {
        log$info("- Working on level {i} ...")
        if (is.null(gs_list[[i]])) next

        log$info("  Calculating cell-type scores ...")
        es.max <- sctype_score(
            scRNAseqData = scRNAseqData,
            scaled = TRUE,
            gs = gs_list[[i]]$gs_positive,
            gs2 = gs_list[[i]]$gs_negative
        )

        log$info("  Merging cell-type scores by cluster ...")
        cl_resutls <- do_call(
            "rbind",
            lapply(
                idents,
                function(cl) {
                    es.max.cl <- sort(
                        rowSums(es.max[
                            ,
                            rownames(object@meta.data[
                                object@meta.data[[ident]] == cl,
                            ])
                        ]),
                        decreasing = !0
                    )
                    head(data.frame(
                        cluster = cl,
                        type = names(es.max.cl),
                        scores = es.max.cl,
                        ncells = sum(object@meta.data[[ident]] == cl)
                    ), 10)
                }
            )
        )

        sctype_scores <- cl_resutls %>%
            group_by(cluster) %>%
            slice_max(scores, n = 1, with_ties = TRUE)

        if (nrow(sctype_scores) > length(idents)) {
            sctype_scores_count <- sctype_scores %>%
                count(cluster) %>%
                filter(n > 1)
            write("\n########## sctype_scores ###########", stderr())
            write(capture.output(sctype_scores), stderr())
            write("\n####### sctype_scores_count ########", stderr())
            write(capture.output(sctype_scores_count), stderr())
            write("\n####################################", stderr())
            log$info(
                "  Scores tied in the above clusters.",
                immediate. = TRUE
            )
        }

        if (length(gs_list) == 1 || i > 1) {
            # set low-confident (low ScType score) clusters to "unknown"
            log$info("  Setting low-confident clusters to 'Unknown'...")
            sctype_scores$type[
                as.numeric(as.character(sctype_scores$scores)) <
                    sctype_scores$ncells / 4
            ] <- "Unknown"
        }

        celltypes <- sapply(
            idents,
            function(cl) {
                cl_type <- sctype_scores[sctype_scores$cluster == cl, ]
                as.character(cl_type$type[1])
            }
        )
        names(celltypes) <- idents
        cell_types_list[[i]] <- celltypes
    }

    if (length(cell_types_list) == 1) {
        celltypes <- as.list(cell_types_list[[1]])
    } else {
        log$info("Merging cell types at all levels ...")
        celltypes <- list()

        for (i in idents) {
            celltypes[[i]] <- ""
            for (j in seq_along(cell_types_list)) {
                idt <- cell_types_list[[j]][[i]]
                if (idt != "Unknown") {
                    celltypes[[i]] <- paste(celltypes[[i]], idt)
                }
            }
        }
    }

    list(mapping = celltypes, type = "cluster")
}

# ---- scsorter ---------------------------------------------------------------

.run_celltypeannotation_scsorter <- function(object, args, ident, ctx) {
    # Check if RunScSorter from the scSorter package is available
    # If not, this is the cran/scSorter package, which doesn't support RunScSorter
    # on Seurat objects
    # We need pwwang/scSorter, which is a fork of scSorter that supports Seurat objects
    if (!requireNamespace("scSorter", quietly = TRUE) ||
        !"RunScSorter" %in% getNamespaceExports("scSorter")) {
        stop(paste(
            "The scSorter package is not installed or does not support RunScSorter.",
            "Please install the pwwang/scSorter package from GitHub."
        ))
    }
    library(scSorter)

    log <- get_logger()
    db <- args$db

    if (is.null(db)) { stop("`envs.scsorter.db` is not set") }

    log$info("Loading scSorter database ...")
    anno <- load_marker_table(db)
    if (!is.data.frame(anno)) {
        stop(paste0(
            "scSorter database must be a data.frame, got: ",
            paste(class(anno), collapse = ", ")
        ))
    }
    if (is_marker_canonical(anno)) {
        anno <- markers_to_scsorter_df(
            anno,
            tissue = args$tissue,
            cancer = args$cancer,
            species = args$species
        )
    } else {
        # Native positional tables have no tissue/cancer/species columns
        stop_on_filtering_native_db(
            args$tissue, args$cancer, args$species
        )
        if (ncol(anno) < 2) {
            stop(paste0(
                "scSorter database file must have at least 2 columns: ",
                db
            ))
        }
        if (ncol(anno) == 2) {
            colnames(anno) <- c("Type", "Marker")
        } else if (ncol(anno) >= 3) {
            colnames(anno)[1:3] <- c("Type", "Marker", "Weight")
        }
    }
    # The filter envs are consumed by the marker conversion above; never
    # forward them to RunScSorter()
    args$tissue <- NULL
    args$cancer <- NULL
    args$species <- NULL

    log$info("Running RunScSorter...")
    # Set the active identity to the ident column
    Idents(object) <- ident
    args$db <- NULL  # db is passed separately as scsorter_db
    args$object <- object
    args$anno <- anno
    args$mc.cores <- args$mc.cores %||% 1L
    object <- do_call(RunScSorter, args)

    # RunScSorter stores per-cell predictions in the scSorter_celltype column;
    # aggregate to one type per cluster by majority vote
    log$info("Aggregating scSorter results by cluster...")
    mapping <- majority_vote(
        object@meta.data$scSorter_celltype,
        as.character(object@meta.data[[ident]])
    )

    list(mapping = mapping, type = "cluster")
}

# ---- singler ----------------------------------------------------------------

.run_celltypeannotation_singler <- function(object, args, ident, ctx) {
    library(SingleR)

    log <- get_logger()
    db <- args$db

    if (is.null(db)) { stop("`envs.singler.db` is not set") }

    if (startsWith(db, "file://")) {
        db <- sub("^file://", "", db)
    }

    if (!file.exists(db)) {
        stop(paste0("SingleR database file does not exist: ", db))
    }

    # Detect which SingleR API is available:
    # - Bioconductor (LTLA): SingleR(test, ref, labels, clusters, ...)
    # - CRAN (dviraran):    SingleR(sc_data, ref_data, types, clusters, ...)
    is_bioc <- "test" %in% names(formals(SingleR))
    log$info(
        "Using SingleR {ifelse(is_bioc, 'Bioconductor', 'CRAN')} API"
    )

    # Load reference (supports RDS, qs, qs2 via biopipen.utils)
    log$info("Loading SingleR reference ...")
    ref <- read_obj(db)

    # Resolve label column
    label_col <- args$label
    args$label <- NULL
    args$db <- NULL  # db is passed separately as singler_db

    # Prepare reference data and labels based on API version
    if (is_bioc) {
        # Bioconductor API: ref is a SummarizedExperiment, labels is a vector
        if (inherits(ref, "Seurat")) {
            log$info("Converting Seurat reference to SummarizedExperiment ...")
            ref <- as.SingleCellExperiment(ref)
        }

        if (!is.null(label_col)) {
            if (is(ref, "SummarizedExperiment")) {
                labels <- SummarizedExperiment::colData(ref)[[label_col]]
            } else {
                stop(paste0("Label column '", label_col, "' not found"))
            }
        } else if (is(ref, "SummarizedExperiment")) {
            labels <- NULL
            for (col in c(
                "label.main", "label.fine", "label.ont", "label"
            )) {
                if (col %in% colnames(SummarizedExperiment::colData(ref))) {
                    labels <- SummarizedExperiment::colData(ref)[[col]]
                    log$info("Auto-detected label column: {col}")
                    break
                }
            }
        } else {
            stop(paste(
                "Reference must be a Seurat or SummarizedExperiment object."
            ))
        }

        if (is.null(labels)) {
            stop(paste(
                "Cannot determine labels from reference.",
                "Set `label` in `envs.singler.label`."
            ))
        }

        # Bioconductor SingleR call
        log$info("Preparing expression matrix ...")
        exp <- as.matrix(GetAssayData(object, layer = "data"))

        clusters <- as.character(object@meta.data[[ident]])
        log$info(
            "Running SingleR with {length(unique(clusters))} clusters ..."
        )

        args$test <- exp
        args$ref <- ref
        args$labels <- labels
        args$clusters <- clusters

        results <- do_call(SingleR, args)

        # Build mapping (prefer pruned.labels, fall back to labels)
        mapping <- as.list(results$pruned.labels)
        names(mapping) <- rownames(results)
        na_mask <- is.na(mapping) | mapping == "NA"
        if (any(na_mask)) {
            mapping[na_mask] <- as.list(results$labels[na_mask])
        }

    } else {
        # CRAN API: ref_data is a matrix, types is a vector
        if (inherits(ref, "Seurat")) {
            log$info("Extracting data from Seurat reference ...")
            ref_data <- as.matrix(GetAssayData(ref, layer = "data"))
            meta <- ref@meta.data
        } else if (is(ref, "SummarizedExperiment")) {
            assay_name <- "logcounts"
            if (!assay_name %in% names(SummarizedExperiment::assays(ref))) {
                assay_name <- names(SummarizedExperiment::assays(ref))[1]
            }
            ref_data <- as.matrix(
                SummarizedExperiment::assay(ref, assay_name)
            )
            meta <- SummarizedExperiment::colData(ref)
        } else {
            stop(paste(
                "Reference must be a Seurat or SummarizedExperiment object."
            ))
        }

        if (!is.null(label_col)) {
            if (!label_col %in% colnames(meta)) {
                stop(paste0(
                    "Label column '", label_col, "' not found in reference"
                ))
            }
            types <- meta[[label_col]]
        } else {
            types <- NULL
            for (col in c(
                "label.main", "label.fine", "label.ont", "label"
            )) {
                if (col %in% colnames(meta)) {
                    types <- meta[[col]]
                    log$info("Auto-detected label column: {col}")
                    break
                }
            }
        }

        if (is.null(types)) {
            stop(paste(
                "Cannot determine cell types from reference.",
                "Set `label` in `envs.singler.label`."
            ))
        }

        # CRAN SingleR call
        log$info("Preparing expression matrix ...")
        sc_data <- as.matrix(GetAssayData(object, layer = "data"))

        clusters <- as.factor(object@meta.data[[ident]])
        log$info(
            "Running SingleR with {length(levels(clusters))} clusters ..."
        )

        args$method <- "cluster"
        args$sc_data <- sc_data
        args$ref_data <- ref_data
        args$types <- types
        args$clusters <- clusters

        results <- do_call(SingleR, args)

        # Build mapping from labels
        mapping <- as.list(results$labels)
    }

    list(mapping = mapping, type = "cluster")
}

# ---- scina ------------------------------------------------------------------

.run_celltypeannotation_scina <- function(object, args, ident, ctx) {
    library(SCINA)

    log <- get_logger()
    db <- args$db

    if (is.null(db)) { stop("`envs.scina.db` is not set") }

    log$info("Loading SCINA signature file ...")
    mt <- load_marker_table(db)
    if (!is_marker_canonical(mt)) {
        # Native signature formats have no tissue/cancer/species columns
        stop_on_filtering_native_db(
            args$tissue, args$cancer, args$species
        )
    }
    if (is_marker_canonical(mt)) {
        signatures <- markers_to_named_list(
            mt,
            tissue = args$tissue,
            cancer = args$cancer,
            species = args$species
        )
    } else if (is.data.frame(mt)) {
        # Native per-cell-type-column CSV/TSV (one column per cell type)
        signatures <- lapply(mt, function(x) x[!is.na(x) & x != ""])
    } else if (is.list(mt)) {
        signatures <- mt  # RDS named list
    } else {
        stop("Cannot recognize the SCINA signature file format.")
    }
    # The filter envs are consumed by the marker conversion above; never
    # forward them to SCINA() which has no such arguments
    args$tissue <- NULL
    args$cancer <- NULL
    args$species <- NULL

    if (!is.list(signatures) || is.null(names(signatures))) {
        stop("SCINA signatures must be a named list")
    }

    # Get expression matrix (log-normalized, genes x cells)
    log$info("Preparing expression matrix...")
    exp <- as.matrix(GetAssayData(object, layer = "data"))

    # Drop signature genes not in the expression matrix, and simulate SCINA's
    # rm_overlap removal, so signatures emptied by either are dropped here
    # with a clear warning (SCINA chokes on an empty signature with a cryptic
    # chol() error).
    log$info("Filtering signatures against the expression matrix...")
    signatures <- lapply(
        signatures,
        function(x) unique(x[x %in% row.names(exp)])
    )
    rm_overlap <- args$rm_overlap %||% formals(SCINA)$rm_overlap %||% 1
    if (isTRUE(rm_overlap) || rm_overlap == 1) {
        counts <- table(unlist(signatures))
        signatures <- lapply(
            signatures,
            function(x) x[x %in% names(counts[counts == 1])]
        )
    }
    signatures <- lapply(
        signatures,
        function(x) x[apply(exp[x, , drop = F], 1, sd) > 0]
    )
    n_genes <- lengths(signatures)
    if (any(n_genes == 0)) {
        log$warn(sprintf(
            paste(
                "Dropping %d signature(s) with no genes in the expression matrix:",
                "%s"
            ),
            sum(n_genes == 0),
            paste(names(n_genes)[n_genes == 0], collapse = ", ")
        ))
        signatures <- signatures[n_genes > 0]
    }
    if (length(signatures) == 0) {
        stop("No SCINA signatures have genes in the expression matrix.")
    }

    # Run SCINA
    log$info("Running SCINA...")
    args$db <- NULL  # db is passed separately as scina_db
    args$exp <- exp
    args$signatures <- signatures
    results <- do_call(SCINA, args)

    cell_labels <- results$cell_labels
    result <- data.frame(
        scina_celltype = unname(cell_labels),
        row.names = names(cell_labels)
    )

    if (is.null(ident)) {
        list(mapping = result, type = "cell")
    } else {
        # Aggregate per-cell results to cluster-level mapping (majority vote)
        log$info("Aggregating SCINA results by cluster...")
        mapping <- majority_vote(
            cell_labels, as.character(object@meta.data[[ident]])
        )
        list(mapping = mapping, type = "cluster", cells = result)
    }
}

# ---- cellid -----------------------------------------------------------------

.run_celltypeannotation_cellid <- function(object, args, ident, ctx) {
    library(CelliD)

    log <- get_logger()
    db <- args$db

    if (is.null(db)) {
        stop("`envs.cellid.db` is required for CelliD annotation")
    }

    # Load marker gene list from file
    marker_info <- load_marker_table(db)
    if (!is_marker_canonical(marker_info)) {
        # Native signature formats have no tissue/cancer/species columns
        stop_on_filtering_native_db(
            args$tissue, args$cancer, args$species
        )
    }
    if (is_marker_canonical(marker_info)) {
        pathways <- markers_to_named_list(
            marker_info,
            tissue = args$tissue,
            cancer = args$cancer,
            species = args$species
        )
        args$tissue <- NULL
        args$cancer <- NULL
        args$species <- NULL
    } else if (is.data.frame(marker_info)) {
        stop("CSV/TSV must have 'gene' and 'cell_type' columns.")
    } else if (is.list(marker_info)) {
        pathways <- marker_info
    } else {
        stop("CelliD marker gene info must be a named list.")
    }

    nmcs <- args$nmcs %||% 50
    n_features <- args$n_features %||% 200
    dims <- args$dims %||% seq(nmcs)
    min_size <- args$min_size %||% 10
    log_trans <- args$log_trans %||% TRUE
    p_adjust <- args$p_adjust %||% TRUE

    # Run MCA
    log$info("Running MCA with {nmcs} components ...")
    object <- RunMCA(object, nmcs = nmcs)

    # Run per-cell hypergeometric test
    log$info(
        "Running per-cell hypergeometric test against {length(pathways)} gene sets ..."
    )
    enrichment <- RunCellHGT(
        X = object,
        pathways = pathways,
        reduction = "mca",
        n.features = n_features,
        dims = dims,
        minSize = min_size,
        log.trans = log_trans,
        p.adjust = p_adjust
    )

    # Convert enrichment matrix to cell type predictions (argmax per cell)
    # enrichment is: pathways (cell types) x cells
    enrichment <- as.matrix(enrichment)
    cell_type_names <- rownames(enrichment)
    pred_idx <- apply(enrichment, 2, which.max)
    predicted <- cell_type_names[pred_idx]
    names(predicted) <- colnames(enrichment)

    # Build cell annotations data frame
    result <- data.frame(
        cellid_celltype = predicted,
        row.names = names(predicted)
    )

    if (is.null(ident)) {
        list(mapping = result, type = "cell")
    } else {
        # Aggregate per-cell results to cluster-level mapping (majority vote)
        log$info("Aggregating CelliD results by cluster...")
        mapping <- majority_vote(
            predicted, as.character(object@meta.data[[ident]])
        )
        list(mapping = mapping, type = "cluster", cells = result)
    }
}

# ---- sccatch ----------------------------------------------------------------

.run_celltypeannotation_sccatch <- function(object, args, ident, ctx) {
    library(scCATCH)

    log <- get_logger()

    if (!is.null(args$marker)) {
        cellmatch <- load_marker_table(args$marker)
        if (is_marker_canonical(cellmatch)) {
            cellmatch <- markers_to_sccatch_df(
                cellmatch,
                tissue = args$tissue,
                cancer = args$cancer,
                species = args$species
            )
            args$tissue <- NULL
            args$cancer <- NULL
            args$species <- NULL
        } else if (!is.data.frame(cellmatch)) {
            stop("The custom marker file for scCATCH must be a table or data.frame.")
        } else {
            # A non-universal marker data.frame: filter by the columns if the
            # filter envs are set. scCATCH itself won't filter the markers
            # when `if_use_custom_marker` is TRUE (see below).
            cellmatch <- apply_marker_filters(
                cellmatch,
                tissue = args$tissue,
                cancer = args$cancer,
                species = args$species
            )
        }
        args$if_use_custom_marker <- TRUE
    } else {
        args$cancer <- args$cancer %||% "Normal"
    }
    args$marker <- cellmatch

    if (is.integer(args$use_method)) {
        args$use_method <- as.character(args$use_method)
    }

    # Check if there is less than 2 clusters
    num_clusters <- length(unique(object@meta.data[[ident]]))
    if (num_clusters < 2) {
        stop(paste(
            "The number of clusters is less than 2.",
            "Sccatch requires at least 2 clusters to perform cell type annotation."
        ))
    }

    log$info("Running createscCATCH ...")
    obj <- createscCATCH(
        data = GetAssayData(object, assay = args$assay),
        cluster = as.character(object@meta.data[[ident]])
    )
    args$object <- obj

    log$info("Running findmarkergene ...")
    obj <- do_call(findmarkergene, args)

    log$info("Running findcelltype ...")
    obj <- findcelltype(object = obj)

    celltypes <- as.list(obj@celltype$cell_type)
    names(celltypes) <- obj@celltype$cluster

    if (length(celltypes) == 0) {
        log$warn("- No cell types annotated from the database!")
    }

    list(mapping = celltypes, type = "cluster")
}

# ---- llmcelltype ------------------------------------------------------------

.run_celltypeannotation_llmcelltype <- function(object, args, ident, ctx) {
    library(LLMCellType)

    log <- get_logger()

    llmcelltype_argnames <- formalArgs(LLMCellType::llmcelltype)
    markers_args <- args[setdiff(names(args), llmcelltype_argnames)]
    args <- args[intersect(names(args), llmcelltype_argnames)]

    # Run FindAllMarkers
    log$info("Find the markers for {ident} ...")
    markers_args$object <- object
    markers_args$group_by <- ident
    markers_args$ident_1 <- NULL
    markers_args$ident_2 <- NULL
    markers_args$log_prefix <- " * "
    markers_args$log <- log

    sigmarkers <- markers_args$sigmarkers
    markers_args$sigmarkers <- NULL

    args$input <- do_call(RunSeuratDEAnalysis, markers_args)
    colnames(args$input)[ncol(args$input)] <- "cluster"
    if (!is.null(sigmarkers)) {
        log$info("Filtering markers with sigmarkers: {sigmarkers}")
        args$input <- filter(args$input, !!parse_expr(sigmarkers))
    }
    rm(object)
    rm(markers_args)
    gc()

    # Run LLMCelltype
    log$info("Running LLMCelltype with model '{args$model}' ...")
    res <- do_call(LLMCellType::llmcelltype, args)
    if (isTRUE(args$return_prompt)) {
        stop("LLMCellType prompt:\n\n", res, "\n\n")
    }
    if (is.character(res) && length(res) == 1 && startsWith(res, "Identify cell types")) {
        stop("LLMCellType failed, do you have the correct API key set?")
    }

    # Build mapping (res is named vector: cluster → cell type)
    mapping <- as.list(res)
    list(mapping = mapping, type = "cluster")
}
