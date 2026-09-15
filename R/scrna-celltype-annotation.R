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
      suppressMessages({markers_all = unique(na.omit(HGNChelper::checkGeneSymbols(markers_all)$Suggested.Symbol))})
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
      suppressMessages({markers_all = unique(na.omit(HGNChelper::checkGeneSymbols(markers_all)$Suggested.Symbol))})
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
        gs_list <- hitype::gs_prepare(getExportedValue("hitype", db), tissue)
    } else {
        db_markers <- load_marker_table(db)
        if (!is.data.frame(db_markers) || !is_marker_canonical(db_markers)) {
            # Native ScType xlsx/TSV/RDS formats have no cancer/species columns
            # (tissue is still handled via gs_prepare below)
            stop_on_filtering_native_db(NULL, cancer, species)
        }
        if (is.character(db_markers)) {
            # native ScType xlsx passthrough
            gs_list <- hitype::gs_prepare(db_markers, tissue)
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
                gs_list <- hitype::gs_prepare(db_markers, NULL)
            } else {
                # Native hitype/ScType db-format data.frame
                gs_list <- hitype::gs_prepare(db_markers, tissue)
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
    object <- hitype::RunHitype(
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
    object <- do_call(scSorter::RunScSorter, args)

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
    is_bioc <- "test" %in% names(formals(SingleR::SingleR))
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
            ref <- Seurat::as.SingleCellExperiment(ref)
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

        results <- do_call(SingleR::SingleR, args)

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

        results <- do_call(SingleR::SingleR, args)

        # Build mapping from labels
        mapping <- as.list(results$labels)
    }

    list(mapping = mapping, type = "cluster")
}

# ---- scina ------------------------------------------------------------------

.run_celltypeannotation_scina <- function(object, args, ident, ctx) {
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
    rm_overlap <- args$rm_overlap %||% formals(SCINA::SCINA)$rm_overlap %||% 1
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
    results <- do_call(SCINA::SCINA, args)

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
    object <- CelliD::RunMCA(object, nmcs = nmcs)

    # Run per-cell hypergeometric test
    log$info(
        "Running per-cell hypergeometric test against {length(pathways)} gene sets ..."
    )
    enrichment <- CelliD::RunCellHGT(
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
    obj <- scCATCH::createscCATCH(
        data = GetAssayData(object, assay = args$assay),
        cluster = as.character(object@meta.data[[ident]])
    )
    args$object <- obj

    log$info("Running findmarkergene ...")
    obj <- do_call(scCATCH::findmarkergene, args)

    log$info("Running findcelltype ...")
    obj <- scCATCH::findcelltype(object = obj)

    celltypes <- as.list(obj@celltype$cell_type)
    names(celltypes) <- obj@celltype$cluster

    if (length(celltypes) == 0) {
        log$warn("- No cell types annotated from the database!")
    }

    list(mapping = celltypes, type = "cluster")
}

# ---- llmcelltype ------------------------------------------------------------

.run_celltypeannotation_llmcelltype <- function(object, args, ident, ctx) {
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

# ---- garnett ----------------------------------------------------------------

# patch_garnett_make_predictions — replacement for the wrapper function of the
# same name in biopipen/scripts/scrna/CellTypeAnnotation-garnett.R
#
# Root cause being fixed: with glmnet >= 4.0, predict(cv.glmnet, type="response")
# on a multinomial fit returns a rank-3 array [cells, classes, s]. garnett's
# `as.matrix(as.data.frame(temp))` then pastes dim2 and dim3 names together, so
# the class columns come out as "B cell.lambda.min" instead of "B cell", and
# every class name is mangled before the which.max()/gate logic runs. Fix: cut
# the singleton `s` dimension down to a plain cells x classes matrix before any
# data.frame conversion, strip any residual ".lambda.<s>" / ".s=<x>" / ".1"
# suffix, and refuse loudly if the class dimension cannot be established
# (all-Unknown is a legitimate result; silently collapsing the classes is not).
#
# Usage: replace the existing patch_garnett_make_predictions() in
# CellTypeAnnotation-garnett.R with this function. The rest of the wrapper is
# unchanged (it is still called once at the top of annotate_garnett()).
#' Patch garnett's make_predictions for glmnet >= 4.0 multinomial fits
#'
#' @param log Logger
#' @return Invisibly, the patched namespace function
#' @export
patch_garnett_make_predictions <- function(log) {
    make_predictions_fixed <- function(cds, classifier, curr_node,
                                       rank_prob_ratio, cores = 1, s) {
        cvfit <- igraph::V(classifier@classification_tree)[curr_node]$model[[1]]
        predictions <- tryCatch({
            if (is.null(cvfit)) {
                child_cell_types <- igraph::V(
                    classifier@classification_tree
                )[suppressWarnings(igraph::.outnei(curr_node))]$name
                predictions <- matrix(
                    FALSE,
                    nrow = nrow(SummarizedExperiment::colData(cds)),
                    ncol = length(child_cell_types),
                    dimnames = list(row.names(SummarizedExperiment::colData(cds)), child_cell_types)
                )
                predictions <- split(
                    predictions,
                    rep(1:ncol(predictions), each = nrow(predictions))
                )
                names(predictions) <- child_cell_types
                predictions
            } else {
                candidate_model_genes <-
                    cvfit$glmnet.fit$beta[[1]]@Dimnames[[1]]
                good_genes <- intersect(
                    row.names(SingleCellExperiment::counts(cds)), candidate_model_genes
                )
                if (length(good_genes) == 0) {
                    stop(paste(
                        "None of the model genes are in your CDS object.",
                        "Did you specify the correct cds_gene_id_type and",
                        "the correct db?"
                    ))
                }
                x <- Matrix::t(SingleCellExperiment::counts(cds[
                    intersect(
                        row.names(SingleCellExperiment::counts(cds)), candidate_model_genes
                    ),
                ]))
                extra <- as(matrix(
                    0,
                    nrow = nrow(x),
                    ncol = length(setdiff(candidate_model_genes, colnames(x)))
                ), "sparseMatrix")
                row.names(extra) <- row.names(x)
                colnames(extra) <- setdiff(candidate_model_genes, colnames(x))
                x <- cbind(x, extra)
                x <- x[, candidate_model_genes]
                nonz <- Matrix::rowSums(do.call(
                    cbind, glmnet::coef.glmnet(cvfit, s = "lambda.min")
                ))
                nonz <- nonz[2:length(nonz)]
                nonz <- names(nonz[nonz != 0])
                if (sum(!nonz %in% row.names(SingleCellExperiment::counts(cds))) > 0) {
                    warning(paste(
                        "The following genes used in the classifier are not",
                        "present in the input CDS. Interpret with caution.",
                        nonz[!nonz %in% row.names(SingleCellExperiment::counts(cds))]
                    ))
                }
                temp <- stats::predict(cvfit, newx = x, s = s, type = "response")
                temp[is.nan(temp)] <- 0
                # --- FIX (a): with glmnet >= 4 the multinomial response is a
                # [cells, classes, s] array. Rebuild it as a plain cells x
                # classes matrix before any data.frame conversion. Two traps:
                # `temp[, , 1, drop = FALSE]` keeps rank 3, so as.data.frame()
                # below pastes dim2+dim3 names together and every class comes
                # out as "<class>.<s-label>"; plain `temp[, , 1]` instead drops
                # any length-1 dim, so a single-class model collapses to a bare
                # vector and the class name is lost. Build the matrix by hand
                # so exactly the class labels survive either way.
                if (is.array(temp) && length(dim(temp)) > 2) {
                    if (length(dim(temp)) != 3) {
                        stop(paste(
                            "Unexpected predict() rank",
                            length(dim(temp)), "(dim =",
                            paste(dim(temp), collapse = " x "),
                            "); expected [cells, classes, s]"
                        ))
                    }
                    if (dim(temp)[3] != 1) {
                        stop(paste(
                            "Expected a single `s` slot in the predict() output,",
                            "got", dim(temp)[3], "(dim =",
                            paste(dim(temp), collapse = " x "),
                            "); refusing to guess the class dimension"
                        ))
                    }
                    temp <- matrix(
                        temp[, , 1],
                        nrow = dim(temp)[1],
                        ncol = dim(temp)[2],
                        dimnames = list(dimnames(temp)[[1]], dimnames(temp)[[2]])
                    )
                }
                if (is.null(temp) || is.null(dim(temp)) || ncol(temp) < 2) {
                    stop(paste(
                        "The classifier returned no usable class dimension",
                        "(dim =", paste(dim(temp), collapse = " x "),
                        "); cannot classify"
                    ))
                }
                colnames(temp) <- sub("\\.lambda\\..*$", "", colnames(temp))
                colnames(temp) <- sub("\\.s=.*$", "", colnames(temp))
                colnames(temp) <- sub("\\.1$", "", colnames(temp))
                prediction_probs <- as.matrix(as.data.frame(temp))
                prediction_probs <-
                    prediction_probs / Biobase::rowMax(prediction_probs)
                prediction_probs[is.nan(prediction_probs)] <- 0
                prediction_probs <- apply(prediction_probs, 1, function(x) {
                    m <- names(which.max(x))
                    s <- sort(x, decreasing = T)
                    c(cell_type = m, odds_ratio = s[1] / s[2])
                })
                prediction_probs <- as.data.frame(t(prediction_probs))
                prediction_probs$cell_name <- row.names(prediction_probs)
                names(prediction_probs) <- c(
                    "cell_type", "odds_ratio", "cell_name"
                )
                prediction_probs$odds_ratio <- as.numeric(
                    as.character(prediction_probs$odds_ratio)
                )
                assignments <- prediction_probs[
                    prediction_probs$odds_ratio > rank_prob_ratio,
                ]
                random_guess_thresh <- 1 / length(cvfit$glmnet.fit$beta)
                assignments <- assignments[
                    assignments$odds_ratio > random_guess_thresh,
                ]
                not_assigned <- row.names(SummarizedExperiment::colData(cds))[
                    !row.names(SummarizedExperiment::colData(cds)) %in% assignments$cell_name
                ]
                if (length(not_assigned) > 0) {
                    assignments <- rbind(
                        assignments,
                        data.frame(
                            cell_name = not_assigned,
                            cell_type = NA, odds_ratio = NA
                        )
                    )
                }
                assignments$cell_type <- stringr::str_replace_all(
                    assignments$cell_type, "\\.1", ""
                )
                predictions <- reshape2::dcast(
                    assignments,
                    cell_name ~ cell_type,
                    value.var = "odds_ratio"
                )
                predictions <- predictions[!is.na(predictions$cell_name), ]
                row.names(predictions) <- predictions$cell_name
                if (ncol(predictions) > 2) {
                    predictions <- predictions[
                        ,
                        setdiff(colnames(predictions), "NA")
                    ]
                    predictions <- predictions[, -1, drop = FALSE]
                    predictions <- predictions[
                        rownames(SummarizedExperiment::colData(cds)), , drop = FALSE
                    ]
                    predictions <- as.matrix(predictions)
                    predictions[is.na(predictions)] <- FALSE
                    predictions[predictions != 0] <- TRUE
                    cell_type_names <- colnames(predictions)
                    predictions <- split(
                        predictions,
                        rep(1:ncol(predictions), each = nrow(predictions))
                    )
                    names(predictions) <- cell_type_names
                } else {
                    cell_type_names <- names(cvfit$glmnet.fit$beta)
                    one_type <- names(predictions)[2]
                    if (one_type == "NA") {
                        names(predictions)[2] <- "Unknown"
                        one_type <- "Unknown"
                    }
                    predictions <- matrix(
                        FALSE,
                        nrow = nrow(SummarizedExperiment::colData(cds)),
                        ncol = length(cell_type_names),
                        dimnames = list(
                            row.names(SummarizedExperiment::colData(cds)), cell_type_names
                        )
                    )
                    # --- FIX (b): "Unknown" (and any name outside the model's
                    # classes) must not index the matrix; leave the mask
                    # all-FALSE instead of crashing.
                    if (one_type %in% cell_type_names) {
                        predictions[, one_type] <- TRUE
                    }
                    predictions <- split(
                        predictions,
                        rep(1:ncol(predictions), each = nrow(predictions))
                    )
                    names(predictions) <- cell_type_names
                }
                predictions
            }
        }, error = function(e) {
            if (e$message == paste(
                "None of the model genes are in your CDS object.",
                "Did you specify the correct cds_gene_id_type and",
                "the correct db?"
            )) {
                stop(e)
            }
            print(e)
            cell_type_names <- names(cvfit$glmnet.fit$beta)
            predictions <- matrix(
                FALSE,
                nrow = nrow(SummarizedExperiment::colData(cds)),
                ncol = length(cell_type_names),
                dimnames = list(row.names(SummarizedExperiment::colData(cds)), cell_type_names)
            )
            predictions <- split(
                predictions,
                rep(1:ncol(predictions), each = nrow(predictions))
            )
            names(predictions) <- cell_type_names
            predictions
        })
    }
    tryCatch({
        monkey_patch("garnett", "make_predictions", make_predictions_fixed)
    }, error = function(e) {
        log$warn(paste(
            "Failed to patch garnett::make_predictions (glmnet >= 4.0 fix):",
            conditionMessage(e)
        ))
    })
}

# Workaround for garnett 0.2.22: the marker-file lexer (`t_NAME` in the rly
# `Lexer` in garnett's namespace) only accepts ASCII letters, so a cell type
# like "γδ-T cells" from ScTypeDB fails with `Marker file error. Syntax error
# 'γ' ...` when train_cell_classifier() parses the file. Widen the token
# regex to Unicode letters via (*UCP) (ASCII-only behavior is unchanged, as
# [:alpha:]/[:alnum:] still cover the ASCII range) so such names pass through
# verbatim. Idempotent — safe to call before train_cell_classifier().
patch_garnett_marker_lexer <- function(log) {
    tryCatch({
        # parse_input() rebuilds the lexer from the generator at every call
        # (rly::lex(Lexer)), so patching the generator in garnett's namespace
        # is enough; its environment is not locked (R6 lock_class = FALSE)
        ns <- asNamespace("garnett")
        Lexer <- get("Lexer", envir = ns)
        fields <- Lexer$public_fields
        fields[["t_NAME"]] <- "(*UCP)[[:alnum:]_+/\\-\\.|=`~\\*&<^%?@!$();:]*[[:alpha:]][[:alnum:]_+/\\-\\.|=`~\\*&<^%?@!$();]*"
        Lexer$public_fields <- fields
    }, error = function(e) {
        log$warn(paste(
            "Failed to patch garnett's marker-file lexer for Unicode cell",
            "type names:",
            conditionMessage(e)
        ))
    })
}

# patch_garnett_run_classifier — fix the upstream crash in garnett 0.2.22's
# run_classifier() when NO cell passes any gate.
#
# Upstream bug: run_classifier() builds its level table with
#   level_table <- data.frame(cell = row.names(imputed_gate_res[[1]]), ...)
# but make_predictions() returns split()-ed plain vectors, whose row.names() is
# NULL, and data.frame(cell = NULL, level1 = "Unknown") errors in R >= 4.x with
# "arguments imply differing number of rows: 0, 1". The line is unconditional,
# so this guard is what makes a degenerate classifier fail loudly as
# all-Unknown instead of killing the job.
# Fix: take cell names from the cds instead of from the prediction vectors, and
# warn when no cell was classified (degenerate classifier / ratio too strict).
patch_garnett_run_classifier <- function(log) {
    fix_run_classifier <- function() {
        src <- deparse(get("run_classifier", envir = asNamespace("garnett")))
        n_cells <- "nrow(SummarizedExperiment::colData(cds))"
        cell_names <- "row.names(SummarizedExperiment::colData(cds))"
        src <- gsub(
            "length(imputed_gate_res[[1]])", n_cells, src, fixed = TRUE
        )
        src <- gsub(
            "row.names(imputed_gate_res[[1]])", cell_names, src, fixed = TRUE
        )
        warn_at <- grep("tree_levels <- igraph::distances", src, fixed = TRUE)
        warn_line <- paste0(
            "    if (length(imputed_gate_res) == 0 || ",
            "!any(vapply(imputed_gate_res, length, integer(1)) > 0)) ",
            "warning(\"garnett run_classifier: no cell passed any gate; all ",
            "cells marked Unknown (degenerate classifier or rank_prob_ratio ",
            "too strict)\")"
        )
        if (length(warn_at) > 0) {
            src <- append(src, warn_line, after = warn_at[1] - 1)
        }
        # A silent no-op would be worse than a loud failure: if garnett's
        # internals change, the substitutions above stop matching.
        if (!any(grepl(n_cells, src, fixed = TRUE)) ||
                any(grepl("imputed_gate_res[[1]]", src, fixed = TRUE))) {
            stop(paste(
                "run_classifier substitution did not match the installed",
                "garnett::run_classifier source; the empty-gate crash fix is",
                "NOT applied"
            ))
        }
        eval(
            parse(text = paste(src, collapse = "\n")),
            envir = asNamespace("garnett")
        )
    }
    tryCatch(
        {
            monkey_patch("garnett", "run_classifier", fix_run_classifier())
            log$info("Patched garnett::run_classifier (empty-gate crash fix)")
        },
        error = function(e) {
            log$warn(paste(
                "Failed to patch garnett::run_classifier",
                "(empty-gate crash fix):", conditionMessage(e)
            ))
        }
    )
}

.run_celltypeannotation_garnett <- function(object, args, ident, ctx) {
    # `args` is the caller's list and the body below NULLs keys out of it
    args <- as.list(args)

    log <- get_logger()
    patch_garnett_make_predictions(log)
    patch_garnett_run_classifier(log)
    patch_garnett_marker_lexer(log)

    classifier_path <- args$classifier
    if (is.null(classifier_path)) { stop("`envs.garnett.classifier` is not set") }
    if (!file.exists(classifier_path)) {
        stop(paste0("Garnett classifier file does not exist: ", classifier_path))
    }

    log$info("Loading Garnett classifier ...")
    classifier <- read_obj(classifier_path)
    if (!inherits(classifier, "garnett_classifier")) {
        stop(paste0(
            "The file in `envs.garnett.classifier` does not contain a ",
            "Garnett classifier (garnett_classifier) object: ", classifier_path
        ))
    }

    # Gene ID conversion database: "none" or an AnnotationDb/OrgDb package name
    db <- args$db %||% "none"
    if (!identical(db, "none")) {
        if (!requireNamespace(db, quietly = TRUE)) {
            stop(paste0(
                "The gene ID database package `", db,
                "` from `envs.garnett.db` is not installed."
            ))
        }
        library(db, character.only = TRUE)
        db <- getExportedValue(db, db)
    }

    assay <- args$assay
    # Keys consumed here must not be forwarded to classify_cells()
    args$classifier <- NULL
    args$db <- NULL
    args$assay <- NULL

    unknown_args <- setdiff(names(args), formalArgs(garnett::classify_cells))
    if (length(unknown_args) > 0) {
        stop(paste0(
            "Unknown arguments in `envs.garnett`: ",
            paste(unknown_args, collapse = ", "),
            " (not arguments of `garnett::classify_cells()`)"
        ))
    }

    log$info("Converting Seurat object to a monocle3 cell_data_set ...")
    # as.cell_data_set is the monocle3 generic; SeuratWrappers only registers
    # the method for Seurat objects (loaded above), so call it unqualified
    cds <- if (is.null(assay)) {
        SeuratWrappers::as.cell_data_set(object)
    } else {
        SeuratWrappers::as.cell_data_set(object, assay = assay)
    }

    # classify_cells() hard-asserts a Size_Factor column but overwrites the
    # value internally with colSums / (cell_totals * median(num_genes_expressed))
    # -- the monocle3 convention (colSums / G) -- so the labels do not depend on
    # what we pass here; the returned cds does keep it though. Install monocle3
    # (median-ratio) size factors unconditionally so the annotation cds stays on
    # the same convention the trainer uses, instead of the
    # SeuratWrappers::as.cell_data_set() colSums convention, and verify the
    # conversion actually happened (a no-op monocle3 would leave the colSums
    # values in place, which is the silent all-"Unknown" trap at training time).
    cds <- monocle3::estimate_size_factors(cds)
    .sf <- as.numeric(SummarizedExperiment::colData(cds)$Size_Factor)
    .ct <- as.numeric(Matrix::colSums(SingleCellExperiment::counts(cds)))
    .g <- exp(mean(log(.ct[.ct > 0])))
    if (anyNA(.sf) || !all(is.finite(.sf))) {
        stop("Could not install finite size factors on the cell_data_set.")
    }
    if (!isTRUE(all.equal(.g, 1)) &&
        isTRUE(all.equal(.sf, .ct, check.attributes = FALSE))) {
        stop(paste(
            "Garnett expects monocle3 (median-ratio) size factors, but",
            "colData(cds)$Size_Factor still equals colSums(counts) (the",
            "SeuratWrappers::as.cell_data_set() convention). At training time",
            "this makes the model fit and the prediction matrix differ by a",
            "factor of G = exp(mean(log(colSums))), which turns every cell into",
            "'Unknown' with no error. Run monocle3::estimate_size_factors(cds)",
            "on the converted cell_data_set."
        ))
    }

    # classify_cells() internally normalizes counts(cds); an assay with only a
    # data layer (converted counts are log values) would silently give garbage
    if (is(SingleCellExperiment::counts(cds), "dgCMatrix") && any(SingleCellExperiment::counts(cds)@x %% 1 != 0)) {
        log$warn(paste(
            "The counts of the converted cell_data_set are not integers;",
            "garnett::classify_cells() expects raw counts. Results may be",
            "unreliable. Set `envs.assay` to an assay with a counts layer."
        ))
    }

    log$info("Classifying cells with Garnett ...")
    classify_args <- args
    classify_args$cds <- cds
    classify_args$classifier <- classifier
    classify_args$db <- db
    result_cds <- do_call(garnett::classify_cells, classify_args)

    labels <- SummarizedExperiment::colData(result_cds)$cell_type
    if (is.null(labels)) {
        stop("classify_cells() did not return a `cell_type` column")
    }
    # Cells with zero counts are excluded by classify_cells and get NA
    labels[is.na(labels)] <- "Unknown"

    n_unknown <- sum(labels == "Unknown")
    if (n_unknown == length(labels)) {
        log$warn(paste(
            "All cells are classified as 'Unknown' by Garnett; check",
            "`envs.garnett.db`/`envs.garnett.cds_gene_id_type` (classifier is",
            "trained on", classifier@gene_id_type, "genes)"
        ))
    } else {
        log$info(
            "Garnett classified {length(labels) - n_unknown}/{length(labels)}",
            " cells (Unknown: {n_unknown})"
        )
    }

    result <- data.frame(
        garnett_celltype = unname(labels),
        row.names = colnames(result_cds)
    )

    if (is.null(ident)) {
        list(mapping = result, type = "cell")
    } else {
        log$info("Aggregating Garnett results by cluster ...")
        mapping <- majority_vote(
            labels, as.character(object@meta.data[[ident]]),
            unknown = "Unknown"  # garnett's sentinel is capital-U
        )
        list(mapping = mapping, type = "cluster", cells = result)
    }
}

# ---- celltypist -------------------------------------------------------------

.run_celltypeannotation_celltypist <- function(object, args, ident, ctx) {
    log <- get_logger()

    if (is.null(args$model)) {
        stop("Please specify a model for celltypist (celltypist.model)")
    } else if (!file.exists(args$model)) {
        stop(paste0("Model file not found (celltypist.model)"))
    }

    require_package(
        "celltypist2",
        version = ">=1.7.1",
        python = args$python
    )

    over_clustering <- args$over_clustering %||% ident

    # Symlink the model file
    model_dir <- file.path(ctx$scratch, "data", "models")
    dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
    modelfile <- file.path(model_dir, basename(args$model))
    suppressWarnings(file.remove(modelfile))
    file.symlink(normalizePath(args$model), modelfile)

    # Determine output file
    celltypist_outfile <- file.path(ctx$scratch, "celltypist.txt")

    # Run celltypist
    biopipen_dir <- get_biopipen_dir(args$python)
    celltypist_script <- file.path(
        biopipen_dir, "scripts", "scrna", "celltypist-wrapper.py"
    )

    if (file.exists(celltypist_outfile) &&
        (file.mtime(celltypist_outfile) > file.mtime(ctx$h5ad))) {
        log$warn(
            "Using existing celltypist results: {celltypist_outfile} ..."
        )
    } else {
        command <- paste(
            paste0("CELLTYPIST_FOLDER='", ctx$scratch, "'"),
            args$python,
            celltypist_script,
            "-i", ctx$h5ad,
            "-m", args$model,
            "-o", celltypist_outfile
        )
        if (!isFALSE(over_clustering) && !is.null(over_clustering)) {
            command <- paste(command, "-c", over_clustering)
        }
        if (isTRUE(args$majority_voting)) {
            command <- paste(command, "-v")
        }
        log$info("Running celltypist:")
        print(paste0("- ", command))
        log$debug("  {command}")
        rc <- system(command)
        if (rc != 0) {
            stop(paste(
                "Failed to run celltypist.",
                "Check the job.stderr file to see the error message."
            ))
        }
    }

    # Read results
    log$info("Reading celltypist results ...")
    celltypist_out <- read.table(
        celltypist_outfile, sep = "\t", header = TRUE, row.names = 1
    )

    output_col <- ifelse(
        isTRUE(args$majority_voting),
        "majority_voting",
        "predicted_labels"
    )

    # Per-cell predictions are always returned
    annotations <- celltypist_out[, output_col, drop = FALSE]
    colnames(annotations) <- output_col

    if (is.null(over_clustering) || isFALSE(over_clustering)) {
        # Cell-level prediction only
        list(mapping = annotations, type = "cell")
    } else {
        # Cluster-level mapping by majority vote
        clusters <- as.character(
            object@meta.data[[over_clustering]][rownames(celltypist_out)]
        )
        mapping <- majority_vote(celltypist_out[[output_col]], clusters)
        list(mapping = mapping, type = "cluster", cells = annotations)
    }
}

# ---- schdeepinsight ---------------------------------------------------------

.run_celltypeannotation_schdeepinsight <- function(object, args, ident, ctx) {
    log <- get_logger()

    schdeepinsight_ref <- args$ref
    if (is.null(schdeepinsight_ref)) {
        stop("`envs.schdeepinsight.ref` is not set")
    }
    if (startsWith(schdeepinsight_ref, "file://")) {
        schdeepinsight_ref <- sub("^file://", "", schdeepinsight_ref)
    }
    if (!file.exists(schdeepinsight_ref)) {
        stop(paste0(
            "scHDeepInsight reference file does not exist: ",
            schdeepinsight_ref
        ))
    }

    require_package(
        "SCHdeepinsight", python = args$python
    )

    # Output paths
    workdir <- file.path(ctx$scratch, "workdir")
    outfile <- file.path(ctx$scratch, "schdeepinsight.txt")

    # Run wrapper script
    biopipen_dir <- get_biopipen_dir(args$python)
    wrapper <- file.path(
        biopipen_dir, "scripts", "scrna", "schdeepinsight-wrapper.py"
    )
    command <- paste(
        args$python, wrapper,
        "-i", ctx$h5ad,
        "-r", schdeepinsight_ref,
        "-o", outfile,
        "-d", workdir,
        "-b", args$batch_size %||% 128,
        "--rhome", R.home()
    )
    log$info("Running scHDeepInsight ...")
    rc <- system(command)
    if (rc != 0) {
        stop("Failed to run scHDeepInsight.")
    }

    # Read results (barcode-indexed, all columns become meta.data)
    results <- read.table(
        outfile, sep = "\t", header = TRUE, row.names = 1
    )
    if (is.null(ident)) {
        list(mapping = results, type = "cell")
    } else {
        # Aggregate per-cell results to cluster-level mapping (majority vote)
        log$info("Aggregating scHDeepInsight results by cluster...")
        mapping <- majority_vote(
            results[["predicted_detailed_type"]],
            as.character(object@meta.data[[ident]][rownames(results)])
        )
        list(mapping = mapping, type = "cluster", cells = results)
    }
}

# ---- scbert -----------------------------------------------------------------

.run_celltypeannotation_scbert <- function(object, args, ident, ctx) {
    log <- get_logger()

    # Validate inputs
    scbert_ref <- args$ref
    scbert_model <- args$model
    scbert_label_dict <- args$label_dict
    if (is.null(scbert_ref)) {
        stop("`envs.scbert.ref` is required for scBERT annotation")
    }
    if (is.null(scbert_model)) {
        stop("`envs.scbert.model` is required for scBERT annotation")
    }
    if (is.null(scbert_label_dict)) {
        stop("`envs.scbert.label_dict` is required for scBERT annotation")
    }

    if (startsWith(scbert_model, "file://")) {
        scbert_model <- sub("^file://", "", scbert_model)
    }
    if (startsWith(scbert_label_dict, "file://")) {
        scbert_label_dict <- sub("^file://", "", scbert_label_dict)
    }
    if (!file.exists(scbert_model)) {
        stop(paste0("scBERT model checkpoint does not exist: ", scbert_model))
    }
    if (!file.exists(scbert_label_dict)) {
        stop(paste0(
            "scBERT label dictionary does not exist: ", scbert_label_dict
        ))
    }

    # Output paths
    outfile <- file.path(ctx$scratch, "scbert.txt")

    # Run wrapper script
    biopipen_dir <- get_biopipen_dir(args$python)
    wrapper <- file.path(
        biopipen_dir, "scripts", "scrna", "scbert-wrapper.py"
    )
    command <- paste(
        args$python, wrapper,
        "-i", ctx$h5ad,
        "-m", scbert_model,
        "-l", scbert_label_dict,
        "-r", scbert_ref,
        "-o", outfile,
        "--bin-num", args$bin_num %||% 5,
        "--gene-num", args$gene_num %||% 16906,
        "--seed", args$seed %||% 2021
    )
    if (isTRUE(args$pos_embed)) {
        command <- paste(command, "--pos-embed")
    } else {
        command <- paste(command, "--no-pos-embed")
    }
    if (isTRUE(args$novel_type)) {
        command <- paste(
            command, "--novel-type",
            "--unassign-thres", args$unassign_thres %||% 0.5
        )
    }
    log$info("Running scBERT ...")
    rc <- system(command)
    if (rc != 0) {
        stop("Failed to run scBERT.")
    }

    # Read results (barcode-indexed)
    results <- read.table(
        outfile, sep = "\t", header = TRUE, row.names = 1
    )
    if (is.null(ident)) {
        list(mapping = results, type = "cell")
    } else {
        # Aggregate per-cell results to cluster-level mapping (majority vote)
        log$info("Aggregating scBERT results by cluster...")
        mapping <- majority_vote(
            results[["scbert_celltype"]],
            as.character(object@meta.data[[ident]][rownames(results)])
        )
        list(mapping = mapping, type = "cluster", cells = results)
    }
}

# ---- scagenttype ------------------------------------------------------------

.run_celltypeannotation_scagenttype <- function(object, args, ident, ctx) {
    log <- get_logger()

    require_package("scagenttype", python = args$python)

    # Output paths
    cluster_file <- file.path(ctx$scratch, "scagenttype.clusters.tsv")
    config_file <- file.path(ctx$scratch, "scagenttype.config.json")
    outfile <- file.path(ctx$scratch, "scagenttype.txt")

    # Cluster memberships (barcode -> cluster) for the wrapper: the h5ad carries
    # no cluster column, and scAgentType requires one for its per-cluster markers.
    # `ident` is guaranteed to be resolved for cluster-level tools (see main script).
    cluster_df <- data.frame(
        barcode = rownames(object@meta.data),
        cluster = as.character(object@meta.data[[ident]]),
        stringsAsFactors = FALSE,
        check.names = FALSE
    )
    write.table(
        cluster_df, cluster_file,
        sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE
    )

    # Config for the wrapper: `envs.scagenttype` minus the R-side and credential
    # keys; `tissue`/`species` are folded into `tissue_context` when not given
    config <- args
    config$python <- NULL
    config$assay <- NULL
    config$api_key <- NULL
    config$base_url <- NULL
    config$tissue <- NULL
    config$species <- NULL
    # Keep the agent's caches (marker DBs, LLM responses) in the job dir
    # instead of the job's working directory (default), which could be inside
    # the source tree. The wrapper clears the LLM-response cache on retry.
    if (is.null(config$cache_dir)) config$cache_dir <- ctx$scratch
    if (is.null(config$tissue_context)) {
        tctx <- c(
            if (!is.null(args$tissue)) args$tissue,
            if (!is.null(args$species)) args$species
        )
        if (length(tctx) > 0) {
            config$tissue_context <- paste(tctx, collapse = " / ")
        }
    }
    config <- config[!sapply(config, is.null)]
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
        stop("`jsonlite` R package is required for scAgentType annotation")
    }
    writeLines(
        jsonlite::toJSON(config, auto_unbox = TRUE, null = "null", na = "null"),
        config_file
    )

    # Pass credentials to the wrapper process as environment variables, since
    # scAgentType reads the keys from the environment at call time
    child_env <- character(0)
    api <- args$api %||% "openai"
    if (!is.null(args$api_key)) {
        key_env <- c(
            openai = "OPENAI_API_KEY",
            anthropic = "ANTHROPIC_API_KEY",
            google = "GOOGLE_API_KEY"
        )[[api]]
        if (is.na(key_env)) stop(paste0("Unknown api: ", api))
        child_env <- c(child_env, paste0(key_env, "=", args$api_key))
    }
    if (!is.null(args$base_url)) {
        base_env <- c(
            openai = "OPENAI_BASE_URL",
            anthropic = "ANTHROPIC_BASE_URL"
        )[[api]]
        if (is.na(base_env)) {
            stop(paste0("`base_url` is not supported for api: ", api))
        }
        child_env <- c(child_env, paste0(base_env, "=", args$base_url))
    }

    # Run the wrapper script
    biopipen_dir <- get_biopipen_dir(args$python)
    wrapper <- file.path(
        biopipen_dir, "scripts", "scrna", "scagenttype-wrapper.py"
    )
    log$info("Running scAgentType ...")
    rc <- system2(
        args$python,
        c(
            wrapper,
            "-i", ctx$h5ad,
            "-c", cluster_file,
            "-o", outfile,
            "--config", config_file
        ),
        env = child_env
    )
    if (rc != 0) {
        stop(
            "Failed to run scAgentType. ",
            "Check the job.stderr file to see the error message."
        )
    }

    # Read the per-cell labels and aggregate to a cluster-level mapping.
    # Seurat meta.data columns carry no cell names, so align by `match()` and
    # subset positionally (name-subsetting would return all NAs).
    results <- read.table(outfile, sep = "\t", header = TRUE, row.names = 1)
    idx <- match(rownames(results), rownames(object@meta.data))
    if (anyNA(idx)) {
        stop(
            sum(is.na(idx)), " barcodes from scAgentType results are not found ",
            "in the Seurat object. The barcodes were likely changed during ",
            "the h5ad conversion."
        )
    }
    log$info("Aggregating scAgentType results by cluster ...")
    mapping <- majority_vote(
        results[["scagenttype_celltype"]],
        as.character(object@meta.data[[ident]][idx])
    )
    list(mapping = mapping, type = "cluster")
}

# ---- cellassign -------------------------------------------------------------

.run_celltypeannotation_cellassign <- function(object, args, ident, ctx) {
    # `args` is the caller's list and the body below NULLs keys out of it
    args <- as.list(args)
    cellassign_db <- args$db

    python <- args$python %||% Sys.which("python")
    if (python == "") {
        stop("Python executable not found. Please specify `envs.cellassign.python`.")
    }
    # load the right Python environment with tensorflow installed
    Sys.setenv(RETICULATE_PYTHON = python)

    log <- get_logger()

    if (is.null(cellassign_db)) {
        stop("`envs.cellassign.db` is required for cellassign annotation")
    }

    # Load marker gene info
    marker_gene_info <- load_marker_table(cellassign_db)
    if (!is_marker_canonical(marker_gene_info)) {
        # Native signature formats have no tissue/cancer/species columns
        stop_on_filtering_native_db(
            args$tissue,
            args$cancer,
            args$species
        )
    }
    if (is_marker_canonical(marker_gene_info)) {
        marker_gene_info <- markers_to_named_list(
            marker_gene_info,
            tissue = args$tissue,
            cancer = args$cancer,
            species = args$species
        )
    } else if (is.data.frame(marker_gene_info)) {
        stop("CSV/TSV must have 'gene' and 'cell_type' columns.")
    } else if (!(is.list(marker_gene_info) || is.matrix(marker_gene_info))) {
        stop("Marker gene info must be a named list or binary matrix.")
    }
    # The filter envs are consumed by the marker conversion above; never
    # forward them to cellassign() which has no such arguments
    args$tissue <- NULL
    args$cancer <- NULL
    args$species <- NULL

    # Get raw counts
    assay <- args$assay %||% DefaultAssay(object)
    log$info("Extracting raw counts from assay: {assay}")
    # genes x cells matrix
    counts <- as.matrix(GetAssayData(object, assay = assay, layer = "counts"))
    library_size <- colSums(counts)
    size_factors <- library_size / median(library_size)

    # Filter to marker genes present in data
    if (is.list(marker_gene_info)) {
        all_markers <- unique(unlist(marker_gene_info))
    } else if (is.matrix(marker_gene_info)) {
        all_markers <- rownames(marker_gene_info)
    } else {
        stop("marker_gene_info must be a named list or binary matrix.")
    }
    common_genes <- intersect(all_markers, rownames(counts))
    if (length(common_genes) == 0) {
        stop("None of the marker genes are found in the expression data.")
    }
    log$info(
        "Found {length(common_genes)} / {length(all_markers)} marker genes in data"
    )

    args$s <- size_factors
    # Filter counts and marker_gene_info to common genes
    counts <- counts[common_genes, , drop = FALSE]
    if (is.list(marker_gene_info)) {
        marker_gene_info <- lapply(
            marker_gene_info,
            function(gs) intersect(gs, common_genes)
        )
        marker_gene_info <- marker_gene_info[lengths(marker_gene_info) > 0]
    } else {
        marker_gene_info <- marker_gene_info[common_genes, , drop = FALSE]
    }

    # Extract extra args for cellassign()
    extra_args <- args
    extra_args$assay <- NULL  # already handled
    extra_args$db <- NULL  # db is passed separately as cellassign_db
    extra_args$python <- NULL  # already handled above

    # Run cellassign
    log$info("Running cellassign...")
    fit <- do_call(cellassign::cellassign, c(
        list(
            exprs_obj = t(counts),
            marker_gene_info = marker_gene_info
        ),
        extra_args
    ))

    # Build cell annotations
    cell_types <- fit$cell_type
    result <- data.frame(
        cellassign_celltype = cell_types,
        row.names = names(cell_types)
    )

    if (is.null(ident)) {
        list(mapping = result, type = "cell")
    } else {
        # Aggregate per-cell results to cluster-level mapping (majority vote)
        log$info("Aggregating cellassign results by cluster...")
        mapping <- majority_vote(
            cell_types, as.character(object@meta.data[[ident]])
        )
        list(mapping = mapping, type = "cluster", cells = result)
    }
}
