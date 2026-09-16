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

# ---- ucell ------------------------------------------------------------------

.run_celltypeannotation_ucell <- function(object, args, ident, ctx) {
    # UCell (Andreatta & Carmona, 2021, doi:10.1016/j.csbj.2021.06.043):
    # rank-based signature scores with equal (unit) weights — the negative
    # markers are read from the `-` gene-name suffix (markers_to_ucell_list()).
    log <- get_logger()
    db <- args$db

    if (is.null(db)) { stop("`envs.ucell.db` is not set") }

    log$info("Loading UCell marker table ...")
    mt <- load_marker_table(db)
    if (!is_marker_canonical(mt)) {
        # Native signature formats have no tissue/cancer/species columns
        stop_on_filtering_native_db(args$tissue, args$cancer, args$species)
        stop(paste0(
            "The UCell db must be a universal marker table ",
            "(a table with `cell_type` and `gene` columns)."
        ))
    }
    mt <- apply_marker_filters(
        mt,
        tissue = args$tissue,
        cancer = args$cancer,
        species = args$species
    )
    sets <- markers_to_ucell_list(mt)

    # Defaults are the UCell defaults (`maxRank` 1000 is biopipen's own default)
    max_rank <- args$maxRank %||% 1000
    w_neg <- args$w_neg %||% 1
    name <- args$name %||% "_UCell"

    log$info("Running UCell on {length(sets)} signatures ...")
    object <- UCell::AddModuleScore_UCell(
        object,
        features = sets,
        maxRank = max_rank,
        w_neg = w_neg,
        # NULL falls back to the default assay of the object
        assay = args$assay,
        name = name
    )

    # The scores land in the metadata as `<signature><name>`; drop the suffix so
    # the argmax labels are the cell types themselves. `sort()` keeps one
    # deterministic column order across runs.
    set_names <- sort(names(sets))
    scores <- as.matrix(
        object@meta.data[, paste0(set_names, name), drop = FALSE]
    )
    colnames(scores) <- set_names
    labels <- colnames(scores)[max.col(scores, ties.method = "first")]

    result <- data.frame(
        ucell_celltype = unname(labels),
        row.names = colnames(object)
    )

    if (is.null(ident)) {
        list(mapping = result, type = "cell")
    } else {
        log$info("Aggregating UCell results by cluster...")
        mapping <- majority_vote(labels, as.character(object@meta.data[[ident]]))
        list(mapping = mapping, type = "cluster", cells = result)
    }
}

# ---- aucell -----------------------------------------------------------------

.run_celltypeannotation_aucell <- function(object, args, ident, ctx) {
    # AUCell (Van de Sande et al., 2020, doi:10.1038/s41596-020-0336-2): the AUC
    # of the signature genes in the per-cell ranking, equal-weighting by design.
    log <- get_logger()
    db <- args$db

    if (is.null(db)) { stop("`envs.aucell.db` is not set") }

    log$info("Loading AUCell marker table ...")
    mt <- load_marker_table(db)
    if (!is_marker_canonical(mt)) {
        # Native signature formats have no tissue/cancer/species columns
        stop_on_filtering_native_db(args$tissue, args$cancer, args$species)
        stop(paste0(
            "The AUCell db must be a universal marker table ",
            "(a table with `cell_type` and `gene` columns)."
        ))
    }
    mt <- apply_marker_filters(
        mt,
        tissue = args$tissue,
        cancer = args$cancer,
        species = args$species
    )
    # AUCell gene sets are positive-only (a ranked list has no direction), so
    # the negative markers are dropped by markers_to_named_list() below
    if ("direction" %in% colnames(mt) &&
        any(normalize_marker_direction(mt$direction) == "negative")) {
        log$warn(paste0(
            "AUCell has no direction support: dropping the ",
            "negative-direction markers."
        ))
    }
    sets <- markers_to_named_list(mt)

    # genes x cells, log-normalized data layer
    expr <- as.matrix(GetAssayData(object, assay = args$assay, layer = "data"))
    auc_max_rank <- args$aucMaxRank %||% ceiling(0.05 * nrow(expr))
    norm_auc <- args$normAUC %||% TRUE

    log$info("Building AUCell rankings ...")
    rankings <- AUCell::AUCell_buildRankings(
        expr, plotStats = FALSE, splitByBlocks = TRUE
    )
    log$info("Running AUCell on {length(sets)} signatures ...")
    auc <- AUCell::AUCell_calcAUC(
        sets, rankings, aucMaxRank = auc_max_rank, normAUC = norm_auc
    )
    # getAUC() is sets x cells; transpose to cells x sets for the argmax
    scores <- t(AUCell::getAUC(auc))
    labels <- colnames(scores)[max.col(scores, ties.method = "first")]

    result <- data.frame(
        aucell_celltype = unname(labels),
        row.names = colnames(object)
    )

    if (is.null(ident)) {
        list(mapping = result, type = "cell")
    } else {
        log$info("Aggregating AUCell results by cluster...")
        mapping <- majority_vote(labels, as.character(object@meta.data[[ident]]))
        list(mapping = mapping, type = "cluster", cells = result)
    }
}

# ---- gsva -------------------------------------------------------------------

.run_celltypeannotation_gsva <- function(object, args, ident, ctx) {
    # GSVA (Hänzelmann et al., 2013, doi:10.1186/1471-2105-14-7): a per-sample
    # (here: per-cell) enrichment score for each gene set, with no weights.
    log <- get_logger()
    db <- args$db

    if (is.null(db)) { stop("`envs.gsva.db` is not set") }

    log$info("Loading GSVA marker table ...")
    mt <- load_marker_table(db)
    if (!is_marker_canonical(mt)) {
        # Native signature formats have no tissue/cancer/species columns
        stop_on_filtering_native_db(args$tissue, args$cancer, args$species)
        stop(paste0(
            "The GSVA db must be a universal marker table ",
            "(a table with `cell_type` and `gene` columns)."
        ))
    }
    mt <- apply_marker_filters(
        mt,
        tissue = args$tissue,
        cancer = args$cancer,
        species = args$species
    )
    # A GSVA gene set carries no direction: its score is the enrichment of the
    # set in the cell, so a down-regulated marker cannot be expressed as a set
    if ("direction" %in% colnames(mt) &&
        any(normalize_marker_direction(mt$direction) == "negative")) {
        log$warn(paste0(
            "GSVA has no direction support: dropping the ",
            "negative-direction markers."
        ))
    }
    sets <- markers_to_named_list(mt)

    # genes x cells, log-normalized data layer (Gaussian kernel by default)
    expr <- as.matrix(GetAssayData(object, assay = args$assay, layer = "data"))

    log$info("Running GSVA on {length(sets)} signatures ...")
    scores <- GSVA::gsva(
        GSVA::gsvaParam(
            expr, sets,
            kcdf = args$kcdf %||% "Gaussian",
            minSize = args$minSize %||% 1,
            maxSize = args$maxSize %||% Inf
        ),
        verbose = FALSE
    )
    # gsva() returns sets x cells; max.col() needs the cells as rows
    labels <- rownames(scores)[max.col(t(scores), ties.method = "first")]

    result <- data.frame(
        gsva_celltype = unname(labels),
        row.names = colnames(object)
    )

    if (is.null(ident)) {
        list(mapping = result, type = "cell")
    } else {
        log$info("Aggregating GSVA results by cluster...")
        mapping <- majority_vote(labels, as.character(object@meta.data[[ident]]))
        list(mapping = mapping, type = "cluster", cells = result)
    }
}

# ---- singscore --------------------------------------------------------------

.run_celltypeannotation_singscore <- function(object, args, ident, ctx) {
    # singscore (Foroutan et al., 2018, doi:10.1186/s12859-018-2435-4): a
    # rank-based score over each signature's up and down gene sets, unweighted.
    log <- get_logger()
    db <- args$db

    if (is.null(db)) { stop("`envs.singscore.db` is not set") }

    log$info("Loading singscore marker table ...")
    mt <- load_marker_table(db)
    if (!is_marker_canonical(mt)) {
        # Native signature formats have no tissue/cancer/species columns
        stop_on_filtering_native_db(args$tissue, args$cancer, args$species)
        stop(paste0(
            "The singscore db must be a universal marker table ",
            "(a table with `cell_type` and `gene` columns)."
        ))
    }
    mt <- apply_marker_filters(
        mt,
        tissue = args$tissue,
        cancer = args$cancer,
        species = args$species
    )
    # The direction is native in singscore: no marker is dropped here
    sets <- markers_to_singscore_list(mt)

    # genes x cells, log-normalized data layer
    expr <- as.matrix(GetAssayData(object, assay = args$assay, layer = "data"))
    log$info("Ranking the genes of {ncol(expr)} cells ...")
    ranked <- singscore::rankGenes(expr)

    # simpleScore() dispatches on the class of every argument, so `downSet` can
    # only be passed when the cell type has negative markers, and `subSamples` /
    # `centerScore` only when the caller set them
    score_one <- function(set) {
        extra <- list()
        if (length(set$down) > 0) {
            extra$downSet <- set$down
        }
        if (!is.null(args$subSamples)) {
            extra$subSamples <- args$subSamples
        }
        if (!is.null(args$centerScore)) {
            extra$centerScore <- args$centerScore
        }
        do_call(
            singscore::simpleScore,
            c(list(rankData = ranked, upSet = set$up), extra)
        )$TotalScore
    }
    log$info("Running singscore on {length(sets)} signatures ...")
    scores <- do.call(cbind, lapply(sets, score_one))
    # The score columns are in the order of the ranked cells; `subSamples` (if
    # given) scores a subset of them only
    scored_cells <- colnames(ranked)
    if (!is.null(args$subSamples)) {
        scored_cells <- scored_cells[args$subSamples]
    }
    rownames(scores) <- scored_cells
    labels <- colnames(scores)[max.col(scores, ties.method = "first")]
    names(labels) <- scored_cells
    # Align back to the object, so the result always has one row per cell (NA
    # for the cells `subSamples` left out)
    labels <- labels[colnames(object)]

    result <- data.frame(
        singscore_celltype = unname(labels),
        row.names = colnames(object)
    )

    if (is.null(ident)) {
        list(mapping = result, type = "cell")
    } else {
        log$info("Aggregating singscore results by cluster...")
        mapping <- majority_vote(labels, as.character(object@meta.data[[ident]]))
        list(mapping = mapping, type = "cluster", cells = result)
    }
}

# ---- reference-consuming annotators -----------------------------------------
# These take a *labelled reference* in `envs.<tool>.db` -- read with read_obj(),
# so an RDS/qs/qs2 file holding a Seurat object, a SingleCellExperiment or a
# plain list -- instead of a marker table, and label the query by projecting it
# onto that reference.

# ---- scmap ------------------------------------------------------------------
# scmap (Kiselev et al. 2018, doi:10.1038/s41592-018-0051-x). The vignette
# (scmap/doc/scmap.Rmd) builds the reference SingleCellExperiment by hand from a
# matrix -- logcounts() + rowData$feature_symbol + the cell types in a colData
# column -- then setFeatures() -> indexCluster(cluster_col) ->
# scmapCluster(projection, index_list, threshold). For a Seurat reference,
# as.SingleCellExperiment() is the shortcut to that same shape.
.run_celltypeannotation_scmap <- function(object, args, ident, ctx) {
    require_package("scmap")
    require_package("SingleCellExperiment")
    log <- get_logger()
    db <- args$db

    if (is.null(db)) { stop("`envs.scmap.db` is not set") }

    assay <- args$assay %||% "RNA"
    threshold <- args$threshold %||% 0.5
    cluster_col <- args$cluster_col %||% "cell_type1"
    features <- args$features
    use_cell_index <- isTRUE(args$use_cell_index)

    log$info("Loading scmap reference ...")
    ref <- read_obj(db)
    if (inherits(ref, "Seurat")) {
        ref <- Seurat::as.SingleCellExperiment(ref, assay = assay)
    }
    if (!is(ref, "SingleCellExperiment")) {
        stop(paste0(
            "The scmap reference must be a SingleCellExperiment or a Seurat ",
            "object, got: ", paste(class(ref), collapse = ", ")
        ))
    }
    if (!"logcounts" %in% SummarizedExperiment::assayNames(ref)) {
        stop(paste(
            "The scmap reference needs a `logcounts` assay.",
            "Normalize the reference before saving it."
        ))
    }
    coldata <- SummarizedExperiment::colData(ref)
    if (!cluster_col %in% colnames(coldata)) {
        stop(paste0(
            "No `", cluster_col, "` column in the reference's colData. Set ",
            "`envs.scmap.cluster_col` to the column holding the cell types."
        ))
    }
    ref_labels <- as.character(coldata[[cluster_col]])

    # the vignette keeps one row per feature symbol
    ref <- ref[!duplicated(rownames(ref)), ]
    SummarizedExperiment::rowData(ref)$feature_symbol <- rownames(ref)
    features <- features %||% rownames(ref)
    log$info("Using {length(features)} features for the scmap index ...")
    ref <- scmap::setFeatures(ref, features)

    log$info("Indexing the scmap reference ...")
    index <- if (use_cell_index) {
        # scmap-cell is k-means based, so its projection is stochastic
        scmap::indexCell(ref)
    } else {
        scmap::indexCluster(ref, cluster_col = cluster_col)
    }
    # scmap keeps the index in the object's metadata
    index <- S4Vectors::metadata(index)

    log$info("Preparing the query ...")
    query <- SingleCellExperiment::SingleCellExperiment(
        assays = list(
            logcounts = as.matrix(
                GetAssayData(object, assay = assay, layer = "data")
            )
        )
    )
    SummarizedExperiment::rowData(query)$feature_symbol <- rownames(query)
    query <- scmap::setFeatures(query, features)

    log$info("Projecting the query with scmap (threshold = {threshold}) ...")
    projected <- if (use_cell_index) {
        # scmap-cell looks at the w nearest neighbours of the reference
        cells <- scmap::scmapCell(
            projection = query, index_list = list(db = index$scmap_cell_index)
        )
        scmap::scmapCell2Cluster(cells, list(ref_labels), threshold = threshold)
    } else {
        scmap::scmapCluster(
            projection = query,
            index_list = list(db = index$scmap_cluster_index),
            threshold = threshold
        )
    }
    # The labels matrix is not row-named and its rows follow the query columns,
    # which are the object's columns; the cells below the threshold come back as
    # "unassigned".
    labels <- unname(projected$scmap_cluster_labs[, 1])

    result <- data.frame(
        scmap_celltype = labels,
        row.names = colnames(object)
    )

    if (is.null(ident)) {
        list(mapping = result, type = "cell")
    } else {
        log$info("Aggregating scmap results by cluster...")
        mapping <- majority_vote(labels, as.character(object@meta.data[[ident]]))
        list(mapping = mapping, type = "cluster", cells = result)
    }
}

# ---- cheetah -----------------------------------------------------------------
# CHETAH (de Kanter et al. 2019, doi:10.1093/bioinformatics/btz500). The
# vignette (CHETAH/doc/CHETAH_introduction.Rmd) hands CHETAHclassifier() a query
# and a reference SingleCellExperiment, both holding *counts*, with the
# reference cell types in colData$celltypes (the `ref_ct` default); it returns
# the query with the calls in colData$celltype_CHETAH. In the installed 1.22.0
# `input_c`/`ref_c` are the *assay names* of the input and the reference ("the
# name of the assay of the input to use"), not cluster assignments.
.run_celltypeannotation_cheetah <- function(object, args, ident, ctx) {
    require_package("CHETAH")
    require_package("SingleCellExperiment")
    log <- get_logger()
    db <- args$db

    if (is.null(db)) { stop("`envs.cheetah.db` is not set") }

    assay <- args$assay %||% "RNA"
    # the reference's colData column holding the cell types
    label_col <- args$label %||% args$ref_ct %||% "celltypes"

    log$info("Loading CHETAH reference ...")
    ref <- read_obj(db)
    if (inherits(ref, "Seurat")) {
        ref <- Seurat::as.SingleCellExperiment(ref, assay = assay)
    }
    if (!is(ref, "SingleCellExperiment")) {
        stop(paste0(
            "The CHETAH reference must be a SingleCellExperiment or a Seurat ",
            "object, got: ", paste(class(ref), collapse = ", ")
        ))
    }
    if (!label_col %in% colnames(SummarizedExperiment::colData(ref))) {
        stop(paste0(
            "No `", label_col, "` column in the reference's colData. Set ",
            "`envs.cheetah.label` to the column holding the cell types."
        ))
    }

    log$info("Preparing the query ...")
    query <- SingleCellExperiment::SingleCellExperiment(
        assays = list(
            counts = as.matrix(
                GetAssayData(object, assay = assay, layer = "counts")
            )
        )
    )

    log$info("Running CHETAH ...")
    args$db <- NULL
    args$assay <- NULL
    args$label <- NULL
    args$input <- query
    args$ref_cells <- ref
    args$ref_ct <- label_col
    res <- do_call(CHETAH::CHETAHclassifier, args)

    # CHETAH names the labels with the query barcodes
    labels <- as.character(res$celltype_CHETAH[colnames(object)])
    result <- data.frame(
        cheetah_celltype = labels,
        row.names = colnames(object)
    )

    if (is.null(ident)) {
        list(mapping = result, type = "cell")
    } else {
        log$info("Aggregating CHETAH results by cluster...")
        mapping <- majority_vote(labels, as.character(object@meta.data[[ident]]))
        list(mapping = mapping, type = "cluster", cells = result)
    }
}

# ---- scclassify --------------------------------------------------------------
# scClassify (Lin et al. 2020, doi:10.1186/s13059-020-02078-x). The vignette
# (scClassify/doc/scClassify.Rmd) trains on a labelled reference matrix and
# predicts a query matrix: scClassify(exprsMat_train, cellTypes_train,
# exprsMat_test = list(<name> = <matrix>)) -> $testRes$<name>$<method>$predRes.
# With a pretrained model the entry point is predict_scClassify(), whose result
# is keyed by the method only ($<method>$predRes). The installed 1.18.0 accepts
# only algorithm WKNN/KNN/DWKNN (there is no "lr"), and builds its HOPACH tree
# from the reference cell types, which needs more than two of them.
.run_celltypeannotation_scclassify <- function(object, args, ident, ctx) {
    require_package("scClassify")
    log <- get_logger()
    db <- args$db

    if (is.null(db)) { stop("`envs.scclassify.db` is not set") }

    assay <- args$assay %||% "RNA"

    log$info("Loading scClassify reference ...")
    ref <- read_obj(db)

    log$info("Preparing the query ...")
    query <- as.matrix(GetAssayData(object, assay = assay, layer = "data"))

    log$info("Running scClassify ...")
    args$db <- NULL
    args$assay <- NULL
    args$algorithm <- args$algorithm %||% "WKNN"
    if (is.list(ref) && !is.null(ref$exprsMat) && !is.null(ref$cellTypes)) {
        # a labelled reference matrix: scClassify() trains and predicts
        args$exprsMat_train <- ref$exprsMat
        args$cellTypes_train <- ref$cellTypes
        args$exprsMat_test <- list(query = query)
        res <- do_call(scClassify::scClassify, args)
        labels <- res$testRes[[1]][[1]]$predRes
    } else if (inherits(ref, "scClassifyTrainModel") || is.list(ref)) {
        # a pretrained model -- train_scClassify() hands back either the S4
        # model or a plain list: predict_scClassify() has no dataset level
        args$exprsMat_test <- query
        args$trainRes <- ref
        res <- do_call(scClassify::predict_scClassify, args)
        labels <- res[[1]]$predRes
    } else {
        stop(paste(
            "The scClassify reference must be a list with `exprsMat` and",
            "`cellTypes`, or a model from scClassify::train_scClassify()."
        ))
    }

    # the predictions are named with the query barcodes
    labels <- as.character(labels[colnames(object)])
    result <- data.frame(
        scclassify_celltype = labels,
        row.names = colnames(object)
    )

    if (is.null(ident)) {
        list(mapping = result, type = "cell")
    } else {
        log$info("Aggregating scClassify results by cluster...")
        mapping <- majority_vote(labels, as.character(object@meta.data[[ident]]))
        list(mapping = mapping, type = "cluster", cells = result)
    }
}

# ---- scpred ------------------------------------------------------------------
# scPred (Alquicira-Hernandez et al. 2019, doi:10.1186/s13059-019-1802-4). The
# old scPred() entry point is gone in the installed 1.9.2: the reference is
# prepared with getFeatureSpace(pvar = <label column>) + trainModel(), and the
# query is predicted with scPredict(), which hands back the query with the
# per-cell calls in its metadata -- `prediction` ("unassigned" below the
# probability threshold) and the same calls without the rejections.
# NOTE: scPredict() -> project_query() reads the query with
# GetAssayData(new, "data"), whose 2nd positional argument is `assay` since
# SeuratObject 5, so it fails there with
# "`assay` must be one of "RNA", not "data"." -- the installed scPred needs a
# SeuratObject < 5 until it is fixed upstream.
.run_celltypeannotation_scpred <- function(object, args, ident, ctx) {
    require_package("scPred")
    log <- get_logger()
    db <- args$db

    if (is.null(db)) { stop("`envs.scpred.db` is not set") }

    pvar <- args$pvar %||% "cell_type"
    model <- args$model %||% "svmRadial"
    reduction <- args$reduction %||% "pca"
    threshold <- args$threshold %||% 0.55

    log$info("Loading scPred reference ...")
    reference <- read_obj(db)
    if (!inherits(reference, "Seurat")) {
        stop(paste0(
            "The scPred reference must be a labelled Seurat object, got: ",
            paste(class(reference), collapse = ", ")
        ))
    }
    if (!pvar %in% colnames(reference@meta.data)) {
        stop(paste0(
            "No `", pvar, "` column in the reference's metadata. Set ",
            "`envs.scpred.pvar` to the column holding the cell types."
        ))
    }

    log$info("Extracting the scPred feature space ...")
    reference <- scPred::getFeatureSpace(
        reference, pvar = pvar, reduction = reduction
    )
    log$info("Training the scPred model ({model}) ...")
    reference <- scPred::trainModel(reference, model = model)

    log$info("Predicting the query with scPredict (threshold = {threshold}) ...")
    predicted <- scPred::scPredict(
        new = object, reference = reference, threshold = threshold
    )

    meta <- predicted@meta.data
    # scPredict prefixes its output columns with `scpred_`
    pred_col <- if ("scpred_prediction" %in% colnames(meta)) {
        "scpred_prediction"
    } else {
        "prediction"
    }
    if (!pred_col %in% colnames(meta)) {
        stop(paste0(
            "No prediction column in the scPredict result: ",
            paste(colnames(meta), collapse = ", ")
        ))
    }
    log$info("Using the '{pred_col}' column of the scPredict result")
    labels <- as.character(meta[[pred_col]])

    result <- data.frame(
        scpred_celltype = labels,
        row.names = colnames(object)
    )

    if (is.null(ident)) {
        list(mapping = result, type = "cell")
    } else {
        log$info("Aggregating scPred results by cluster...")
        mapping <- majority_vote(labels, as.character(object@meta.data[[ident]]))
        list(mapping = mapping, type = "cluster", cells = result)
    }
}

# ---- azimuth -----------------------------------------------------------------
# Azimuth (Hao et al. 2021, doi:10.1016/j.cell.2021.04.048). RunAzimuth()
# resolves `ref` either as a name looked up through SeuratData -- an
# uninstalled reference is downloaded first (`pbmcref` is ~73 MB) -- or as a
# directory holding `ref.Rds` + `idx.annoy`. The reference carries the cell type
# levels and TransferData() puts a `predicted.<level>` column (plus
# `predicted.<level>.score`) into the query metadata for each of them.
# Cluster-level: the object's identity is set to `ident` for the run and each
# cluster gets the majority call of its cells.
# The installed 0.5.1 has no `dims`/`k.anchor` argument -- it reads the
# dimensionality off the reference's own annoy index.
.run_celltypeannotation_azimuth <- function(object, args, ident, ctx) {
    require_package("Azimuth")
    log <- get_logger()
    ref <- args$ref %||% args$db
    anno_col_in <- args$anno_col_in

    if (is.null(ref)) {
        stop(paste0(
            "[RunCellTypeAnnotation] Tool 'azimuth' needs `envs.azimuth.ref`, ",
            "a reference name such as \"pbmcref\" or a directory holding ",
            "ref.Rds + idx.annoy"
        ))
    }
    for (nm in c("dims", "k.anchor")) {
        if (!is.null(args[[nm]])) {
            log$warn(
                "Ignoring `", nm, "`: Azimuth ", packageVersion("Azimuth"),
                " reads it off the reference instead of taking it as an argument"
            )
        }
    }

    original_ident <- Idents(object)
    log$info("Setting the object's identity to '{ident}' for Azimuth ...")
    Idents(object) <- ident

    log$info("Running Azimuth with reference '{ref}' ...")
    args$ref <- NULL
    args$db <- NULL
    args$anno_col_in <- NULL
    args$dims <- NULL
    args$k.anchor <- NULL
    args$query <- object
    args$reference <- ref
    # Azimuth's internals look `Key<-` up on the search path (it is a SeuratObject
    # replacement function Azimuth does not import itself), so a `::`-only call
    # fails with `could not find function "Key<-"`. Attaching the namespace is the
    # documented way to provide it without a `library()` call in package code.
    suppressMessages(attachNamespace("SeuratObject"))
    object <- do_call(Azimuth::RunAzimuth, args)
    # our own copy only; the caller's object is untouched
    Idents(object) <- original_ident

    meta <- object@meta.data
    if (is.null(anno_col_in)) {
        # one `predicted.<level>` column per reference annotation level, the
        # coarsest one first
        cols <- grep("^predicted\\.", colnames(meta), value = TRUE)
        cols <- cols[!grepl("\\.score$", cols)]
        if (length(cols) == 0) {
            stop(paste0(
                "Azimuth returned no `predicted.*` column: ",
                paste(colnames(meta), collapse = ", ")
            ))
        }
        anno_col_in <- grep("^predicted\\.celltype\\.l1$", cols, value = TRUE)
        anno_col_in <- if (length(anno_col_in) > 0) anno_col_in[1] else cols[1]
        log$info("Using the Azimuth annotation column '{anno_col_in}'")
    }
    if (!anno_col_in %in% colnames(meta)) {
        stop(paste0(
            "No `", anno_col_in, "` column in the Azimuth result: ",
            paste(colnames(meta), collapse = ", ")
        ))
    }
    labels <- as.character(meta[[anno_col_in]])

    log$info("Aggregating Azimuth results by cluster...")
    mapping <- majority_vote(labels, as.character(meta[[ident]]))
    result <- data.frame(
        azimuth_celltype = labels,
        row.names = colnames(object)
    )

    list(mapping = mapping, type = "cluster", cells = result)
}

# ---- mapquery ----------------------------------------------------------------
# Seurat's own reference mapping (Hao et al. 2021,
# doi:10.1016/j.cell.2021.04.048): FindTransferAnchors() -> MapQuery().
# RunSeuratMap2Ref() is the package's wrapper around exactly that pipeline (it is
# what the SeuratMap2Ref proc calls), so this runner is argument plumbing plus
# label extraction. The reference goes in `envs.mapquery.db`, the reference's
# cell-type column in `envs.mapquery.use`. MapQuery()/TransferData() predict per
# cell (`predicted.<use>`), so like the other cell-level tools this returns the
# per-cell labels, and reduces them to one label per cluster by majority vote
# when `ident` is given.
# The reference must carry a reduction (used as `reference.reduction`) and a
# model reduction (`reduction.model`, e.g. a UMAP built with `return.model =
# TRUE`); RunSeuratMap2Ref() defaults them to the reference's first reduction and
# "umap", and errors listing the alternatives when they are missing.
.run_celltypeannotation_mapquery <- function(object, args, ident, ctx) {
    log <- get_logger()
    db <- args$db
    use <- args$use

    if (is.null(db)) { stop("`envs.mapquery.db` is not set") }
    if (is.null(use)) {
        stop(paste0(
            "`envs.mapquery.use` is not set. It is the reference's metadata ",
            "column holding the cell types to map the query onto"
        ))
    }

    # The level follows the engine's `ident` (the pipeline's `envs.ident`): with
    # it, each cluster gets the majority call of its cells; without it, the run
    # stays cell-level. `args$ident` is not read for this -- the pipeline's
    # `envs.mapquery.ident` is the SeuratMap2Ref-era *output* column name, which
    # is now `envs.mapquery.ident_name`: RunSeuratMap2Ref() writes the labels into
    # the column named by its `ident` argument (it drops the `predicted.<use>`
    # column again), so that argument gets `ident_name` and the query's cluster
    # column is never the one overwritten.
    ident_name <- args$ident_name %||% "predicted.id"
    clusters <- NULL
    if (!is.null(ident)) {
        # Read the clusters before the mapping call: it overwrites the column it
        # writes to, which is the cluster column when the two names coincide.
        clusters <- as.character(object@meta.data[[ident]])
        # length 0, not NULL: a missing column gives NULL and as.character(NULL)
        # is character(0)
        if (length(clusters) == 0) {
            stop(paste0(
                "No `", ident, "` column in the query's metadata: ",
                paste(colnames(object@meta.data), collapse = ", ")
            ))
        }
    }

    log$info("Mapping the query to the reference with RunSeuratMap2Ref ...")
    object <- RunSeuratMap2Ref(
        object,
        ref = db,
        use = use,
        ident = ident_name,
        refnorm = args$refnorm %||% "auto",
        skip_if_normalized = args$skip_if_normalized %||% TRUE,
        ncores = args$ncores %||% 1,
        MapQueryArgs = args$map_query %||% list(),
        FindTransferAnchorsArgs = args$find_transfer_anchors %||% list(),
        SCTransformArgs = args$sctransform %||% list(),
        NormalizeDataArgs = args$normalize_data %||% list(),
        log = log,
        cache = ctx$cache
    )

    labels <- as.character(object@meta.data[[ident_name]])
    result <- data.frame(
        mapquery_celltype = labels,
        row.names = colnames(object)
    )

    if (is.null(ident)) {
        list(mapping = result, type = "cell")
    } else {
        log$info("Aggregating mapquery results by cluster...")
        mapping <- majority_vote(labels, clusters)
        log$info(
            "Mapped {length(mapping)} clusters to {length(unique(labels))} cell types"
        )
        list(mapping = mapping, type = "cluster", cells = result)
    }
}

# ---- LLM annotators ---------------------------------------------------------
# Both ask an LLM to name each cluster from its marker genes, so both need
# credentials (or a local OpenAI-compatible endpoint) to return labels. Neither
# has a `db`; the markers come from RunSeuratDEAnalysis().

# ---- mllmcelltype -----------------------------------------------------------
# mLLMCelltype (CRAN; the maintained fork of the archived LLMCellType). One call
# per run: the top `top_gene_count` markers per cluster by avg_log2FC go into a
# prompt, the model answers with one cell type per line, in cluster order. With
# `api_key = NA` -- or with no key at all -- the function returns that prompt
# instead of calling the API, and the runner stops rather than passing the
# prompt off as an annotation.
.run_celltypeannotation_mllmcelltype <- function(object, args, ident, ctx) {
    require_package("mLLMCelltype")
    log <- get_logger()

    tissue <- args$tissue
    if (is.null(tissue) || !nzchar(tissue)) {
        stop("`envs.mllmcelltype.tissue` is required (e.g. 'human PBMC')")
    }
    # the tool reads no environment variables itself, so an OpenAI-compatible
    # endpoint is pointed at with OPENAI_MODEL/OPENAI_BASE_URL here
    env_or <- function(name) {
        value <- Sys.getenv(name)
        if (nzchar(value)) value
    }
    model <- args$model %||% env_or("OPENAI_MODEL") %||% "gpt-5.5"
    # the model decides which provider is called, and so which key it needs
    key_env <- if (grepl("^(anthropic|claude)", tolower(model))) {
        "ANTHROPIC_API_KEY"
    } else {
        "OPENAI_API_KEY"
    }
    api_key <- args$api_key %||% Sys.getenv(key_env)
    if (!nzchar(api_key)) {
        log$info("No {key_env} set; mLLMCelltype can only build the prompt")
        api_key <- NA
    }
    base_urls <- args$base_urls
    if (is.null(base_urls)) {
        # OPENAI_BASE_URL follows the SDK convention -- a host the client
        # appends the path to -- while mLLMCelltype takes `base_urls` as the
        # request URL in full, so a host-only base is completed here
        base_urls <- env_or("OPENAI_BASE_URL")
        if (!is.null(base_urls) && !grepl("/chat/completions/*$", base_urls)) {
            base_urls <- paste0(
                sub("/+$", "", base_urls),
                if (grepl("/v[0-9]+$", base_urls)) {
                    "/chat/completions"
                } else {
                    "/v1/chat/completions"
                }
            )
        }
    }
    # the host only: the endpoint is not a secret, the key is
    endpoint <- if (is.null(base_urls)) {
        "the provider's default endpoint"
    } else {
        sub("/.*$", "", sub("^[^:/]+://", "", base_urls))
    }

    log$info("Find the markers for {ident} ...")
    markers <- RunSeuratDEAnalysis(object, group_by = ident, log = log)
    # the tool wants the cluster column to be named `cluster`
    names(markers)[names(markers) == ident] <- "cluster"

    call_args <- args[
        # NOTE: mLLMCelltype's own logger writes <cwd>/logs/mllm_*.log on every
        # call (including the prompt-only path); the repo ignores `logs/`.
        intersect(names(args), formalArgs(mLLMCelltype::annotate_cell_types))
    ]
    call_args$input <- markers
    call_args$tissue_name <- tissue
    call_args$model <- model
    call_args$api_key <- api_key
    call_args$base_urls <- base_urls

    log$info("Running mLLMCelltype with model '{model}' via {endpoint} ...")
    res <- do_call(mLLMCelltype::annotate_cell_types, call_args)

    if (is.character(res) && length(res) == 1 && !is.na(res) &&
        startsWith(res, "You are ")) {
        stop(paste0(
            "mLLMCelltype returned a prompt, not an annotation: set ",
            "`api_key`/", key_env, "` (or pass `api_key = NA` on purpose ",
            "to inspect the prompt)"
        ))
    }

    # the API path answers one cell type per line, in the order the clusters
    # were shown in the prompt; the reasoning path is already named by cluster
    labels <- if (isTRUE(call_args$return_reasoning)) {
        vapply(res, function(x) x$cell_type, character(1))
    } else {
        as.character(res)
    }
    clusters <- names(res) %||% unique(trimws(as.character(markers$cluster)))
    if (length(labels) != length(clusters)) {
        stop(paste0(
            "mLLMCelltype returned ", length(labels), " label(s) for ",
            length(clusters), " cluster(s): ", paste(labels, collapse = ", ")
        ))
    }
    mapping <- stats::setNames(as.list(labels), clusters)

    log$info("Annotated {length(mapping)} cluster(s)")
    list(mapping = mapping, type = "cluster")
}

# ---- lict -------------------------------------------------------------------
# LICT (GitHub only) queries every provider at once and combines the answers. It
# reads the credentials from the *process environment* rather than from its
# arguments -- `openai.api_key`/`openai_api_key`, `Gemini_api_key`,
# `ANTHROPIC_API_KEY`, `ERNIE_api_key` + `ERNIE_secret_key`, `Llama3_api_key` +
# `Llama3_secret_key` -- skips every provider without one (printing an error per
# provider to stdout) and returns the string "error" when none was set.
#
# `args$keys` is a convenience for setting those variables with Sys.setenv()
# before the call. It is a *process-wide, permanent* mutation: the variables stay
# set for the rest of the R session and are inherited by child processes. Only
# the variable names are logged, never the values. When `keys` is NULL the
# environment is used as-is, so credentials already set elsewhere keep working.
#
#' @noRd
.run_celltypeannotation_lict <- function(object, args, ident, ctx) {
    require_package("LICT")
    log <- get_logger()

    species <- args$species %||% "Human"
    topgenenumber <- args$topgenenumber %||% 30
    validate <- args$validate %||% TRUE
    percent <- args$percent %||% 0.5

    if (length(args$keys) > 0) {
        do_call(Sys.setenv, args$keys)
        log$info(
            "Set the LICT provider key environment variable(s): ",
            "{paste(names(args$keys), collapse = ', ')}"
        )
    }

    log$info("Find the markers for {ident} ...")
    markers <- RunSeuratDEAnalysis(object, group_by = ident, log = log)
    # LICT wants the cluster column to be named `cluster`
    names(markers)[names(markers) == ident] <- "cluster"

    log$info("Running LICT with species '{species}' ...")
    res <- LICT::LLMCellType(
        markers,
        topgenenumber = topgenenumber,
        species = species,
        tissuename = args$tissue
    )
    if (!is.list(res)) {
        # every provider without a key is skipped, and with no provider left
        # LLMCellType() returns the string "error"
        stop(paste0(
            "LICT returned no labels (", deparse(res), "): no provider API key ",
            "was found. Set one of `openai_api_key` (or `openai.api_key`), ",
            "`Gemini_api_key`, `ANTHROPIC_API_KEY`, `ERNIE_api_key` + ",
            "`ERNIE_secret_key`, `Llama3_api_key` + `Llama3_secret_key` in the ",
            "environment, or pass them through `envs.lict.keys`"
        ))
    }

    provider <- args$provider
    if (is.null(provider)) {
        provider <- names(res)[1]
        if (length(res) > 1) {
            log$warn(
                "{length(res)} LICT providers answered ",
                "({paste(names(res), collapse = ', ')}); using '{provider}'. ",
                "Set `envs.lict.provider` to pick another one"
            )
        }
    }
    if (!provider %in% names(res)) {
        stop(paste0(
            "LICT did not answer with provider '", provider, "', got: ",
            paste(names(res), collapse = ", ")
        ))
    }
    answered <- res[[provider]]

    # the provider numbers the clusters from 0, in the order the markers were
    # grouped in (level order for a factor, sorted otherwise)
    clusters <- if (is.factor(markers$cluster)) {
        levels(droplevels(markers$cluster))
    } else {
        sort(unique(as.character(markers$cluster)))
    }
    to_mapping <- function(answered, source) {
        if (!is.data.frame(answered) ||
            !all(c("clusters", "cell_type") %in% names(answered))) {
            stop(paste0(
                "Unexpected result from the LICT ", source, ": ",
                paste(names(answered), collapse = ", ")
            ))
        }
        idx <- as.integer(answered$clusters) + 1
        if (anyNA(idx) || any(idx < 1) || any(idx > length(clusters))) {
            stop(paste0(
                "LICT numbered its clusters outside the marker table: ",
                paste(answered$clusters, collapse = ", "), " for ",
                length(clusters), " cluster(s)"
            ))
        }
        stats::setNames(as.list(as.character(answered$cell_type)), clusters[idx])
    }
    mapping <- to_mapping(answered, paste0("provider '", provider, "'"))

    # The second stage re-checks the provider's markers against the expression
    # data (`Validate()`), turns that answer into LICT's data frame
    # (`Validate_Result_to_Df()`) and asks the model for a second opinion on it
    # plus each cluster's 11th-20th DE genes (`Feedback_Info()`, LICT's
    # "talk-to-machine"). It is best-effort: it costs extra LLM calls, needs
    # reticulate + the python `openai` package, and a failure here still leaves
    # the first-stage labels usable.
    more <- NULL
    if (isTRUE(validate)) {
        log$info("Validating the LICT labels ...")
        # Validate() subsets the object by its identities, in the same order the
        # markers were grouped in, so the identity has to be `ident`
        Idents(object) <- ident
        more <- tryCatch({
            checked <- LICT::Validate(res[provider], object, percent, species)
            # `Validate()` hands back a list of per-provider tables with the
            # marker columns it re-checked; `Feedback_Info()` takes the data
            # frame `Validate_Result_to_Df()` derives from it:
            # `clusters`, `cell_type`, `row`, `<provider>_positive_marker` and
            # `<provider>_negative_marker` per provider, then `reliable` and
            # `unreliable`.
            #
            # That function binds the five provider tables by name and slices
            # the bound table by position (`df[, 26:27]`, and `[, 29:31]` in
            # `Feedback_Info()`), so it only accepts all five providers. A run
            # with fewer keys -- a single endpoint, as in this repo's tests --
            # repeats the provider that answered into the empty slots, renamed
            # after the slot: the repeated columns carry that provider's own
            # marker check, and `Feedback_Info()` unions the marker genes and
            # ORs the flags over them.
            providers <- c("ERNIE", "Gemini", "GPT", "Llama", "Claude")
            validated <- LICT::Validate_Result_to_Df(stats::setNames(
                lapply(providers, function(name) {
                    slot <- checked[[provider]]
                    names(slot) <- sub(
                        paste0("^", provider, "_"), paste0(name, "_"),
                        names(slot)
                    )
                    slot
                }),
                providers
            ))
            list(
                validate = validated,
                refined = tryCatch(
                    # `Feedback_Info()` asks every provider (ERNIE first) no
                    # matter which keys are set, so a partial provider set fails
                    # in there before it reaches the one that answered
                    to_mapping(
                        LICT::Feedback_Info(validated, 11, 20, markers)[[provider]],
                        "talk-to-machine refinement"
                    ),
                    error = function(e) {
                        log$warn(
                            "The LICT talk-to-machine refinement did not run: ",
                            "{conditionMessage(e)}"
                        )
                        NULL
                    }
                )
            )
        }, error = function(e) {
            log$warn("The LICT validate stage failed: {conditionMessage(e)}")
            NULL
        })
    }

    if (!is.null(more$refined)) {
        # the refinement relabels every cluster from the markers it re-checked
        mapping <- more$refined
        log$info(
            "Refined {length(mapping)} cluster(s) with the talk-to-machine stage"
        )
    }

    log$info("Annotated {length(mapping)} cluster(s) with '{provider}'")
    list(mapping = mapping, type = "cluster", more = more)
}

# ---- python-based marker tools (scsa, maca, scmapnet) ------------------------

# The marker table these three wrappers take as `-m/--marker`: a headerless
# two-column TSV of the cell type and the gene, written into the scratch dir.
.cta_py_marker_file <- function(db, scratch, tool) {
    if (is.null(db)) {
        stop(paste0("`envs.", tool, ".db` is not set"))
    }
    df <- load_marker_table(db)
    if (!is.data.frame(df) || !is_marker_canonical(df)) {
        stop(paste0(
            "The `", tool, "` marker table must be a universal marker table ",
            "(with `cell_type` and `gene` columns): ", db
        ))
    }
    file <- file.path(scratch, "markers.tsv")
    write.table(
        markers_to_scsa_df(df), file,
        sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE
    )
    file
}

# SCSA labels whole clusters (its `-i` is a per-cluster marker table that the
# wrapper computes itself), and the wrapper maps the cluster labels back onto
# the cells. `scsa_dir` is a clone of the SCSA repo: SCSA is not on
# CRAN/Bioconductor/PyPI, and the clone carries the reference database.
.run_celltypeannotation_scsa <- function(object, args, ident, ctx) {
    log <- get_logger()

    scsa_dir <- args$scsa_dir
    if (is.null(scsa_dir)) {
        stop(paste0(
            "`envs.scsa.scsa_dir` is not set. SCSA is not on ",
            "CRAN/Bioconductor/PyPI: clone ",
            "https://github.com/bioinfo-ibms-pumc/SCSA and point ",
            "`envs.scsa.scsa_dir` at the clone (it holds `SCSA.py` and ",
            "`whole.db`)."
        ))
    }
    markers <- .cta_py_marker_file(args$db, ctx$scratch, "scsa")

    outfile <- file.path(ctx$scratch, "scsa.txt")
    biopipen_dir <- get_biopipen_dir(args$python)
    wrapper <- file.path(biopipen_dir, "scripts", "scrna", "scsa-wrapper.py")
    command <- c(
        args$python, wrapper,
        "-i", ctx$h5ad,
        "-o", outfile,
        "-m", markers,
        "--scsa-dir", scsa_dir,
        "--ident", ident
    )
    if (isFALSE(args$use_refdb)) {
        command <- c(command, "--norefdb")
    }
    if (!is.null(args$species)) {
        command <- c(command, "-g", args$species)
    }
    if (!is.null(args$tissue)) {
        command <- c(command, "-k", args$tissue)
    }

    log$info("Running SCSA ...")
    # The wrapper names what is missing (a clone without SCSA.py, no marker
    # passing SCSA's own thresholds), so surface its output on failure
    run_command(command, stdout = TRUE, stderr = TRUE)

    # (barcode, scsa_celltype), one row per cell
    results <- read.table(outfile, sep = "\t", header = TRUE, row.names = 1)

    cells_df <- data.frame(
        scsa_celltype = unname(results[["scsa_celltype"]]),
        row.names = rownames(results)
    )
    # SCSA labels the clusters, so the cells of a cluster share a label unless
    # the wrapper could not map a cell back; vote instead of trusting the
    # first cell, and say so when that happens
    labels <- results[["scsa_celltype"]]
    names(labels) <- rownames(results)
    # meta.data columns are unnamed, so the clusters have to be taken by row
    # (a `[[ident]][<barcodes>]` lookup would be all NA)
    clusters <- as.character(object@meta.data[names(labels), ident])

    log$info("Aggregating SCSA results by cluster ...")
    mapping <- majority_vote(labels, clusters)
    disagreeing <- names(Filter(
        function(cell_types) length(unique(cell_types)) > 1,
        split(as.character(labels), clusters)
    ))
    if (length(disagreeing) > 0) {
        log$warn(paste0(
            "The cells of cluster(s) ",
            paste(disagreeing, collapse = ", "),
            " have different SCSA labels; using the majority vote."
        ))
    }

    list(mapping = mapping, type = "cluster", cells = cells_df)
}

# MACA must be installed in the python the process runs with; the wrapper
# names what to check when the import fails or the marker table shares too few
# genes with the object.
.run_celltypeannotation_maca <- function(object, args, ident, ctx) {
    log <- get_logger()

    markers <- .cta_py_marker_file(args$db, ctx$scratch, "maca")

    outfile <- file.path(ctx$scratch, "maca.txt")
    biopipen_dir <- get_biopipen_dir(args$python)
    wrapper <- file.path(biopipen_dir, "scripts", "scrna", "maca-wrapper.py")
    command <- c(
        args$python, wrapper,
        "-i", ctx$h5ad,
        "-o", outfile,
        "-m", markers
    )
    if (!is.null(args$n_pcs)) {
        command <- c(command, "--n-pcs", args$n_pcs)
    }
    if (!is.null(args$res)) {
        command <- c(
            command, "--res", paste(unlist(args$res), collapse = ",")
        )
    }
    if (!is.null(args$n_neis)) {
        command <- c(
            command, "--n-neis", paste(unlist(args$n_neis), collapse = ",")
        )
    }
    if (!is.null(args$freq)) {
        command <- c(command, "--freq", args$freq)
    }
    if (isTRUE(args$use_weight)) {
        command <- c(command, "--use-weight")
    }

    log$info("Running MACA ...")
    # A missing MACA, or a marker table it cannot score, is not an R error to
    # hide: the wrapper prints what to check, so surface that
    run_command(command, stdout = TRUE, stderr = TRUE)

    results <- read.table(outfile, sep = "\t", header = TRUE, row.names = 1)
    if (is.null(ident)) {
        list(mapping = results, type = "cell")
    } else {
        log$info("Aggregating MACA results by cluster...")
        mapping <- majority_vote(
            results[["maca_celltype"]],
            # meta.data columns are unnamed, so the clusters have to be taken
            # by row (a `[[ident]][<barcodes>]` lookup would be all NA)
            as.character(object@meta.data[rownames(results), ident])
        )
        list(mapping = mapping, type = "cluster", cells = results)
    }
}

# scMapNet's treemap images and its checkpoint are not distributable: the
# clone is passed in, and the weights are a manual download under CC BY-NC 4.0
# (non-commercial).
.run_celltypeannotation_scmapnet <- function(object, args, ident, ctx) {
    log <- get_logger()

    if (is.null(args$scmapnet_dir)) {
        stop(paste0(
            "`envs.scmapnet.scmapnet_dir` is not set. scMapNet is not a ",
            "package: clone https://github.com/Yuz7/scMapNet and point ",
            "`envs.scmapnet.scmapnet_dir` at the clone (it holds ",
            "`main_finetune.py` and `generate_image_script.sh`)."
        ))
    }
    if (is.null(args$weights)) {
        stop(paste0(
            "`envs.scmapnet.weights` is not set. The pre-trained weights are ",
            "a manual download (they are not part of the repo, see ",
            "https://github.com/Yuz7/scMapNet) and are licensed CC BY-NC 4.0 ",
            "(non-commercial); point `envs.scmapnet.weights` at a checkpoint ",
            "fine-tuned on the cell types of the marker table."
        ))
    }
    markers <- .cta_py_marker_file(args$db, ctx$scratch, "scmapnet")

    outfile <- file.path(ctx$scratch, "scmapnet.txt")
    biopipen_dir <- get_biopipen_dir(args$python)
    wrapper <- file.path(biopipen_dir, "scripts", "scrna", "scmapnet-wrapper.py")
    command <- c(
        args$python, wrapper,
        "-i", ctx$h5ad,
        "-o", outfile,
        "-m", markers,
        "--scmapnet-dir", args$scmapnet_dir,
        "--weights", args$weights
    )
    if (!is.null(args$organ)) {
        command <- c(command, "--organ", args$organ)
    }

    log$info("Running scMapNet ...")
    run_command(command, stdout = TRUE, stderr = TRUE)

    results <- read.table(outfile, sep = "\t", header = TRUE, row.names = 1)
    if (is.null(ident)) {
        list(mapping = results, type = "cell")
    } else {
        log$info("Aggregating scMapNet results by cluster...")
        mapping <- majority_vote(
            results[["scmapnet_celltype"]],
            as.character(object@meta.data[rownames(results), ident])
        )
        list(mapping = mapping, type = "cluster", cells = results)
    }
}
