#' AddSeuratCommand
#' Expand a single numer into 1:number
#'
#' This is useful to expand args$dims = 10 to args$dims = 1:10
#'
#' @param x A numeric vector
#' @return A numeric vector
#' @keywords internal
.expand_number <- function(x) {
    if (length(x) == 1 && x > 0) {
        return(1:x)
    }
    x
}

#' Recode the clusters, from 0, 1, 2, ... to x1, x2, x3, ...
#' @param clusters A numeric vector of clusters
#' @return A character vector of recoded clusters
#' @keywords internal
.recode_clusters <- function(clusters, prefix = "c") {
    recode <- function(x) paste0(prefix, as.integer(as.character(x)) + 1)
    clusters <- factor(recode(clusters), levels = recode(levels(clusters)))
    clusters
}

#' Expand the resolution for `FindClusters`
#'
#' So that we can have 0.1:0.4:0.1 to be expanded to
#' 0.1, 0.2, 0.3, 0.4
#'
#' @param resolution A numeric vector or a character vector
#' @return A numeric vector with the expanded resolution
#' @keywords internal
.expand_findclusters_resolution <- function(resolution) {
    expanded_res <- c()
    for (res in resolution) {
        if (is.numeric(res)) {
            expanded_res <- c(expanded_res, res)
        } else {
            # is.character
            parts <- trimws(unlist(strsplit(res, ",")))
            for (part in parts) {
                if (grepl(":", part)) {
                    ps <- trimws(unlist(strsplit(part, ":")))
                    if (length(ps) == 2) {
                        ps <- c(ps, 0.1)
                    }
                    if (length(ps) != 3) {
                        stop(
                            "Invalid resolution format: {part}. Expected 2 or 3 parts separated by ':' for a range."
                        )
                    }
                    ps <- as.numeric(ps)
                    expanded_res <- c(
                        expanded_res,
                        seq(ps[1], ps[2], by = ps[3])
                    )
                } else {
                    expanded_res <- c(expanded_res, as.numeric(part))
                }
            }
        }
    }
    # keep the last resolution at last
    rev(unique(rev(round(expanded_res, 2))))
}

#' Rename features in a Seurat object
#' @param obj A Seurat object
#' @param features_map A named character vector, where the names are the original feature names and the values are the new names
#' @return A Seurat object with renamed features
#' @keywords internal
.rename_seurat_features <- function(obj, features_map) {
    assay_name <- DefaultAssay(obj)
    assay_obj <- obj[[assay_name]]
    cur_names <- rownames(assay_obj)
    mask <- cur_names %in% names(features_map)
    if (!any(mask)) {
        return(obj)
    }
    new_names <- cur_names
    new_names[mask] <- features_map[cur_names[mask]]
    if (inherits(assay_obj, "Assay5")) {
        for (lyr in SeuratObject::Layers(assay_obj)) {
            ldata <- SeuratObject::LayerData(obj, layer = lyr)
            rownames(ldata) <- new_names
            SeuratObject::LayerData(obj, layer = lyr) <- ldata
        }
    } else {
        for (slt in c("counts", "data")) {
            sdata <- methods::slot(assay_obj, slt)
            if (!is.null(sdata) && prod(dim(sdata)) > 0) {
                rownames(sdata) <- new_names
                methods::slot(assay_obj, slt) <- sdata
            }
        }
        if (
            !is.null(assay_obj@meta.features) &&
                nrow(assay_obj@meta.features) > 0
        ) {
            rownames(assay_obj@meta.features) <- new_names
        }
        obj[[assay_name]] <- assay_obj
    }
    obj
}

#' Parse features argument into a named character vector
#'
#' @param features A named vector, list, or path to a TAB-delimited file
#' @return A named character vector (old_name = new_name), or NULL
#' @keywords internal
.parse_features_map <- function(features) {
    if (is.null(features)) {
        return(NULL)
    }
    if (
        is.character(features) && length(features) == 1 && file.exists(features)
    ) {
        feat_lines <- readLines(features)
        feat_lines <- feat_lines[!grepl("^#", feat_lines) & nzchar(feat_lines)]
        feat_df <- utils::read.table(
            text = paste(feat_lines, collapse = "\n"),
            sep = "\t",
            header = FALSE,
            quote = "",
            colClasses = "character",
            stringsAsFactors = FALSE
        )
        return(stats::setNames(feat_df[[2]], feat_df[[1]]))
    }
    if (is.list(features)) {
        features <- unlist(features)
    }
    stopifnot(
        "'features' must be a named vector/list or a file path to a TAB-delimited file" = is.character(
            features
        ) &&
            !is.null(names(features))
    )
    features
}


#' Load expression data from a sample path
#'
#' Handles ParseBio, HIVE, 10X directory, loom, and h5 formats.
#'
#' @param path Path to the sample data
#' @param sam Sample name
#' @param tmpdir Temporary directory for symlink workaround
#' @param LoadLoomArgs Arguments for SeuratDisk::LoadLoom
#' @param log Logger
#' @return A list with `exprs` (matrix or Seurat object) and `cell_meta` (data.frame or NULL)
#' @keywords internal
.load_expression_data <- function(path, sam, tmpdir, LoadLoomArgs, log) {
    if (!dir.exists(path)) {
        if (endsWith(path, ".loom")) {
            LoadLoomArgs$file <- path
            return(list(
                exprs = do_call(SeuratDisk::LoadLoom, LoadLoomArgs),
                cell_meta = NULL
            ))
        }
        if (file.exists(path)) {
            return(list(exprs = Read10X_h5(path), cell_meta = NULL))
        }
        stop("[LoadSeuratSample] {sam}: Path not found: {path}")
    }

    # Directory-based formats
    if (
        file.exists(file.path(path, "all_genes.csv")) &&
            file.exists(file.path(path, "cell_metadata.csv")) &&
            file.exists(file.path(path, "count_matrix.mtx"))
    ) {
        exprs <- ReadParseBio(path)
        exprs <- exprs[nchar(rownames(exprs)) > 0, , drop = FALSE]
        return(list(
            exprs = exprs,
            cell_meta = utils::read.csv(
                file.path(path, "cell_metadata.csv"),
                row.names = 1
            )
        ))
    }

    if (
        length(Sys.glob(file.path(path, "*TCM.tsv.gz"))) > 0 &&
            length(Sys.glob(file.path(path, "*ReadsQC.tsv"))) > 0
    ) {
        cmfile <- Sys.glob(file.path(path, "*TCM.tsv.gz"))
        if (length(cmfile) > 1) {
            log$warn(
                "  Multiple TCM files found, using the first one: {cmfile[1]} ..."
            )
            cmfile <- cmfile[1]
        }
        readsfile <- Sys.glob(file.path(path, "*ReadsQC.tsv"))
        if (length(readsfile) > 1) {
            log$warn(
                "  Multiple ReadsQC files found, using the first one: {readsfile[1]} ..."
            )
            readsfile <- readsfile[1]
        }
        exprs <- utils::read.table(
            cmfile,
            header = 1,
            row.names = 1,
            check.names = FALSE,
            sep = "\t",
            quote = "",
            stringsAsFactors = FALSE
        )
        cells <- colnames(exprs)
        data_reads <- utils::read.table(readsfile, header = 1, row.names = 1)
        reads <- data_reads[rownames(data_reads) %in% cells, , drop = FALSE]
        logtotreads <- log10(as.matrix(reads)[, 1])
        readAll <- data_reads[, 1]
        readMap <- reads[, 2]
        readExon <- reads[, 3]
        cell_meta <- data.frame(
            ExonReads = readExon,
            Log.TotReads = logtotreads,
            ExonvMapped = readExon / readMap,
            ExonvTotal = readExon / readAll,
            reads.mapped = readMap,
            reads.Total = readAll,
            row.names = cells
        )
        return(list(exprs = exprs, cell_meta = cell_meta))
    }

    # Default: 10X format
    exprs <- tryCatch(
        {
            Read10X(data.dir = path)
        },
        error = function(e) {
            tmpdatadir <- file.path(tmpdir, slugify(sam))
            if (dir.exists(tmpdatadir)) {
                unlink(tmpdatadir, recursive = TRUE)
            }
            dir.create(tmpdatadir, recursive = TRUE, showWarnings = FALSE)
            barcodefile <- Sys.glob(file.path(path, "*barcodes.tsv.gz"))[1]
            file.symlink(
                normalizePath(barcodefile),
                file.path(tmpdatadir, "barcodes.tsv.gz")
            )
            genefile <- glob(file.path(path, "*{genes,features}.tsv.gz"))[1]
            file.symlink(
                normalizePath(genefile),
                file.path(tmpdatadir, "features.tsv.gz")
            )
            matrixfile <- Sys.glob(file.path(path, "*matrix.mtx.gz"))[1]
            file.symlink(
                normalizePath(matrixfile),
                file.path(tmpdatadir, "matrix.mtx.gz")
            )
            Read10X(data.dir = tmpdatadir)
        }
    )
    list(exprs = exprs, cell_meta = NULL)
}


#' Perform the contamination correction
#'
#' Decontaminate the count matrix of the input SeuratObject based on the input contaminative
#' genes using a Youden index-based method.
#'
#' Patched version for CreateAssay5Object not found error in scCDC package.
#'
#' @param object a clustered SeuratObject
#' @param cont_genes a contaminative geneset within the input SeuratObject
#' @param auc_thres the AUROC threshold to determine the boundary between eGCG_positive and eGCG_negative clusters (Default as 0.9, 90 percent)
#' @param min.cell the parameter used to filter the cell populations without sufficient number of cells. Cell populations that reaches the threshold could be used in downstream analysis.
#'
#' @return the input clustered SeuratObject with a additional corrected assay of counts
#'
#' @keywords internal
#'
.scCDC.ContaminationCorrection <- function(
    object,
    cont_genes,
    auc_thres = 0.9,
    min.cell = 50
) {
    # calculate the threshold for every cont_gene (GCG)
    message('Calculating correction threshold...')
    ## We have normalized the data in the previous step for the clustering
    # object <- Seurat::NormalizeData(
    #     object,
    #     normalization.method = "LogNormalize",
    #     scale.factor = 10000,
    #     verbose = FALSE
    # ) # normalization

    Cal_AUCs <- utils::getFromNamespace("Cal_AUCs", "scCDC")
    Cal_thres <- utils::getFromNamespace("Cal_thres", "scCDC")
    num_cells <- table(SeuratObject::Idents(object))
    qualified_cls <- names(num_cells[num_cells >= min.cell])
    thres_vals <- unlist(lapply(cont_genes, function(x) {
        ###
        eGCG_aucs <- Cal_AUCs(object, x, qualified_cls)
        thres <- Cal_thres(object, x, eGCG_aucs[[1]], auc_thres = auc_thres)
        return(thres)
    }))
    # recover the status
    object@assays[[SeuratObject::DefaultAssay(object)]]@layers[[
        "data"
    ]] <- object@assays[[SeuratObject::DefaultAssay(object)]]@layers[["counts"]]

    # fetch the matrix with cont_genes
    exp_matrix <- SeuratObject::GetAssayData(object, layer = 'counts')
    decont_matrix_tmp <- exp_matrix[cont_genes, ]
    if (length(cont_genes) == 1) {
        decont_matrix_tmp <- t(as.matrix(decont_matrix_tmp))
    }

    operation_matrix <- cbind(decont_matrix_tmp, thres = as.numeric(thres_vals))
    # correcting the counts
    message('Decontaminating...')
    corrected_mat_part <- apply(operation_matrix, 1, function(x) {
        thres <- x[length(x)]
        x <- x[1:(length(x) - 1)]
        decont_x <- pmax(x - round(thres), 0)
        return(decont_x)
    })
    corrected_mat_part <- t(as.matrix(corrected_mat_part))
    # cover the original counts with corrected counts
    exp_matrix[cont_genes, ] <- corrected_mat_part

    Corrected_Assay <- SeuratObject::CreateAssayObject(
        counts = Matrix::Matrix(exp_matrix, sparse = T)
    )
    object <- SeuratObject::RenameAssays(
        object,
        assay.name = "RNA",
        new.assay.name = "Contaminated"
    )
    # object@assays[['Corrected']] <- Corrected_Assay
    # # add the default key to avoid some version problems
    # object@assays$Corrected@key = "corrected_"
    object@assays[['RNA']] <- as(Corrected_Assay, "Assay5")
    object@assays$RNA@key <- "RNA_"
    SeuratObject::DefaultAssay(object) <- "RNA"
    return(object)
}
