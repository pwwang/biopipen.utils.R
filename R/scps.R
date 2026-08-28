# scPS (single-cell Pathway Score): single-cell RNA-seq gene set analysis
# (scGSA) method. Ported from the scPS script
# (https://github.com/Thakar-Lab/scPS) by Ruoqiao Wang (2025).

#' Calculate combined PC scores of gene sets for scPS scoring
#'
#' Runs PCA on the expression of each gene set and combines the components
#' weighted by their explained variance. Ported from the scPS script
#' (https://github.com/Thakar-Lab/scPS).
#'
#' @param Seurat_data A Seurat object (Seurat V5) with a scaled `RNA` assay
#'   (`scale.data` layer) or an `SCT` assay
#' @param GeneSet A `GeneSet` or a `GeneSetCollection`
#' @return A matrix of combined PC scores, gene sets x cells
#' @keywords internal
GS_PCA_Calculation <- function(Seurat_data, GeneSet) {
    require_package("GSEABase")
    # RunPCA reads the scale.data of the default assay; make sure it exists
    # and covers the gene set genes, or RunPCA fails with a cryptic
    # "max(nu, nv) must be positive" error (or silently drops genes)
    sd_assay <- SeuratObject::DefaultAssay(Seurat_data)
    sd_layers <- tryCatch(
        SeuratObject::Layers(Seurat_data, assay = sd_assay),
        error = function(e) character(0)
    )
    if (!("scale.data" %in% sd_layers)) {
        stop(paste0(
            "No layer matching pattern 'scale.data' not found. ",
            "Please run ScaleData or SCTransform and retry"
        ))
    }
    scale_data <- SeuratObject::GetAssayData(Seurat_data, assay = sd_assay, layer = "scale.data")
    is_collection <- is(GeneSet, "GeneSetCollection")
    GSdiscription <- if (is_collection) {
        names(GeneSet)
    } else {
        GSEABase::setName(GeneSet)
    }
    GS_PCA <- list()
    for (currGS in GSdiscription) {
        genes <- if (is_collection) {
            GSEABase::geneIds(GeneSet[[currGS]])
        } else {
            GSEABase::geneIds(GeneSet)
        }
        unscaled <- setdiff(genes, rownames(scale_data))
        if (length(unscaled) > 0) {
            stop(paste0(
                "Some genes of the gene set '", currGS, "' are not scaled in the '",
                sd_assay, "' assay: ", paste(unscaled, collapse = ", "),
                ". Please run ScaleData (with the genes) and retry"
            ))
        }
        GS_PCA[[currGS]] <- Seurat::RunPCA(
            Seurat_data, features = genes, npcs = 10,
            weight.by.var = FALSE, verbose = FALSE
        )
    }
    # cumulative and per-PC variance explained
    varExplThresh <- 0.5 # target for cumulative max var explained
    cumulVarExp <- function(sdevPC) { cumsum(sdevPC^2) / sum(sdevPC^2) }
    varExp <- function(sdevPC) { (sdevPC^2) / sum(sdevPC^2) }
    maxCombPC <- vapply(GS_PCA, function(x) {
        idx <- which(cumulVarExp(Seurat::Stdev(x[["pca"]])) > varExplThresh)
        if (length(idx) == 0) 1 else idx[1]
    }, integer(1))
    # scale PC scores above 0
    aggregPC.scaled <- mat.or.vec(nr = length(GSdiscription), nc = ncol(Seurat_data))
    rownames(aggregPC.scaled) <- GSdiscription
    colnames(aggregPC.scaled) <- colnames(Seurat_data)
    PC.X <- lapply(GS_PCA, function(x) {
        emb <- Seurat::Embeddings(x[["pca"]])
        emb - min(emb)
    })
    for (currGS in rownames(aggregPC.scaled)) {
        currVarExp <- varExp(Seurat::Stdev(GS_PCA[[currGS]][["pca"]]))
        currWeightedPC <- sapply(1:maxCombPC[currGS], function(i) {
            PC.X[[currGS]][, i] * currVarExp[i]
        })
        if (ncol(currWeightedPC) > 1) {
            aggregPC.scaled[currGS, rownames(currWeightedPC)] <-
                sqrt(rowSums(currWeightedPC[, 1:maxCombPC[currGS], drop = FALSE]))
        } else {
            aggregPC.scaled[currGS, rownames(currWeightedPC)] <-
                sqrt(currWeightedPC[, 1:maxCombPC[currGS], drop = FALSE])
        }
    }
    aggregPC.scaled
}

#' Calculate the mean expression level of gene sets
#'
#' Ported from the scPS script (https://github.com/Thakar-Lab/scPS).
#'
#' @param Seurat_data A Seurat object (Seurat V5) with a normalized `SCT`
#'   or `RNA` assay
#' @param GeneSet A `GeneSet` or a `GeneSetCollection`
#' @return A matrix of mean expression levels, gene sets x cells
#' @keywords internal
GS_Experssion_Calculation <- function(Seurat_data, GeneSet) {
    require_package("GSEABase")
    assays <- SeuratObject::Assays(Seurat_data)
    if ("SCT" %in% assays) {
        expr <- SeuratObject::GetAssayData(Seurat_data, assay = "SCT", layer = "data")
    } else if ("data" %in% SeuratObject::Layers(Seurat_data, assay = "RNA")) {
        expr <- SeuratObject::GetAssayData(Seurat_data, assay = "RNA", layer = "data")
    } else {
        stop(paste0(
            "No layer matching pattern 'data' not found. ",
            "Please run NormalizeData or SCTransform and retry"
        ))
    }
    is_collection <- is(GeneSet, "GeneSetCollection")
    GSdiscription <- if (is_collection) {
        names(GeneSet)
    } else {
        GSEABase::setName(GeneSet)
    }
    mean_expression <- mat.or.vec(nr = length(GSdiscription), nc = ncol(Seurat_data))
    rownames(mean_expression) <- GSdiscription
    colnames(mean_expression) <- colnames(Seurat_data)
    for (currGS in GSdiscription) {
        genes <- if (is_collection) {
            GSEABase::geneIds(GeneSet[[currGS]])
        } else {
            GSEABase::geneIds(GeneSet)
        }
        mean_expression[currGS, ] <- apply(
            expr[rownames(expr) %in% genes, , drop = FALSE], 2, mean
        )
    }
    mean_expression
}

#' Calculate scPS scores of gene sets for single cells
#'
#' scPS (single-cell Pathway Score) scores each cell for each gene set as the
#' product of the combined PC score (from PCA on the scaled expression of the
#' gene set) and the mean expression level of the gene set in the cell.
#' Ported from the scPS script
#' (https://github.com/Thakar-Lab/scPS) (see references).
#'
#' @param Seurat_data A Seurat object (Seurat V5) with a scaled `RNA` assay
#'   (`scale.data` layer) or an `SCT` assay, and a normalized `data` layer
#' @param GeneSet A `GeneSet` or a `GeneSetCollection`, or a path to a GMT
#'   file of gene sets
#' @return A matrix of scPS scores, gene sets x cells
#' @keywords internal
scPS <- function(Seurat_data, GeneSet) {
    if (is.character(GeneSet)) {
        require_package("GSEABase")
        GeneSet <- GSEABase::getGmt(GeneSet)
    }
    GS_PCs <- GS_PCA_Calculation(Seurat_data, GeneSet)
    GS_MeanExpress <- GS_Experssion_Calculation(Seurat_data, GeneSet)
    GS_PCs * GS_MeanExpress
}
