
#' SeuratDisk's "Project<-.h5Seurat" function
#' @keywords internal
".set_h5_seurat_project<-" <- function(object, ..., value) {
    object$attr_delete(attr_name = 'project')
    object$create_attr(
        attr_name = 'project',
        robj = value,
        dtype = SeuratDisk:::GuessDType(x = value)
    )
    return(invisible(x = object))
}

#' patch to fix https://github.com/mojaveazure/seurat-disk/issues/31
#' @keywords internal
.H5ADToH5Seurat <- function(
    source,
    dest,
    assay = "RNA",
    overwrite = FALSE,
    verbose = TRUE) {
    if (file.exists(dest)) {
        if (overwrite) {
            file.remove(dest)
        } else {
            stop("Destination h5Seurat file exists", call. = FALSE)
        }
    }
    dfile <- SeuratDisk::h5Seurat$new(filename = dest, mode = SeuratDisk:::WriteMode(overwrite = FALSE))
    # Get rownames from an H5AD data frame
    #
    # @param dset Name of data frame
    #
    # @return Returns the name of the dataset that contains the rownames
    #
    GetRownames <- function(dset) {
        if (inherits(x = source[[dset]], what = "H5Group")) {
            # rownames <- if (source[[dset]]$attr_exists(attr_name = '_index')) {
            rownames <- if (isTRUE(x = SeuratDisk:::AttrExists(x = source[[dset]], name = "_index"))) {
                hdf5r::h5attr(x = source[[dset]], which = "_index")
            } else if (source[[dset]]$exists(name = "_index")) {
                "_index"
            } else if (source[[dset]]$exists(name = "index")) {
                "index"
            } else {
                stop("Cannot find rownames in ", dset, call. = FALSE)
            }
        } else {
            # TODO: fix this
            stop("Don't know how to handle datasets", call. = FALSE)
            # rownames(x = source[[dset]])
        }
        return(rownames)
    }
    ColToFactor <- function(dfgroup) {
        if (dfgroup$exists(name = "__categories")) {
            for (i in names(x = dfgroup[["__categories"]])) {
                tname <- basename(path = tempfile(tmpdir = ""))
                dfgroup$obj_copy_to(dst_loc = dfgroup, dst_name = tname, src_name = i)
                dfgroup$link_delete(name = i)
                # Because AnnData stores logicals as factors, but have too many levels
                # for factors
                bool.check <- dfgroup[["__categories"]][[i]]$dims == 2
                if (isTRUE(x = bool.check)) {
                    bool.check <- all(sort(x = dfgroup[["__categories"]][[i]][]) == c("False", "True"))
                }
                if (isTRUE(x = bool.check)) {
                    dfgroup$create_dataset(
                        name = i,
                        robj = dfgroup[[tname]][] + 1L,
                        dtype = dfgroup[[tname]]$get_type()
                    )
                } else {
                    dfgroup$create_group(name = i)
                    dfgroup[[i]]$create_dataset(
                        name = "values",
                        robj = dfgroup[[tname]][] + 1L,
                        dtype = dfgroup[[tname]]$get_type()
                    )
                    if (SeuratDisk:::IsDType(x = dfgroup[["__categories"]][[i]], dtype = "H5T_STRING")) {
                        dfgroup$obj_copy_to(
                            dst_loc = dfgroup,
                            dst_name = paste0(i, "/levels"),
                            src_name = paste0("__categories/", i)
                        )
                    } else {
                        dfgroup[[i]]$create_dataset(
                            name = "levels",
                            robj = as.character(x = dfgroup[[H5Path("__categories", i)]][]),
                            dtype = StringType()
                        )
                    }
                }
                dfgroup$link_delete(name = tname)
                # col.order <- hdf5r::h5attr(x = dfile[['var']], which = 'column-order')
                # col.order <- c(col.order, var.name)
                # dfile[['var']]$attr_rename(
                #   old_attr_name = 'column-order',
                #   new_attr_name = 'old-column-order'
                # )
                # dfile[['var']]$create_attr(
                #   attr_name = 'column-order',
                #   robj = col.order,
                #   dtype = SeuratDisk:::GuessDType(x = col.order)
                # )
                # dfile[['var']]$attr_delete(attr_name = 'old-column-order')
            }
            dfgroup$link_delete(name = "__categories")
        }
        return(invisible(x = NULL))
    }
    ds.map <- c(
        scale.data = if (inherits(x = source[["X"]], what = "H5D")) {
            "X"
        } else {
            NULL
        },
        data = if (inherits(x = source[["X"]], what = "H5D") && source$exists(name = "raw")) {
            "raw/X"
        } else {
            "X"
        },
        counts = if (source$exists(name = "raw")) {
            "raw/X"
        } else {
            "X"
        }
    )
    # Add assay data
    assay.group <- dfile[["assays"]]$create_group(name = assay)
    for (i in seq_along(along.with = ds.map)) {
        if (verbose) {
            message("Adding ", ds.map[[i]], " as ", names(x = ds.map)[i])
        }
        dst <- names(x = ds.map)[i]
        assay.group$obj_copy_from(
            src_loc = source,
            src_name = ds.map[[i]],
            dst_name = dst
        )
        # if (assay.group[[dst]]$attr_exists(attr_name = 'shape')) {
        if (isTRUE(x = SeuratDisk:::AttrExists(x = assay.group[[dst]], name = "shape"))) {
            dims <- rev(x = hdf5r::h5attr(x = assay.group[[dst]], which = "shape"))
            assay.group[[dst]]$create_attr(
                attr_name = "dims",
                robj = dims,
                dtype = SeuratDisk:::GuessDType(x = dims)
            )
            assay.group[[dst]]$attr_delete(attr_name = "shape")
        }
    }
    features.source <- ifelse(
        test = source$exists(name = "raw") && source$exists(name = "raw/var"),
        yes = "raw/var",
        no = "var"
    )
    if (inherits(x = source[[features.source]], what = "H5Group")) {
        features.dset <- GetRownames(dset = features.source)
        assay.group$obj_copy_from(
            src_loc = source,
            src_name = paste(features.source, features.dset, sep = "/"),
            dst_name = "features"
        )
    } else {
        tryCatch(
            expr = assay.group$create_dataset(
                name = "features",
                robj = rownames(x = source[[features.source]]),
                dtype = SeuratDisk:::GuessDType(x = "")
            ),
            error = function(...) {
                stop("Cannot find feature names in this H5AD file", call. = FALSE)
            }
        )
    }
    scaled <- !is.null(x = ds.map["scale.data"]) && !is.na(x = ds.map["scale.data"])
    if (scaled) {
        if (inherits(x = source[["var"]], what = "H5Group")) {
            scaled.dset <- GetRownames(dset = "var")
            assay.group$obj_copy_from(
                src_loc = source,
                src_name = paste0("var/", scaled.dset),
                dst_name = "scaled.features"
            )
        } else {
            tryCatch(
                expr = assay.group$create_dataset(
                    name = "scaled.features",
                    robj = rownames(x = source[["var"]]),
                    dtype = SeuratDisk:::GuessDType(x = "")
                ),
                error = function(...) {
                    stop("Cannot find scaled features in this H5AD file", call. = FALSE)
                }
            )
        }
    }
    assay.group$create_attr(
        attr_name = "key",
        robj = paste0(tolower(x = assay), "_"),
        dtype = SeuratDisk:::GuessDType(x = assay)
    )
    # Set default assay
    SeuratObject::DefaultAssay(object = dfile) <- assay
    # Add feature-level metadata
    if (!getOption(x = "SeuratDisk.dtypes.dataframe_as_group", default = FALSE)) {
        warning(
            "Adding feature-level metadata as a compound is not yet supported",
            call. = FALSE,
            immediate. = TRUE
        )
    }
    # TODO: Support compound metafeatures
    if (SeuratDisk:::Exists(x = source, name = "raw/var")) {
        if (inherits(x = source[["raw/var"]], what = "H5Group")) {
            if (verbose) {
                message("Adding meta.features from raw/var")
            }
            assay.group$obj_copy_from(
                src_loc = source,
                src_name = "raw/var",
                dst_name = "meta.features"
            )
            if (scaled) {
                features.use <- assay.group[["features"]][] %in% assay.group[["scaled.features"]][]
                features.use <- which(x = features.use)
                meta.scaled <- names(x = source[["var"]])
                meta.scaled <- meta.scaled[!meta.scaled %in% c("__categories", scaled.dset)]
                for (mf in meta.scaled) {
                    if (!mf %in% names(x = assay.group[["meta.features"]])) {
                        if (verbose) {
                            message("Adding ", mf, " from scaled feature-level metadata")
                        }
                        assay.group[["meta.features"]]$create_dataset(
                            name = mf,
                            dtype = source[["var"]][[mf]]$get_type(),
                            space = hdf5r::H5S$new(dims = assay.group[["features"]]$dims)
                        )
                    } else if (verbose) {
                        message("Merging ", mf, " from scaled feature-level metadata")
                    }
                    assay.group[["meta.features"]][[mf]][features.use] <- source[["var"]][[mf]]$read()
                }
            }
        } else {
            warning(
                "Cannot yet add feature-level metadata from compound datasets",
                call. = FALSE,
                immediate. = TRUE
            )
            assay.group$create_group(name = "meta.features")
        }
    } else {
        if (inherits(x = source[["var"]], what = "H5Group")) {
            if (verbose) {
                message("Adding meta.features from var")
            }
            assay.group$obj_copy_from(
                src_loc = source,
                src_name = "var",
                dst_name = "meta.features"
            )
        } else {
            warning(
                "Cannot yet add feature-level metadata from compound datasets",
                call. = FALSE,
                immediate. = TRUE
            )
            assay.group$create_group(name = "meta.features")
        }
    }
    ColToFactor(dfgroup = assay.group[["meta.features"]])
    # if (assay.group[['meta.features']]$attr_exists(attr_name = 'column-order')) {
    if (isTRUE(x = SeuratDisk:::AttrExists(x = assay.group[["meta.features"]], name = "column-order"))) {
        colnames <- hdf5r::h5attr(
            x = assay.group[["meta.features"]],
            which = "column-order"
        )
        assay.group[["meta.features"]]$create_attr(
            attr_name = "colnames",
            robj = colnames,
            dtype = SeuratDisk:::GuessDType(x = colnames)
        )
    }
    if (inherits(x = source[["var"]], what = "H5Group")) {
        assay.group[["meta.features"]]$link_delete(name = GetRownames(dset = "var"))
    }
    # Add cell-level metadata
    if (source$exists(name = "obs") && inherits(x = source[["obs"]], what = "H5Group")) {
        if (!source[["obs"]]$exists(name = "__categories") && !getOption(x = "SeuratDisk.dtypes.dataframe_as_group", default = TRUE)) {
            warning(
                "Conversion from H5AD to h5Seurat allowing compound datasets is not yet implemented",
                call. = FALSE,
                immediate. = TRUE
            )
        }
        dfile$obj_copy_from(
            src_loc = source,
            src_name = "obs",
            dst_name = "meta.data"
        )
        ColToFactor(dfgroup = dfile[["meta.data"]])
        # if (dfile[['meta.data']]$attr_exists(attr_name = 'column-order')) {
        if (isTRUE(x = SeuratDisk:::AttrExists(x = dfile[["meta.data"]], name = "column-order"))) {
            colnames <- hdf5r::h5attr(x = dfile[["meta.data"]], which = "column-order")
            dfile[["meta.data"]]$create_attr(
                attr_name = "colnames",
                robj = colnames,
                dtype = SeuratDisk:::GuessDType(x = colnames)
            )
        }
        rownames <- GetRownames(dset = "obs")
        dfile$obj_copy_from(
            src_loc = dfile,
            src_name = paste0("meta.data/", rownames),
            dst_name = "cell.names"
        )
        dfile[["meta.data"]]$link_delete(name = rownames)
    } else {
        warning(
            "No cell-level metadata present, creating fake cell names",
            call. = FALSE,
            immediate. = TRUE
        )
        ncells <- if (inherits(x = assay.group[["data"]], what = "H5Group")) {
            assay.group[["data/indptr"]]$dims - 1
        } else {
            assay.group[["data"]]$dims[2]
        }
        dfile$create_group(name = "meta.data")
        dfile$create_dataset(
            name = "cell.names",
            robj = paste0("Cell", seq.default(from = 1, to = ncells)),
            dtype = SeuratDisk:::GuessDType(x = "Cell1")
        )
    }
    # Add dimensional reduction information
    if (source$exists(name = "obsm")) {
        # Add cell embeddings
        if (inherits(x = source[["obsm"]], what = "H5Group")) {
            for (reduc in names(x = source[["obsm"]])) {
                sreduc <- gsub(pattern = "^X_", replacement = "", x = reduc)
                reduc.group <- dfile[["reductions"]]$create_group(name = sreduc)
                message("Adding ", reduc, " as cell embeddings for ", sreduc)
                SeuratDisk:::Transpose(
                    x = source[["obsm"]][[reduc]],
                    dest = reduc.group,
                    dname = "cell.embeddings",
                    verbose = FALSE
                )
                reduc.group$create_group(name = "misc")
                reduc.group$create_attr(
                    attr_name = "active.assay",
                    robj = assay,
                    dtype = SeuratDisk:::GuessDType(x = assay)
                )
                key <- paste0(
                    if (grepl(pattern = "pca", x = sreduc, ignore.case = TRUE)) {
                        "PC"
                    } else if (grepl(pattern = "tsne", x = sreduc, ignore.case = TRUE)) {
                        "tSNE"
                    } else {
                        sreduc
                    },
                    "_"
                )
                reduc.group$create_attr(
                    attr_name = "key",
                    robj = key,
                    dtype = SeuratDisk:::GuessDType(x = reduc)
                )
                global <- SeuratDisk:::BoolToInt(x = grepl(
                    pattern = "tsne|umap",
                    x = sreduc,
                    ignore.case = TRUE
                ))
                reduc.group$create_attr(
                    attr_name = "global",
                    robj = global,
                    dtype = SeuratDisk:::GuessDType(x = global)
                )
            }
        } else {
            warning(
                "Reading compound dimensional reductions not yet supported, please update your H5AD file",
                call. = FALSE,
                immediate. = TRUE
            )
        }
        # Add feature loadings
        if (source$exists(name = "varm")) {
            if (inherits(x = source[["varm"]], what = "H5Group")) {
                for (reduc in names(x = source[["varm"]])) {
                    sreduc <- switch(EXPR = reduc,
                        "PCs" = "pca",
                        tolower(x = reduc)
                    )
                    if (!isTRUE(x = sreduc %in% names(x = dfile[["reductions"]]))) {
                        warning(
                            "Cannot find a reduction named ",
                            sreduc,
                            " (",
                            reduc,
                            " in varm)",
                            call. = FALSE,
                            immediate. = TRUE
                        )
                        next
                    }
                    if (isTRUE(x = verbose)) {
                        message("Adding ", reduc, " as feature loadings fpr ", sreduc)
                    }
                    SeuratDisk:::Transpose(
                        x = source[["varm"]][[reduc]],
                        dest = dfile[["reductions"]][[sreduc]],
                        dname = "feature.loadings",
                        verbose = FALSE
                    )
                    reduc.features <- dfile[["reductions"]][[sreduc]][["feature.loadings"]]$dims[1]
                    assay.features <- if (assay.group[["features"]]$dims == reduc.features) {
                        "features"
                    } else if (assay.group$exists(name = "scaled.features") && assay.group[["scaled.features"]]$dims == reduc.features) {
                        "scaled.features"
                    } else {
                        NULL
                    }
                    if (is.null(x = assay.features)) {
                        warning(
                            "Cannot find features for feature loadings, will not be able to load",
                            call. = FALSE,
                            immediate. = TRUE
                        )
                    } else {
                        dfile[["reductions"]][[sreduc]]$obj_copy_from(
                            src_loc = assay.group,
                            src_name = assay.features,
                            dst_name = "features"
                        )
                    }
                }
            } else {
                warning(
                    "Reading compound dimensional reductions not yet supported",
                    call. = FALSE,
                    immediate. = TRUE
                )
            }
        }
        # Add miscellaneous information
        if (source$exists(name = "uns")) {
            for (reduc in names(x = source[["uns"]])) {
                if (!isTRUE(x = reduc %in% names(x = dfile[["reductions"]]))) {
                    next
                }
                if (verbose) {
                    message("Adding miscellaneous information for ", reduc)
                }
                dfile[["reductions"]][[reduc]]$link_delete(name = "misc")
                dfile[["reductions"]][[reduc]]$obj_copy_from(
                    src_loc = source[["uns"]],
                    src_name = reduc,
                    dst_name = "misc"
                )
                if ("variance" %in% names(x = dfile[["reductions"]][[reduc]][["misc"]])) {
                    if (verbose) {
                        message("Adding standard deviations for ", reduc)
                    }
                    dfile[["reductions"]][[reduc]]$create_dataset(
                        name = "stdev",
                        robj = sqrt(x = dfile[["reductions"]][[reduc]][["misc"]][["variance"]][]),
                        dtype = SeuratDisk:::GuessDType(x = 1.0)
                    )
                }
            }
        }
    }
    # Add project and cell identities
    .set_h5_seurat_project(object = dfile) <- "AnnData"
    idents <- dfile$create_group(name = "active.ident")
    idents$create_dataset(
        name = "values",
        dtype = SeuratDisk:::GuessDType(x = 1L),
        space = hdf5r::H5S$new(dims = dfile[["cell.names"]]$dims)
    )
    idents$create_dataset(
        name = "levels",
        robj = "AnnData",
        dtype = SeuratDisk:::GuessDType(x = "AnnData")
    )
    idents[["values"]]$write(
        args = list(seq.default(from = 1, to = idents[["values"]]$dims)),
        value = 1L
    )
    # Add nearest-neighbor graph
    if (SeuratDisk:::Exists(x = source, name = "uns/neighbors/distances")) {
        graph.name <- paste(
            assay,
            ifelse(
                test = source$exists(name = "uns/neighbors/params/method"),
                yes = source[["uns/neighbors/params/method"]][1],
                no = "anndata"
            ),
            sep = "_"
        )
        if (verbose) {
            message("Saving nearest-neighbor graph as ", graph.name)
        }
        dfile[["graphs"]]$obj_copy_from(
            src_loc = source,
            src_name = "uns/neighbors/distances",
            dst_name = graph.name
        )
        # if (dfile[['graphs']][[graph.name]]$attr_exists(attr_name = 'shape')) {
        if (isTRUE(x = SeuratDisk:::AttrExists(x = dfile[["graphs"]], name = "shape"))) {
            dfile[["graphs"]][[graph.name]]$create_attr(
                attr_name = "dims",
                robj = hdf5r::h5attr(x = dfile[["graphs"]][[graph.name]], which = "shape"),
                dtype = SeuratDisk:::GuessDType(x = hdf5r::h5attr(
                    x = dfile[["graphs"]][[graph.name]],
                    which = "shape"
                ))
            )
            dfile[["graphs"]][[graph.name]]$attr_delete(attr_name = "shape")
        }
        if (isTRUE(x = SeuratDisk:::AttrExists(x = dfile[["graphs"]][[graph.name]], name = "assay.used"))) {
            dfile[["graphs"]][[graph.name]]$attr_delete(attr_name = "assay.used")
        }
        dfile[["graphs"]][[graph.name]]$create_attr(
            attr_name = "assay.used",
            robj = assay,
            dtype = SeuratDisk:::GuessDType(x = assay)
        )
    }
    # Add miscellaneous information
    if (source$exists(name = "uns")) {
        misc <- setdiff(
            x = names(x = source[["uns"]]),
            y = c("neighbors", names(x = dfile[["reductions"]]))
        )
        for (i in misc) {
            if (verbose) {
                message("Adding ", i, " to miscellaneous data")
            }
            dfile[["misc"]]$obj_copy_from(
                src_loc = source[["uns"]],
                src_name = i,
                dst_name = i
            )
        }
    }
    # Add layers
    if (SeuratDisk:::Exists(x = source, name = "layers")) {
        slots <- c("data")
        if (!isTRUE(x = scaled)) {
            slots <- c(slots, "counts")
        }
        for (layer in names(x = source[["layers"]])) {
            layer.assay <- dfile[["assays"]]$create_group(name = layer)
            layer.assay$obj_copy_from(
                src_loc = dfile[["assays"]][[assay]],
                src_name = "features",
                dst_name = "features"
            )
            layer.assay$create_attr(
                attr_name = "key",
                robj = SeuratDisk:::UpdateKey(key = layer),
                dtype = SeuratDisk:::GuessDType(x = layer)
            )
            for (slot in slots) {
                if (verbose) {
                    message("Adding layer ", layer, " as ", slot, " in assay ", layer)
                }
                layer.assay$obj_copy_from(
                    src_loc = source[["layers"]],
                    src_name = layer,
                    dst_name = slot
                )
                # if (layer.assay[[slot]]$attr_exists(attr_name = 'shape')) {
                if (isTRUE(x = SeuratDisk:::AttrExists(x = layer.assay[[slot]], name = "shape"))) {
                    dims <- rev(x = hdf5r::h5attr(x = layer.assay[[slot]], which = "shape"))
                    layer.assay[[slot]]$create_attr(
                        attr_name = "dims",
                        robj = dims,
                        dtype = SeuratDisk:::GuessDType(x = dims)
                    )
                    layer.assay[[slot]]$attr_delete(attr_name = "shape")
                }
            }
        }
    }
    return(dfile)
}
