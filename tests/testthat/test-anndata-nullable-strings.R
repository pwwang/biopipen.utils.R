# Tests for .normalize_h5ad_nullable_strings() and the anndata >= 0.13
# nullable string layout (a group with `values` and `mask`, where `mask == TRUE`
# means the value is missing) in ConvertAnnDataToSeurat().
# Fixtures are written with hdf5r only; no python/anndata involved.

quiet_log <- list(
    info = function(...) invisible(NULL),
    warn = function(...) invisible(NULL),
    debug = function(...) invisible(NULL)
)

write_string_dataset <- function(grp, name, values) {
    grp$create_dataset(
        name,
        robj = as.character(values),
        dtype = hdf5r::H5T_STRING$new(size = Inf)
    )
}

write_nullable_string <- function(grp, name, values) {
    missing <- is.na(values)
    sub <- grp$create_group(name)
    write_string_dataset(sub, "values", ifelse(missing, "", values))
    sub$create_dataset("mask", robj = missing)
}

write_h5ad_fixture <- function(
    path,
    nullable = FALSE,
    with_raw = FALSE,
    missing_batch = FALSE
) {
    cells <- paste0("cell", 1:5)
    genes <- paste0("gene", 1:3)
    symbols <- paste0("sym", 1:3)
    batch <- c("b1", "b2", "b3", "b1", "b2")
    if (missing_batch) {
        batch[2] <- NA
    }
    mat <- matrix(
        as.numeric(seq_len(15)),
        nrow = 3,
        dimnames = list(genes, cells)
    )

    f <- hdf5r::H5File$new(path, "w")
    on.exit(f$close_all(), add = TRUE)

    f$create_attr("encoding-type", "anndata")
    f$create_attr("encoding-version", "0.1.0")

    write_matrix <- function(name, mat) {
        ds <- f$create_dataset(name, robj = mat)
        ds$create_attr("encoding-type", "array")
        ds$create_attr("encoding-version", "0.1.0")
    }
    write_column <- function(grp, name, values) {
        if (nullable) {
            write_nullable_string(grp, name, values)
        } else {
            write_string_dataset(grp, name, values)
        }
    }
    write_df <- function(name, index, columns) {
        grp <- f$create_group(name)
        write_column(grp, "_index", index)
        for (nm in names(columns)) {
            write_column(grp, nm, columns[[nm]])
        }
        grp$create_attr("_index", "_index")
        grp$create_attr("column-order", names(columns))
        grp$create_attr("encoding-type", "dataframe")
        grp$create_attr("encoding-version", "0.2.0")
    }

    write_matrix("X", mat)
    write_df("obs", cells, list(batch = batch))
    write_df("var", genes, list(gene_symbol = symbols))
    if (with_raw) {
        f$create_group("raw")
        write_matrix("raw/X", mat)
        write_df("raw/var", genes, list(gene_symbol = symbols))
    }
}

test_that("legacy h5ad layout converts and is not copied", {
    path <- tempfile(fileext = ".h5ad")
    write_h5ad_fixture(path, nullable = FALSE)
    before <- unname(tools::md5sum(path))

    expect_identical(.normalize_h5ad_nullable_strings(path, quiet_log), path)

    obj <- ConvertAnnDataToSeurat(path, outfile = NULL, log = quiet_log)
    expect_equal(unname(dim(obj)), c(3L, 5L))
    expect_identical(rownames(obj), paste0("gene", 1:3))
    expect_identical(colnames(obj), paste0("cell", 1:5))
    expect_identical(as.character(obj$batch), c("b1", "b2", "b3", "b1", "b2"))
    expect_identical(unname(tools::md5sum(path)), before)
    unlink(path)
})

test_that("anndata >= 0.13 nullable strings are normalised on a temp copy", {
    path <- tempfile(fileext = ".h5ad")
    write_h5ad_fixture(path, nullable = TRUE, missing_batch = TRUE)
    before <- unname(tools::md5sum(path))

    normalized <- .normalize_h5ad_nullable_strings(path, quiet_log)
    expect_false(identical(normalized, path))
    expect_true(file.exists(normalized))
    unlink(normalized)

    obj <- ConvertAnnDataToSeurat(path, outfile = NULL, log = quiet_log)
    expect_equal(unname(dim(obj)), c(3L, 5L))
    expect_identical(rownames(obj), paste0("gene", 1:3))
    expect_identical(colnames(obj), paste0("cell", 1:5))
    # missing values are written as "" because hdf5r cannot store NA in strings
    expect_identical(as.character(obj$batch), c("b1", "", "b3", "b1", "b2"))
    expect_identical(unname(tools::md5sum(path)), before)
    unlink(path)
})

test_that("nullable raw/var (scaled branch) converts without error", {
    path <- tempfile(fileext = ".h5ad")
    write_h5ad_fixture(path, nullable = TRUE, with_raw = TRUE)
    before <- unname(tools::md5sum(path))

    obj <- ConvertAnnDataToSeurat(path, outfile = NULL, log = quiet_log)
    expect_equal(unname(dim(obj)), c(3L, 5L))
    expect_identical(rownames(obj), paste0("gene", 1:3))
    expect_identical(colnames(obj), paste0("cell", 1:5))
    scale <- SeuratObject::GetAssayData(
        obj,
        assay = "RNA",
        layer = "scale.data"
    )
    expect_equal(dim(scale), c(3L, 5L))
    expect_identical(unname(tools::md5sum(path)), before)
    unlink(path)
})
