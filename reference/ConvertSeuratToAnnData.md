# Convert a Seurat object (or RDS/H5Seurat/qs2 file) to an AnnData object file

Convert a Seurat object (or RDS/H5Seurat/qs2 file) to an AnnData object
file

## Usage

``` r
ConvertSeuratToAnnData(
  object_or_file,
  outfile,
  assay = NULL,
  subset = NULL,
  log = NULL
)
```

## Arguments

- object_or_file:

  The Seurat object or the path to the RDS or H5Seurat file

- outfile:

  Output file

- assay:

  Assay to be used

- subset:

  Subset of cells to be kept in the AnnData object

- log:

  Logger

## Value

No return value

## Examples

``` r
# \donttest{
ConvertSeuratToAnnData(SeuratObject::pbmc_small, "/tmp/pbmc_small.h5ad")
#> Registered S3 method overwritten by 'SeuratDisk':
#>   method            from  
#>   as.sparse.H5Group Seurat
#> Error in ConvertSeuratToAnnData(SeuratObject::pbmc_small, "/tmp/pbmc_small.h5ad"): object 'object' not found
ConvertSeuratToAnnData(SeuratObject::pbmc_small, "/tmp/pbmc_small.g1.h5ad",
    subset = 'groups == "g1"'
)
#> Error in ConvertSeuratToAnnData(SeuratObject::pbmc_small, "/tmp/pbmc_small.g1.h5ad",     subset = "groups == \"g1\""): object 'object' not found

saveRDS(SeuratObject::pbmc_small, "/tmp/pbmc_small.rds")
ConvertSeuratToAnnData("/tmp/pbmc_small.rds", "/tmp/pbmc_small.h5ad")
#> Warning: Key ‘rna_’ taken, using ‘rnav3_’ instead
#> Renaming default assay from RNAv3 to RNA
#> Warning: Key ‘rnav3_’ taken, using ‘rna_’ instead
#> Creating h5Seurat file for version 3.1.5.9900
#> Warning: The `slot` argument of `GetAssayData()` is deprecated as of SeuratObject 5.0.0.
#> ℹ Please use the `layer` argument instead.
#> ℹ The deprecated feature was likely used in the SeuratDisk package.
#>   Please report the issue at
#>   <https://github.com/mojaveazure/seurat-disk/issues>.
#> Adding counts for RNA
#> Adding data for RNA
#> Adding scale.data for RNA
#> Adding variable features for RNA
#> Adding feature-level metadata for RNA
#> Adding cell embeddings for pca
#> Adding loadings for pca
#> Adding projected loadings for pca
#> Adding standard deviations for pca
#> Adding JackStraw information for pca
#> Adding cell embeddings for tsne
#> No loadings for tsne
#> No projected loadings for tsne
#> No standard deviations for tsne
#> No JackStraw data for tsne
#> Validating h5Seurat file
#> Adding scale.data from RNA as X
#> Transfering meta.features to var
#> Adding data from RNA as raw
#> Transfering meta.features to raw/var
#> Transfering meta.data to obs
#> Adding dimensional reduction information for pca
#> Adding feature loadings for pca
#> Adding dimensional reduction information for tsne
#> Adding RNA_snn as neighbors
# }
```
