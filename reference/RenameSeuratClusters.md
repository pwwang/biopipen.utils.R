# Rename cluster names

Rename cluster names

## Usage

``` r
RenameSeuratClusters(object, ident = NULL, save_as = NULL, merge = FALSE, ...)
```

## Arguments

- object:

  Seurat object

- ident:

  Column name in `@meta.data` to rename Defaults to the identity column
  (from
  [`GetIdentityColumn()`](https://pwwang.github.io/biopipen.utils.R/reference/GetIdentityColumn.md))
  When `ident` is not identical as the column identified by
  [`GetIdentityColumn()`](https://pwwang.github.io/biopipen.utils.R/reference/GetIdentityColumn.md),

- save_as:

  If not NULL, save the renamed clusters to this new column. If NULL,
  will overwrite the original `ident` column.

- merge:

  Whether to merge the clusters with the same new name. Default is
  FALSE. Otherwise, the clusters with the same new name will be suffixed
  with ".1", ".2", etc. to make them unique.

- ...:

  Additional arguments. If named arguments are provided, they will be
  used as the mapping from old cluster names to new cluster names.

  - If a named argument's name does not exist in the original cluster
    names, it will be ignored.

  - If a original cluster name does not have a corresponding named
    argument, it will be kept unchanged.

  - If you want to exclude some clusters, you can set their new names to
    `NA`. If unnamed arguments are provided, it must be a single list of
    mappings. with the names being the original cluster names, and the
    values being the new cluster names.

## Value

The Seurat object with renamed clusters

## Details

This function provides a convenient way to rename clusters in a Seurat
object. You can provide the mapping of old cluster names to new cluster
names either as named arguments or as a single list. Unlike
[`Seurat::RenameIdents()`](https://satijalab.org/seurat/reference/reexports.html),
this function also renames the cluster names in the metadata column, so
that the changes are persistent. If `save_as` is provided, the renamed
clusters will be saved to a new column, otherwise the original column
will be overwritten. In addition, the order of the clusters will be
preserved based on the order of the original cluster names. For the
default identities (`Idents(object)`), it is only changed when `ident`
is NULL or identical to the identity column
(`GetIdentityColumn(object)`).

## Examples

``` r
# \donttest{
object <- scplotter::pancreas_sub
GetIdentityColumn(object)
#> [1] "seurat_clusters"
table(object$seurat_clusters)
#> 
#>   0   1   2   3   4   5   6 
#> 186 180 157 157 126 100  94 

# Rename using Seurat::RenameIdents
object1 <- Seurat::RenameIdents(object, `2` = 'Alpha', `1` = 'Beta')
# The order of identities is changed
table(SeuratObject::Idents(object1))
#> 
#> Alpha  Beta     0     3     4     5     6 
#>   157   180   186   157   126   100    94 
# The identity column is not changed
table(object1$seurat_clusters)
#> 
#>   0   1   2   3   4   5   6 
#> 186 180 157 157 126 100  94 
# Since the inconsistency, GetIdentityColumn can't determine the identity column
GetIdentityColumn(object1)
#> NULL

# Rename using RenameSeuratClusters
object1 <- RenameSeuratClusters(object, `2` = 'Alpha', `1` = 'Beta')
#> [1] "seurat_clusters"
# The order of identities is preserved
table(SeuratObject::Idents(object1))
#> 
#>     0  Beta Alpha     3     4     5     6 
#>   186   180   157   157   126   100    94 
# The identity column is also changed
table(object1$seurat_clusters)
#> 
#>     0  Beta Alpha     3     4     5     6 
#>   186   180   157   157   126   100    94 
# Since the consistency, GetIdentityColumn can still determine the identity column
GetIdentityColumn(object1)
#> [1] "seurat_clusters"

# Rename a non-identity column
table(object$CellType)
#> 
#>        Ductal   Ngn3 low EP  Ngn3 high EP Pre-endocrine     Endocrine 
#>           239            81           168           150           362 
object1 <- RenameSeuratClusters(
    object, Ductal = 'C1', `Endocrine` = 'C1', ident = 'CellType')
#> [1] "CellType"
table(object1$CellType)
#> 
#>          C1.1   Ngn3 low EP  Ngn3 high EP Pre-endocrine          C1.2 
#>           239            81           168           150           362 
table(object1$seurat_clusters)
#> 
#>   0   1   2   3   4   5   6 
#> 186 180 157 157 126 100  94 
# Identity column is not changed
GetIdentityColumn(object1)
#> [1] "seurat_clusters"

# Merge clusters with the same new name
object1 <- RenameSeuratClusters(
   object, Ductal = 'C1', `Endocrine` = 'C1', ident = 'CellType', merge = TRUE)
#> [1] "CellType"
table(object1$CellType)
#> 
#>            C1   Ngn3 low EP  Ngn3 high EP Pre-endocrine 
#>           601            81           168           150 

# Exclude some clusters by setting their new names to NA
object1 <- RenameSeuratClusters(
   object, Ductal = 'C1', `Endocrine` = NA, ident = 'CellType')
#> [1] "CellType"
#> Warning: [RenameSeuratClusters] The following clusters will be excluded: Endocrine
table(object1$CellType)
#> 
#>            C1   Ngn3 low EP  Ngn3 high EP Pre-endocrine 
#>           239            81           168           150 
# }
```
