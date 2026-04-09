# Mutater the Seurat metadata

Mutater the Seurat metadata

## Usage

``` r
MutateSeuratMeta(object, mutaters, log = NULL)
```

## Arguments

- object:

  Seurat object

- mutaters:

  A named list of mutater expressions, where the names are the new
  column names and the values are the expressions to mutate the columns
  The name with the suffix `:ident` will be used as the new identity
  column The values can be either character strings of expressions to be
  parsed

- log:

  Logger object to log the messages. If NULL, the default logger will be
  used.

## Value

A Seurat object with mutated metadata

## Examples

``` r
# \donttest{
obj <- MutateSeuratMeta(
    SeuratObject::pbmc_small,
    list(a = 1, `g:ident` = "paste0(groups, '_ident')")
)
#> 
head(obj@meta.data[, c("a", "g")])
#>                a        g
#> ATGCCAGAACGACT 1 g2_ident
#> CATGGCCTGTGCAT 1 g1_ident
#> GAACCTGATGAACC 1 g2_ident
#> TGACTGGATTCTCA 1 g2_ident
#> AGTCAGACTGCACA 1 g2_ident
#> TCTGATACACGTGT 1 g1_ident
GetIdentityColumn(obj)
#> [1] "g"
# }
```
