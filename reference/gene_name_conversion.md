# Convert gene names between different formats

Convert gene names between different formats

## Usage

``` r
gene_name_conversion(
  genes,
  infmt,
  outfmt,
  dup = "first",
  species = "human",
  notfound = "na",
  suppress_messages = TRUE
)
```

## Arguments

- genes:

  A character/integer vector of gene names/ids

- infmt:

  A character vector of input gene name formats See the available scopes
  at https://docs.mygene.info/en/latest/doc/data.html#available-fields
  You can use ensg as a shortcut for ensembl.gene

- outfmt:

  A character vector of output gene name formats

- dup:

  How to deal with duplicate gene names found.

  - "first": keep the first one (default), sorted by score descendingly

  - "last": keep the last one, sorted by score descendingly

  - "all": keep all of them, each will be a separate row

  - "`<X>`": combine them into a single string, separated by `X`

- species:

  A character vector of species names

- notfound:

  How to deal with gene names that are not found

  - "error": stop with an error message

  - "use-query": use the query gene name as the converted gene name

  - "skip": skip the gene names that are not found

  - "ignore": Same as "skip"

  - "na"/"NA": use NA as the converted gene name (default)

- suppress_messages:

  Whether to suppress the warning messages

## Value

A tibble with the query gene names and the converted gene names When a
gene name is not found, the converted name will be NA When duplicate
gene names are found, the one with the highest score will be kept
