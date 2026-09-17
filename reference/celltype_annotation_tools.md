# List the cell type annotation tools supported by [`RunCellTypeAnnotation()`](https://pwwang.github.io/biopipen.utils.R/reference/RunCellTypeAnnotation.md)

List the cell type annotation tools supported by
[`RunCellTypeAnnotation()`](https://pwwang.github.io/biopipen.utils.R/reference/RunCellTypeAnnotation.md)

## Usage

``` r
celltype_annotation_tools(name = NULL)
```

## Arguments

- name:

  The name(s) of the tool(s) to look up. If `NULL` (default), all
  supported tools are returned.

## Value

A named list of the tools selected by `name`, each with:

- `level`: `"cluster"` or `"cell"`, the level the tool natively
  annotates at.

- `h5ad`: `TRUE` when the tool needs the object converted to AnnData
  (python-based tools, omitted otherwise).

## Examples

``` r
names(celltype_annotation_tools())
#>  [1] "hitype"         "sctype"         "sccatch"        "celltypist"    
#>  [5] "scsorter"       "scina"          "garnett"        "singler"       
#>  [9] "schdeepinsight" "llmcelltype"    "cellassign"     "scbert"        
#> [13] "scagenttype"    "cellid"         "direct"         "cell"          
#> [17] "ucell"          "aucell"         "gsva"           "singscore"     
#> [21] "scmap"          "cheetah"        "scclassify"     "scpred"        
#> [25] "mapquery"       "azimuth"        "scsa"           "maca"          
#> [29] "scmapnet"       "mllmcelltype"   "lict"          
celltype_annotation_tools("direct")
#> $direct
#> $direct$level
#> [1] "cluster"
#> 
#> 
```
