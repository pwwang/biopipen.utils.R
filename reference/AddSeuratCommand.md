# Add a command to a Seurat object `@commands` slot

Add a command to a Seurat object `@commands` slot

## Usage

``` r
AddSeuratCommand(
  object,
  name,
  call.string = paste0(name, "(object, ...)"),
  params = list(),
  assay.used = NULL,
  time.stamp = Sys.time()
)
```

## Arguments

- object:

  Seurat object

- name:

  Name of the command

- call.string:

  Call string of the command

- params:

  Parameters of the command

- assay.used:

  Assay used in the command

- time.stamp:

  Time stamp of the command

## Value

The Seurat object with the command added

## Examples

``` r
object <- SeuratObject::pbmc_small
names(object@commands)
#>  [1] "NormalizeData.RNA"        "ScaleData.RNA"           
#>  [3] "RunPCA.RNA"               "BuildSNN.RNA.pca"        
#>  [5] "FindClusters"             "RunTSNE.pca"             
#>  [7] "JackStraw.RNA.pca"        "ScoreJackStraw.pca"      
#>  [9] "ProjectDim.RNA.pca"       "FindVariableFeatures.RNA"

object <- AddSeuratCommand(
    object, "RunSeuratDEAnalysis",
    "RunSeuratDEAnalysis(object, group_by = 'groups', ident_1 = 'g1', ident_2 = 'g2')",
    list(group_by = "groups", ident_1 = "g1", ident_2 = "g2")
)
object@commands$RunSeuratDEAnalysis
#> Command: RunSeuratDEAnalysis(object, group_by = 'groups', ident_1 = 'g1', ident_2 = 'g2')
#> Time: 2026-02-26 04:39:03.153943
#> group_by : groups 
#> ident_1 : g1 
#> ident_2 : g2 
```
