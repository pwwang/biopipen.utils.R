# Aggregate expression of single cells into psedobulk expression matrix

Aggregate expression of single cells into psedobulk expression matrix

## Usage

``` r
AggregateExpressionPseudobulk(
  object,
  aggregate_by = "Sample",
  assay = "RNA",
  layer = "counts",
  subset = NULL,
  log = NULL
)
```

## Arguments

- object:

  Seurat object

- aggregate_by:

  The metadata column to aggregate by. Default is "Sample". You can add
  other columns to aggregate by, such as "Condition", "Batch", etc.

- assay:

  The assay to use for aggregation. Default is "RNA".

- layer:

  The layer to use for aggregation. Default is "counts".

- subset:

  A string to filter the cells before aggregation. For example,
  `subset = "Condition == 'Control'"` will only aggregate cells from the
  "Control" condition.

- log:

  Logger

## Value

The expression matrix aggregated by the specified metadata columns. With
the metadata as the metadata at `meta` attribute.

## Examples

``` r
# \donttest{
obj <- SeuratObject::pbmc_small
obj$Sample <- rep(paste0("S", 1:10), each = ncol(obj) / 10)
obj$Condition <- rep(c("Control", "Treatment"), each = ncol(obj) / 2)
result <- AggregateExpressionPseudobulk(obj, aggregate_by = c("Sample", "Condition"))
#> INFO    [2025-11-25 20:36:40] Aggregating expression by: Sample, Condition
#> INFO    [2025-11-25 20:36:40] Aggregating expression matrix ...
#> INFO    [2025-11-25 20:36:40] Creating metadata for aggregated samples ...
#> INFO    [2025-11-25 20:36:40] Aggregation complete. Matrix dimensions: 230 x 10
head(result)
#>          S1_Control S2_Control S3_Control S4_Control S5_Control S6_Treatment
#> MS4A1             0         17         12          1          0            0
#> CD79B             1         18         10          2          2            0
#> CD79A             0         22         24          1          0            0
#> HLA-DRA           4        110         98         34          3            3
#> TCL1A             0          9         11          0          0            0
#> HLA-DQB1          1         21         10          2          0            0
#>          S7_Treatment S8_Treatment S9_Treatment S10_Treatment
#> MS4A1               0            0            1             0
#> CD79B               7            7            1             0
#> CD79A               0            8            1             0
#> HLA-DRA            59          320          430            13
#> TCL1A               0            4            0             0
#> HLA-DQB1            4           52           46             1
head(attr(result, "meta"))
#>         Sample Condition
#> 1   S1_Control   Control
#> 2   S2_Control   Control
#> 3   S3_Control   Control
#> 4   S4_Control   Control
#> 5   S5_Control   Control
#> 6 S6_Treatment Treatment
# }
```
