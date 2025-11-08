# Run data integration on Seurat object

Run data integration on Seurat object

## Usage

``` r
RunSeuratIntegration(
  object,
  no_integration = FALSE,
  IntegrateLayersArgs = list(),
  log = NULL,
  cache = NULL
)
```

## Arguments

- object:

  Seurat object

- no_integration:

  Whether to skip integration, just join layers

- IntegrateLayersArgs:

  Arguments to pass to
  [Seurat::IntegrateLayers](https://satijalab.org/seurat/reference/IntegrateLayers.html)

- log:

  Logger

- cache:

  Directory to cache the results. Set to `FALSE` to disable caching

## Value

The Seurat object with integrated data
