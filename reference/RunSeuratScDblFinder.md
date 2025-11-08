# Run SCDblFinder on a Seurat object

Run SCDblFinder on a Seurat object

## Usage

``` r
RunSeuratScDblFinder(object, ncores = 1, ...)
```

## Arguments

- object:

  Seurat object

- ncores:

  Number of cores to use

- ...:

  Additional arguments to pass to
  [scDblFinder::scDblFinder](https://plger.github.io/scDblFinder/reference/scDblFinder.html)

## Value

The Seurat object with doublet detection results in `@misc$scDblFinder`
