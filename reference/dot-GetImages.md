# Patch for SeuratDisk's GetImages function

R \>= 4.5 validates [`unlist()`](https://rdrr.io/r/base/unlist.html)'s
`recursive` argument; SeuratDisk passes `index$global$images` there
*positionally*:
`unique(x = c(unlist(x = assays.images, index$global$images)))`. R 4.4
silently ignored it, R 4.5 aborts with "'recursive' must be a length-1
vector", so `LoadH5Seurat()` fails on any h5Seurat file that carries
global images. Keep the union of the assay images and the global images,
without the positional argument.

## Usage

``` r
.GetImages(images, index, assays = NULL)
```
