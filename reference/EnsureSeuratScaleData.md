# Ensure marker genes are in the scale.data layer of an assay of the Seurat object

Values added for the missing features are clamped to a range resolved
as: the `clip.range` argument when one is given, otherwise the range the
scalers recorded in `@misc$scale_clip[[assay]]`, otherwise the
`c(-10, 10)` default. Pass `NULL` or a range of `Inf` to add the values
uncapped. The range applies to both the z-scores of an RNA assay and the
residuals of an SCT assay; for the latter a recorded range (e.g. the
clip of the SCT models) is normally wider than the +/- 10 default and
wins over it.

## Usage

``` r
EnsureSeuratScaleData(
  object,
  features,
  assay = NULL,
  umi_assay = "RNA",
  clip.range = c(-10, 10)
)
```

## Arguments

- object:

  Seurat object

- features:

  Character vector, or a list of character vectors, of feature names to
  ensure in the scale.data layer

- assay:

  Assay to use. If NULL, the default assay will be used.

- umi_assay:

  Assay to use for the UMI counts. Default is "RNA". This is used to get
  the counts for scaling.

- clip.range:

  Numeric vector of length 2: the lower and upper bound applied to the
  added values, in the units of the scale.data layer (z-scores, or
  residuals for an SCT assay). `Inf` means unbounded. When omitted, the
  range recorded in `object@misc$scale_clip[[assay]]` is reused, falling
  back to the `c(-10, 10)` default. Pass `NULL` (or `c(-Inf, Inf)`) to
  clamp nothing.

## Value

The Seurat object with the features ensured in the scale.data layer
