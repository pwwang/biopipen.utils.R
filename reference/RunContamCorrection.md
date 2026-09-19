# Run contaminant RNA correction on a Seurat object

Run contaminant RNA correction on a Seurat object

## Usage

``` r
RunContamCorrection(
  object,
  method,
  decontXArgs = list(),
  scCDCArgs = list(Detection = list(), Quantification = list(), Correction = list()),
  keep_contam_assay = FALSE,
  log = NULL
)
```

## Arguments

- object:

  Seurat object

- method:

  Method to use for contaminant RNA correction. Must be one of "decontx"
  (using decontX function from the celda package) and "sccdc" (using
  scCDC package), case-insensitively.

- decontXArgs:

  Arguments to pass to decontX function from the celda package. See
  [`?celda::decontX`](https://rdrr.io/pkg/celda/man/decontX.html) for
  details.

- scCDCArgs:

  Arguments to pass to scCDC function from the scCDC package. It is a
  list with 3 elements: Detection, Quantification and Correction, which
  are lists of arguments to pass to the corresponding functions from the
  scCDC package: `scCDC::ContaminationDetection`,
  `scCDC::ContaminationQuantification` and
  `scCDC::ContaminationCorrection`.

- keep_contam_assay:

  Whether to keep the `Contaminated` assay (the original counts before
  contamination correction) in the object. If `FALSE` (default), the
  assay is dropped right after the correction to save memory.

- log:

  Logger

## Value

A Seurat object with contaminant RNA corrected counts in the "RNA"
assay, the original counts in the "Contaminated" assay (unless
`keep_contam_assay` is `FALSE`), and the tool used in
`@misc$contamination$tool`. For scCDC, the detected GCGs and
contamination ratios are recorded in `@misc$contamination` as well.
