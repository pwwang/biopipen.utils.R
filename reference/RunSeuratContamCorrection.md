# Run contaminant RNA correction on a Seurat object

Run contaminant RNA correction on a Seurat object

## Usage

``` r
RunSeuratContamCorrection(
  object,
  method = "decontx",
  decontXArgs = list(),
  scCDCArgs = list(Detection = list(), Quantification = list(), Correction = list())
)
```

## Arguments

- object:

  Seurat object

- method:

  Method to use for contaminant RNA correction. Supported methods:
  "decontx" (using decontX function from the celda package) and "sccdc"
  (using scCDC package).

- decontXArgs:

  Arguments to pass to decontX functionn from the celda package. See
  [`?celda::decontX`](https://rdrr.io/pkg/celda/man/decontX.html) for
  details.

- scCDCArgs:

  Arguments to pass to scCDC function from the scCDC package. It is a
  list with 3 elements: Detection, Quantification and Correction, which
  are lists of arguments to pass to the corresponding functions from the
  scCDC package:
  [`scCDC::ContaminationDetection`](https://rdrr.io/pkg/scCDC/man/ContaminationDetection.html),
  [`scCDC::ContaminationQuantification`](https://rdrr.io/pkg/scCDC/man/ContaminationQuantification.html)
  and
  [`scCDC::ContaminationCorrection`](https://rdrr.io/pkg/scCDC/man/ContaminationCorrection.html).

## Value

A Seurat object with contaminant RNA corrected counts in the "RNA" assay
and the original counts in the "Contaminated" assay.
