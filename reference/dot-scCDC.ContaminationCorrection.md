# Perform the contamination correction

Decontaminate the count matrix of the input SeuratObject based on the
input contaminative genes using a Youden index-based method.

## Usage

``` r
.scCDC.ContaminationCorrection(
  object,
  cont_genes,
  auc_thres = 0.9,
  min.cell = 50
)
```

## Arguments

- object:

  a clustered SeuratObject

- cont_genes:

  a contaminative geneset within the input SeuratObject

- auc_thres:

  the AUROC threshold to determine the boundary between eGCG_positive
  and eGCG_negative clusters (Default as 0.9, 90 percent)

- min.cell:

  the parameter used to filter the cell populations without sufficient
  number of cells. Cell populations that reaches the threshold could be
  used in downstream analysis.

## Value

the input clustered SeuratObject with a additional corrected assay of
counts

## Details

Patched version for CreateAssay5Object not found error in scCDC package.
