# Perform gene QC

Perform gene QC

## Usage

``` r
PerformGeneQC(object, gene_qc)
```

## Arguments

- object:

  Seurat object of a single sample.

- gene_qc:

  Gene QC criteria

## Value

A data frame with Sample (`object@project.name`), Count (number of cells
a gene is expressed in) and QC (whether the gene passes the QC)
