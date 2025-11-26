# Convert an AnnData file (h5ad) to a Seurat object or an RDS/qs2 file

Convert an AnnData file (h5ad) to a Seurat object or an RDS/qs2 file

## Usage

``` r
ConvertAnnDataToSeurat(
  infile,
  outfile = NULL,
  assay = NULL,
  ident = NULL,
  log = NULL
)
```

## Arguments

- infile:

  Input file

- outfile:

  Output file. If "NULL" (default) is given, a temporary file will be
  created and saved and the Seurat object will be read from it.

- assay:

  Assay name to save in the Seurat object

- ident:

  The name of the identity in metadata after conversion

- log:

  Logger

## Value

The Seurat object if `outfile` is "", otherwise NULL.
