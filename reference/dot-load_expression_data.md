# Load expression data from a sample path

Handles ParseBio, HIVE, 10X directory, loom, and h5 formats.

## Usage

``` r
.load_expression_data(path, sam, tmpdir, LoadLoomArgs, log)
```

## Arguments

- path:

  Path to the sample data

- sam:

  Sample name

- tmpdir:

  Temporary directory for symlink workaround

- LoadLoomArgs:

  Arguments for SeuratDisk::LoadLoom

- log:

  Logger

## Value

A list with `exprs` (matrix or Seurat object) and `cell_meta`
(data.frame or NULL)
