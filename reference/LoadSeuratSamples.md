# Load samples into a list of Seurat objects

This loads each sample without performing any QC, so that QC can be done
per sample by
[`PerformSeuratQC()`](https://pwwang.github.io/biopipen.utils.R/reference/PerformSeuratQC.md)
before the samples are merged by
[`LoadSeuratAndPerformQC()`](https://pwwang.github.io/biopipen.utils.R/reference/LoadSeuratAndPerformQC.md).

## Usage

``` r
LoadSeuratSamples(
  meta,
  min_cells = 0,
  min_features = 0,
  features = NULL,
  samples = NULL,
  LoadLoomArgs = list(),
  tmpdir = NULL,
  log = NULL
)
```

## Arguments

- meta:

  Metadata of the samples Required columns: Sample, RNAData. The RNAData
  column should contain the path to the 10X or ParseBio data, either a
  directory or a file If the path is a directory, the function will look
  for barcodes.tsv.gz, features.tsv.gz and matrix.mtx.gz. The directory
  should be loaded by
  [Seurat::Read10X](https://satijalab.org/seurat/reference/Read10X.html),
  [Seurat::ReadParseBio](https://satijalab.org/seurat/reference/ReadParseBio.html)
  or the HIVE data. Sometimes, there may be prefix in the file names,
  e.g. "'prefix'.barcodes.tsv.gz", which is also supported. If the path
  is a file ending with ".loom", it will be loaded by
  [`SeuratDisk::Connect()`](https://mojaveazure.github.io/seurat-disk/reference/Connect.html)
  and converted to a Seurat object. Otherwise, if the path is a file, it
  should be a h5 file that can be loaded by
  [`Seurat::Read10X_h5()`](https://satijalab.org/seurat/reference/Read10X_h5.html)

  This can also be a Seurat object to split into samples. It requires
  the "Sample" column in the meta.data slot specifying the sample for
  each cell.

- min_cells:

  Include features detected in at least this many cells. This will be
  applied to all samples and passed to the
  [`Seurat::CreateSeuratObject()`](https://satijalab.github.io/seurat-object/reference/CreateSeuratObject.html)
  function. QCs can be further performed on the object after loading.
  You can also provide a list of values, where the names of the list are
  sample names and the values are the minimum number of cells for each
  sample to load by
  [`Seurat::CreateSeuratObject()`](https://satijalab.github.io/seurat-object/reference/CreateSeuratObject.html).
  You can have a default value in the list with the name "DEFAULT" for
  the samples that are not listed. This won't work if data is loaded
  from a loom file or `meta` is a Seurat object.

- min_features:

  Include cells where at least this many features are detected. This
  will be applied to all samples and passed to the
  [`Seurat::CreateSeuratObject()`](https://satijalab.github.io/seurat-object/reference/CreateSeuratObject.html)
  function. QCs can be further performed on the object after loading.
  You can also provide a list of values, where the names of the list are
  sample names and the values are the minimum number of features for
  each sample to load by
  [`Seurat::CreateSeuratObject()`](https://satijalab.github.io/seurat-object/reference/CreateSeuratObject.html).
  You can have a default value in the list with the name "DEFAULT" for
  the samples that are not listed. This won't work if data is loaded
  from a loom file or `meta` is a Seurat object.

- features:

  A named character vector/list or a file path to rename features. If a
  named vector/list is given, the names are the original feature names
  and the values are the new names. If a file path is given, it should
  be a TAB-delimited file with two columns (no header); lines beginning
  with '#' are ignored. The first column contains the original feature
  names and the second column the new names.

- samples:

  Samples to load. If NULL, all samples will be loaded

- LoadLoomArgs:

  Arguments to pass to
  [`SeuratDisk::LoadLoom()`](https://mojaveazure.github.io/seurat-disk/reference/LoadLoom.html)
  when loading loom files.

- tmpdir:

  Temporary directory to store intermediate files when there are prefix
  in the file names

- log:

  Logger

## Value

A named list of Seurat objects, one per sample. Samples that have no
data or no cells are skipped with a warning.

## Examples

``` r
# \donttest{
datadir <- system.file("extdata", "scrna", package = "biopipen.utils")
meta <- data.frame(
    Sample = c("Sample1", "Sample2"),
    RNAData = c(
        file.path(datadir, "Sample1"),
        file.path(datadir, "Sample2")
    )
)

objs <- LoadSeuratSamples(meta)
#> INFO    [2026-09-19 06:11:19] - Loading Sample1 ...
#> INFO    [2026-09-19 06:11:19] - Loading Sample2 ...
names(objs)
#> [1] "Sample1" "Sample2"
# }
```
