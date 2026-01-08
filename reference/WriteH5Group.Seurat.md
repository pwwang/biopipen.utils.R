# Patch to fix SeuratDisk::WriteH5Group for Assay objects SeuratObject 5.3.0 raises an error with SeuratDisk::WriteH5Group() for Assay objects

Patch to fix SeuratDisk::WriteH5Group for Assay objects SeuratObject
5.3.0 raises an error with SeuratDisk::WriteH5Group() for Assay objects

## Usage

``` r
WriteH5Group.Seurat(x, name, hgroup, verbose = TRUE)
```

## Arguments

- x:

  Assay object

- name:

  Name of the group to create

- hgroup:

  HDF5 group to write to

- verbose:

  Whether to print messages

## Value

Invisibly returns `NULL`
