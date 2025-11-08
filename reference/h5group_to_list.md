# Convert H5D or H5Group to a list

When the structure of the HDF5 file is miscellaneous, simply using
[`as.list()`](https://rdrr.io/r/base/list.html) will not work.

## Usage

``` r
h5group_to_list(h5group)
```

## Arguments

- h5group:

  An H5D or H5Group object

## Value

A list representation of the H5D or H5Group
