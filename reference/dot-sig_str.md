# Generate a string representation of an R object for caching signature

This function generates a string representation of an R object using
[`utils::str()`](https://rdrr.io/r/utils/str.html). The output is
designed to be consistent and comprehensive for caching purposes. It
captures the structure of the object, including its attributes, and is
used to create a unique signature for caching.

## Usage

``` r
.sig_str(object)
```

## Arguments

- object:

  The R object to generate the string representation for

## Value

A character vector representing the structure of the object
