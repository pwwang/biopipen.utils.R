# The string representation of an object

The string representation of an object

## Usage

``` r
repr(x, newline = FALSE, ...)
```

## Arguments

- x:

  An object

- newline:

  Whether to add newlines to the output for each element

- ...:

  Other arguments passed to the `.repr` API.

## Value

The string representation

## Examples

``` r
repr(1:10)
#> [1] "c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)"
# [1] "c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)"
```
