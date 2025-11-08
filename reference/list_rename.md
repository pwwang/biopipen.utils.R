# Rename to names of a list

Rename to names of a list

## Usage

``` r
list_rename(x, fn)
```

## Arguments

- x:

  A list

- fn:

  A function to rename the names. If the function returns NULL or FALSE,
  the name will be removed. If the function returns TRUE, the name will
  be kept. If the function returns a string, the name will be changed to
  the string. The function can take 1 or 2 arguments, the first is the
  name, the second is the value.

## Value

The list with renamed names
