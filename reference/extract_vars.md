# Extract variables from a named list

Extract variables from a named list

## Usage

``` r
extract_vars(
  x,
  ...,
  keep = FALSE,
  allow_nonexisting = FALSE,
  env = parent.frame()
)
```

## Arguments

- x:

  A named list

- ...:

  The names of the variables named arguments are allowed to rename the
  variables. `b = "a"` will extract `a` and assign it to `b`

- keep:

  Keep the extracted variables in the list

- allow_nonexisting:

  Allow non-existing variables, if `TRUE`, it will not throw an error if
  the variable is not found If `FALSE`, it will throw an error if the
  variable is not found. The non-existing variables will be extracted as
  `NULL`.

- env:

  The environment to assign the extracted variables

## Value

The list with/ithout the extracted variables
