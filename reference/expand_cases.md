# Expand the cases with default values

Expand the cases with default values

## Usage

``` r
expand_cases(cases, defaults, post = NULL, default_case = "DEFAULT")
```

## Arguments

- cases:

  A list of cases

- defaults:

  A list of default values

- post:

  A function for post-handling each case, returning the a list with the
  name and the case. One can also return multiple cases based on the
  demand. All cases returned will be finally merged.

- default_case:

  The name of the default case. If a function is provided, it will take
  the defaults as argument and return the name of the default case.

## Value

A list of expanded cases
