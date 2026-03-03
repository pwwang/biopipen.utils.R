# Get the height or width of the plot by given default size and user-specified size

Get the height or width of the plot by given default size and
user-specified size

## Usage

``` r
get_worh(default_size, user_size)
```

## Arguments

- default_size:

  The default size of the plot

- user_size:

  The user-specified size of the plot, can be NULL, a single number, or
  "+", "-", "\*", "/" followed by a number to indicate relative size to
  the default size.

## Value

The calculated size of the plot

## Examples

``` r
if (FALSE) { # \dontrun{
get_worh(5, NULL)  # returns 5
get_worh(5, 10)    # returns 10
get_worh(5, "+2")  # returns 7
get_worh(5, "-1")  # returns 4
get_worh(5, "*2")  # returns 10
get_worh(5, "/2")  # returns 2.5
} # }
```
