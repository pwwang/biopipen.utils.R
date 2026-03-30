# Slugify strings

Slugify strings

## Usage

``` r
slugify(
  x,
  non_alphanum_replace = "-",
  collapse_replace = TRUE,
  tolower = FALSE,
  strip = "both"
)
```

## Arguments

- x:

  strings to slugify

- non_alphanum_replace:

  Replace non-alphanumeric characters

- collapse_replace:

  Collapse consecutive non-alphanumeric character replacements

- tolower:

  Convert to lowercase

- strip:

  Whether to strip leading and trailing non-alphanumeric characters
  Possible values are "none", "both", "leading", "trailing". Default is
  "both".

## Value

slugified strings
