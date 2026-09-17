# Normalize the marker directions

Map the direction aliases (case-insensitive `pos`/`+` and `neg`/`-`) to
`positive`/`negative`, and error on any other value.

## Usage

``` r
normalize_marker_direction(x)
```

## Arguments

- x:

  A vector of direction values.

## Value

A character vector of `positive`/`negative` values.
