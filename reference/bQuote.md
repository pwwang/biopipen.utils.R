# backtick quoting

backtick quoting

## Usage

``` r
bQuote(x, force = FALSE)
```

## Arguments

- x:

  a character vector

- force:

  whether to force backtick quoting

## Value

backtick quoted character vector

## Examples

``` r
bQuote(c("a", "b", "c"))
#> [1] "a" "b" "c"
# [1] "a" "b" "c"
bQuote(c("a 1", "b 2", "c 3"))
#> [1] "`a 1`" "`b 2`" "`c 3`"
# [1] "`a 1`" "`b 2`" "`c 3`"
```
