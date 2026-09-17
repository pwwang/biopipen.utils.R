# Convert a marker table to the singscore format

A named list of `up`/`down` gene sets, one per cell type, for
[`singscore::simpleScore()`](https://DavisLaboratory.github.io/singscore/reference/simpleScore.html).

## Usage

``` r
markers_to_singscore_list(df)
```

## Arguments

- df:

  A canonical marker table.

## Value

A named list of `list(up = <genes>, down = <genes>)`, named by the cell
types.

## Details

The direction is native here, so both marker directions are kept:
positive-direction rows become `up` and negative-direction rows become
`down`.
