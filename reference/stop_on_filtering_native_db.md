# Stop on filtering a native (non-universal) marker database

Native marker formats have no `tissue`/`cancer`/`species` columns to
filter by, so asking for such a filter is an error. It is a no-op when
no filter is given.

## Usage

``` r
stop_on_filtering_native_db(tissue = NULL, cancer = NULL, species = NULL)
```

## Arguments

- tissue:

  The tissue filter. If not NULL, an error is raised.

- cancer:

  The cancer filter. If not NULL, an error is raised.

- species:

  The species filter. If not NULL, an error is raised.

## Value

`NULL`, invisibly.
