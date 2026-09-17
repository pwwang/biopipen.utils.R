# Convert a marker table to a garnett-native marker file

Write `> <cell type>` blocks with `expressed:` / `not expressed:` rules.
Negative-direction markers become `not expressed:` rules.

## Usage

``` r
markers_to_garnett_file(df, file, tissue = NULL, cancer = NULL, species = NULL)
```

## Arguments

- df:

  A canonical marker table.

- file:

  The path of the garnett marker file to write.

- tissue:

  The tissue to filter by. If NULL, no filtering is done.

- cancer:

  The cancer to filter by. If NULL, no filtering is done.

- species:

  The species to filter by. If NULL, no filtering is done.

## Value

The path of the written garnett marker file.
