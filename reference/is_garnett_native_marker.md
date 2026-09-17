# Detect a garnett-native marker file

A garnett-native marker file has blocks headed by `> <cell type>`. The
universal binary formats (rds/qs/qs2 data.frame, xlsx/xls) and a missing
file are never native. Text files are decided by content: the first
non-blank, non-`#` line of a native file is a `>` header.

## Usage

``` r
is_garnett_native_marker(path)
```

## Arguments

- path:

  The path of the marker file.

## Value

`TRUE` if the file is garnett-native, `FALSE` otherwise.
