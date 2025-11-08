# Copy a directory to another location

Unlike `file.copy`, this function copies a directory and its contents to
another location, instead of copying into another location. It creates
the target directory if it does not exist.

## Usage

``` r
.dir.copy(from, to, overwrite = FALSE, copy.date = TRUE)
```

## Arguments

- from:

  Source directory path

- to:

  Target directory path

- overwrite:

  Whether to overwrite existing files in the target directory

- copy.date:

  Whether to copy the file modification date

## Value

a logical vector indicating which operation succeeded for each of the
files in `from` attempted
