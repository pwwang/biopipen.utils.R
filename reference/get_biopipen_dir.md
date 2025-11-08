# Get the path where biopipen is installed

This is useful when the script is running in a different
environment/platform. At runtime, we need to know where the biopipen
package is installed so that we can source or use some files (or
scripts) shipped with biopipen.

## Usage

``` r
get_biopipen_dir(python = "python")
```

## Arguments

- python:

  The path to the Python executable at runtime.

## Value

The path where biopipen is installed
