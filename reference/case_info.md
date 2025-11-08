# Create information for a casename

Create information for a casename

## Usage

``` r
case_info(name, outdir, is_dir = TRUE, create = FALSE)
```

## Arguments

- name:

  A casename Sections are separated by `::` in the casename When section
  is specified, the case will be created in the section directory

- outdir:

  The output directory, where the case directory will be created or
  prefix will be under

- is_dir:

  Whether to create a directory for the case Otherwise, a prefix will be
  returned.

- create:

  Create the directory if not exists when `is_dir` is TRUE, otherwise,
  create the parent directory of the prefix.

## Value

A list of information, including:

- `name`: The name of the case, without the sections

- `section`: The sections of the case as a vector

- `slug`: The slugified name

- `section_slug`: The slugified sections

- `prefix`: The prefix of the case, either the directory or the file
  path prefix
