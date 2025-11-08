# Then write the list to a group

This function writes a list to an H5Group, creating datasets for each
element. If the list contains nested lists, it will create subgroups.

## Usage

``` r
list_to_h5group(h5fg, name, lst)
```

## Arguments

- h5fg:

  An H5File or H5Group object where the group will be created

- name:

  The name of the group to create

- lst:

  The list to write to the group

## Value

The created H5Group object
