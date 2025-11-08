# Read and write objects to/from files

Read and write objects to/from files

Alias of read_obj

Save an object to a file

Alias of save_obj

## Usage

``` r
read_obj(file, type = c("auto", "qs2", "rds"))

load_obj(file, type = c("auto", "qs2", "rds"))

save_obj(obj, file, type = c("auto", "qs2", "rds"))

write_obj(obj, file, type = c("auto", "qs2", "rds"))
```

## Arguments

- file:

  The file to read or write

- type:

  The type of the file. Can be "auto", "qs2" or "rds". If "auto", the
  type will be inferred from the file extension.

- obj:

  The object to save

## Value

The object read from the file or NULL if writing
