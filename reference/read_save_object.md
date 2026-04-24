# Read and write objects to/from files

Read and write objects to/from files

Alias of read_obj

Save an object to a file

Alias of save_obj

## Usage

``` r
read_obj(file, type = c("auto", "qs2", "rds", "h5seurat"))

load_obj(file, type = c("auto", "qs2", "rds", "h5seurat"))

save_obj(obj, file, type = c("auto", "qs2", "rds", "h5seurat"))

write_obj(obj, file, type = c("auto", "qs2", "rds", "h5seurat"))
```

## Arguments

- file:

  The file to read or write

- type:

  The type of the file. Can be "auto", "qs2", "rds" or "h5seurat". If
  "auto", the type will be inferred from the file extension.

- obj:

  The object to save

## Value

The object read from the file or NULL if writing
