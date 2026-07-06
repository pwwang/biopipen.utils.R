# Read and write objects to/from files

Read and write objects to/from files

Alias of read_obj

Save an object to a file

Alias of save_obj

## Usage

``` r
read_obj(file, type = c("auto", "qs2", "rds", "h5seurat", "h5ad"), ...)

load_obj(file, type = c("auto", "qs2", "rds", "h5seurat", "h5ad"), ...)

save_obj(obj, file, type = c("auto", "qs2", "rds", "h5seurat", "h5ad"), ...)

write_obj(obj, file, type = c("auto", "qs2", "rds", "h5seurat", "h5ad"), ...)
```

## Arguments

- file:

  The file to save the object to

- type:

  The type of the file. Can be "auto", "qs2", "rds", "h5seurat" or
  "h5ad". If "auto", the type will be inferred from the file extension.

- ...:

  Additional arguments passed to the underlying save functions.

- obj:

  The object to save

## Value

The object read from the file or NULL if writing
