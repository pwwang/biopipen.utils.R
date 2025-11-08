# Cache class for object, file or directory caching

A class to handle caching of objects, files, or directories. It
generates a signature from the provided object and creates cached
versions based on that signature.

## Public fields

- `cache_dir`:

  Directory where cached files are stored

- `source`:

  path to the original file/directory/prefix

## Methods

### Public methods

- [`Cache$new()`](#method-Cache-new)

- [`Cache$clear()`](#method-Cache-clear)

- [`Cache$clear_all()`](#method-Cache-clear_all)

- [`Cache$restore()`](#method-Cache-restore)

- [`Cache$is_cached()`](#method-Cache-is_cached)

- [`Cache$get_path()`](#method-Cache-get_path)

- [`Cache$save()`](#method-Cache-save)

- [`Cache$clone()`](#method-Cache-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new Cache object

#### Usage

    Cache$new(
      sig_object,
      prefix,
      cache_dir,
      save_sig = FALSE,
      kind = c("object", "file", "dir", "prefix"),
      path = NULL
    )

#### Arguments

- `sig_object`:

  Object used to generate the cache signature

- `prefix`:

  Prefix for the cache filename

- `cache_dir`:

  Directory where cached files are stored

- `save_sig`:

  Whether to save the signature to a file

- `kind`:

  Type of cache: "object", "file", "dir" or "prefix"

  - object: cache an R object

  - file: cache a file

  - dir: cache a directory

  - prefix: cache files/directories with the same prefix

- `path`:

  Path to the file or directory to cache This is required when `kind` is
  "file", "dir", or "prefix"

#### Details

The `sig_object` is used to generate a unique signature for the cache.
The signature is based on the structure of the object, which helps in
determining if the cached version is still valid. The `prefix` is used
to create a unique identifier for the cached files. The `cache_dir` is
the directory where the cached files will be stored. If `save_sig` is
TRUE, the signature will be saved to a file in the cache directory. The
`kind` parameter determines how the cache will be handled:

- "object": The cache will store an R object.

- "file": The cache will store a file.

- "dir": The cache will store a directory.

- "prefix": The cache will store files/directories with the same prefix.
  The `path` parameter is required when `kind` is "file", "dir", or
  "prefix". It specifies the path to the file or directory to cache. If
  `cache_dir` is NULL or an empty string, caching will not be performed.
  If `cache_dir` is FALSE, caching will not be performed.

#### Returns

A new Cache object

#### Examples

    \dontrun{
    # Create a Cache object for an R object
    cache <- Cache$new(sig_object = mtcars, prefix = "mtcars_cache",
                       cache_dir = tempdir(), save_sig = TRUE, kind = "object")
    # Save the object to cache
    cache$save(mtcars)
    # Restore the cached object
    cached_mtcars <- cache$restore()
    # Check if the object is cached
    cache$is_cached()  # Should return TRUE
    # Clear the cache
    cache$clear()
    # Clear all cached objects in the cache directory
    cache$clear_all()
    }

------------------------------------------------------------------------

### Method `clear()`

Clear the current cached object/file/directory

#### Usage

    Cache$clear()

------------------------------------------------------------------------

### Method `clear_all()`

Clear all cached objects in the cache directory Be careful with this
operation as it will remove all cached files in the cache directory

#### Usage

    Cache$clear_all()

------------------------------------------------------------------------

### Method `restore()`

Retrieve the cached object/file/directory When NULL is returned, it
means the cache does not exist

#### Usage

    Cache$restore()

#### Returns

The cached object if kind is "object", otherwise NULL

------------------------------------------------------------------------

### Method `is_cached()`

Check if we have a cached object/file/directory

#### Usage

    Cache$is_cached()

#### Returns

TRUE if the cached object/file/directory exists, FALSE otherwise

------------------------------------------------------------------------

### Method `get_path()`

Get the path to the cached object/file/directory

#### Usage

    Cache$get_path()

#### Returns

The path to the cached object/file/directory

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

Save an object/file/directory/prefix to cache

#### Usage

    Cache$save(data = NULL)

#### Arguments

- `data`:

  The object to cache, or NULL for non-"object" kinds

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Cache$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## ------------------------------------------------
## Method `Cache$new`
## ------------------------------------------------

if (FALSE) { # \dontrun{
# Create a Cache object for an R object
cache <- Cache$new(sig_object = mtcars, prefix = "mtcars_cache",
                   cache_dir = tempdir(), save_sig = TRUE, kind = "object")
# Save the object to cache
cache$save(mtcars)
# Restore the cached object
cached_mtcars <- cache$restore()
# Check if the object is cached
cache$is_cached()  # Should return TRUE
# Clear the cache
cache$clear()
# Clear all cached objects in the cache directory
cache$clear_all()
} # }
```
