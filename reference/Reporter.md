# Add and save report

Add report to a report.json to pipen-report to build a report

## Public fields

- `report`:

  The report

## Methods

### Public methods

- [`Reporter$new()`](#method-Reporter-new)

- [`Reporter$add()`](#method-Reporter-add)

- [`Reporter$add2()`](#method-Reporter-add2)

- [`Reporter$image()`](#method-Reporter-image)

- [`Reporter$clear()`](#method-Reporter-clear)

- [`Reporter$save()`](#method-Reporter-save)

- [`Reporter$clone()`](#method-Reporter-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize the reporter

#### Usage

    Reporter$new()

------------------------------------------------------------------------

### Method `add()`

Add a content to the report

#### Usage

    Reporter$add(..., h1, h2 = "#", h3 = "#", ui = "flat")

#### Arguments

- `...`:

  The content to add

- `h1`:

  The first level heading of the report

- `h2`:

  The second level heading of the report

- `h3`:

  The third level heading of the report

- `ui`:

  The user interface of the report

------------------------------------------------------------------------

### Method `add2()`

Add a content to the report, but infer the headings from headings and
sub-headings

#### Usage

    Reporter$add2(..., hs, hs2 = c(), ui = "flat", collapse = ": ")

#### Arguments

- `...`:

  The content to add

- `hs`:

  The headings of the case

- `hs2`:

  The headings that must be shown. When there are more items in `hs`,
  they will be concatenated. For example, if
  `hs = c("Section1", "Case1")`, and `hs2 = c("A", "B")`, then headings
  will be `h1 = "Section1: Case1"` and `h2 = "A"` and `h3 = "B"`.

- `ui`:

  The user interface of the report

- `collapse`:

  The separator to concatenate the headings

------------------------------------------------------------------------

### Method [`image()`](https://rdrr.io/r/graphics/image.html)

Generate a report for an image to be added

#### Usage

    Reporter$image(prefix, more_formats, save_code, kind = "image", ...)

#### Arguments

- `prefix`:

  The prefix of the image

- `more_formats`:

  More formats of the image available

- `save_code`:

  Whether to save the code to reproduce the plot

- `kind`:

  The kind of the report, default is "image"

- `...`:

  Other arguments to add to the report

#### Returns

a list of the report for the image

#### Examples

    reporter <- get_reporter()
    reporter$add(
      list(
         name = "Image 1",
         contents = list(reporter$image("/path/to/image1", "pdf", save_code = TRUE))
      ),
      h1 = "Images",
      h2 = "Image 1"
    )

------------------------------------------------------------------------

### Method `clear()`

Clear the report

#### Usage

    Reporter$clear()

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

Save the report to a file

#### Usage

    Reporter$save(path, clear = TRUE)

#### Arguments

- `path`:

  The path to save the report If the path is a directory, the report
  will be saved as `report.json` in the directory Otherwise, the report
  will be saved to the path

- `clear`:

  Whether to clear the report after saving

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Reporter$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## ------------------------------------------------
## Method `Reporter$image`
## ------------------------------------------------

reporter <- get_reporter()
reporter$add(
  list(
     name = "Image 1",
     contents = list(reporter$image("/path/to/image1", "pdf", save_code = TRUE))
  ),
  h1 = "Images",
  h2 = "Image 1"
)
```
