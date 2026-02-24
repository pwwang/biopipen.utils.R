# Save the plot into multiple formats

Save the plot into multiple formats

## Usage

``` r
save_plot(
  plot,
  prefix,
  devpars = NULL,
  bg = "white",
  formats = c("png", "pdf"),
  selfcontained = TRUE
)
```

## Arguments

- plot:

  The plot object. Can be a ggplot2 object or a plotly/htmlwidget
  object. For plotly/htmlwidget objects, only `"html"` format is
  supported. If static formats are requested, a warning is issued and an
  HTML file is saved instead. For ggplot2 objects, `"html"` is also
  supported by converting the plot to a plotly object via
  [`plotly::ggplotly`](https://rdrr.io/pkg/plotly/man/ggplotly.html)
  first.

- prefix:

  The prefix of the file The ending `.` is removed from the prefix. So
  both `/path/to/file` and `/path/to/file.` are valid and will save the
  files as `/path/to/file.png`, `/path/to/file.pdf` and
  `/path/to/file.<format>` etc.

- devpars:

  The device parameters

- bg:

  The background color

- formats:

  The formats to save. Use `"html"` to save as an interactive HTML file.

- selfcontained:

  Whether to save the HTML file as a self-contained file (only used when
  saving as HTML). Default is `TRUE`.
