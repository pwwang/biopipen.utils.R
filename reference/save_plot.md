# Save the plot into multiple formats

Save the plot into multiple formats

## Usage

``` r
save_plot(
  plot,
  prefix,
  devpars = NULL,
  bg = "white",
  formats = c("png", "pdf")
)
```

## Arguments

- plot:

  The plot object

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

  The formats to save
