.onAttach <- function(libname, pkgname) {
    fonts <- plotthis::list_fonts(family_only = TRUE)
    fonts$system <- NULL
    packageStartupMessage(
        "Listing available fonts from plotthis package:\n",
        "Builtin fonts:\n",
        paste(
            paste0(
                " - ",
                sapply(
                    split(fonts$builtin, ceiling(seq_along(fonts$builtin) / 5)),
                    function(x) paste(x, collapse = ", ")
                )
            ),
            collapse = "\n"
        ),
        "\nSystem fonts:\n",
        if (length(fonts$system) == 0) {
            " - No system fonts found"
        } else {
            paste(
                paste0(
                    " - ",
                    sapply(
                        split(fonts$system, ceiling(seq_along(fonts$system) / 5)),
                        function(x) paste(x, collapse = ", ")
                    )
                ),
                collapse = "\n"
            )
        },
        "\nGoogle fonts (need internet access):\n",
        paste(
            paste0(
                " - ",
                sapply(
                    split(fonts$google, ceiling(seq_along(fonts$google) / 5)),
                    function(x) paste(x, collapse = ", ")
                )
            ),
            collapse = "\n"
        ),
        "\n"
    )
}
