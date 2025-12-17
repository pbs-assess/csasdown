#' Get pandoc highlighting argument based on version
#'
#' Pandoc 3.8+ uses --syntax-highlighting, older versions use --highlight-style
#' @return Character string with the appropriate flag
#' @keywords internal
#' @noRd
get_pandoc_highlight_arg <- function() {
  pandoc_ver <- rmarkdown::pandoc_version()
  if (is.null(pandoc_ver)) {
    return("--highlight-style=none")
  }

  if (pandoc_ver >= "3.8") {
    "--syntax-highlighting=none"
  } else {
    "--highlight-style=none"
  }
}
