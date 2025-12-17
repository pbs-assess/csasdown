#' Get pandoc highlighting argument based on version
#'
#' Pandoc 3.8+ uses --syntax-highlighting=none, older versions use --no-highlight
#' @return Character string with the appropriate flag
#' @keywords internal
#' @noRd
get_pandoc_highlight_arg <- function() {
  pandoc_ver <- rmarkdown::pandoc_version()
  if (is.null(pandoc_ver)) {
    return("--no-highlight")
  }

  if (pandoc_ver >= "3.8") {
    "--syntax-highlighting=none"
  } else {
    "--no-highlight"
  }
}
