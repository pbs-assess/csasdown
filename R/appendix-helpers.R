#' Set the current appendix letter
#'
#' @description
#' Sets the current appendix context for automatic figure and table numbering.
#' This function stores the appendix letter in a global option so that
#' [appendix_fig_opts()] and [appendix_tab_opts()] can automatically use it.
#'
#' @param letter Character. The appendix letter (e.g., "A", "B", "C")
#'
#' @return Invisible NULL
#' @export
#'
#' @examples
#' \dontrun{
#' # At the start of Appendix A
#' set_appendix("A")
#'
#' # At the start of Appendix B
#' set_appendix("B")
#' }
set_appendix <- function(letter) {
  if (!is.character(letter) || length(letter) != 1) {
    cli::cli_abort("{.arg letter} must be a single character string")
  }
  options(csasdown2_current_appendix = letter)
  invisible()
}

#' Set appendix figure chunk options
#'
#' @description
#' Sets chunk options for appendix figures with automatic numbering (e.g., A.1, A.2).
#' This should be called at the start of each appendix in a setup chunk to configure
#' figure numbering for that appendix.
#'
#' @param appendix_letter Character. The appendix letter (e.g., "A", "B"). If NULL,
#'   uses the value set by [set_appendix()]
#'
#' @return Invisible NULL (sets chunk options as a side effect)
#' @export
#'
#' @examples
#' \dontrun{
#' # In a setup chunk at the start of Appendix A
#' set_appendix("A")
#' appendix_fig_opts()
#'
#' # Or specify the letter directly
#' appendix_fig_opts("B")
#' }
appendix_fig_opts <- function(appendix_letter = NULL) {
  if (is.null(appendix_letter)) {
    appendix_letter <- getOption("csasdown2_current_appendix")
    if (is.null(appendix_letter)) {
      cli::cli_abort(
        c("No appendix letter specified",
          "i" = "Either provide {.arg appendix_letter} or call {.fn set_appendix} first")
      )
    }
  }

  # Store the current appendix letter for use by individual chunks
  options(csasdown2_current_appendix = appendix_letter)

  # Set default chunk options for figures in this appendix
  knitr::opts_chunk$set(
    fig.cap.pre = paste0("Figure ", appendix_letter, "."),
    fig.lp = paste0("fig:", appendix_letter),
    fig.autonum.start_at = 1
  )

  invisible()
}

#' Set appendix table chunk options
#'
#' @description
#' Sets chunk options for appendix tables with automatic numbering (e.g., A.1, A.2).
#' This should be called at the start of each appendix in a setup chunk to configure
#' table numbering for that appendix.
#'
#' @param appendix_letter Character. The appendix letter (e.g., "A", "B"). If NULL,
#'   uses the value set by [set_appendix()]
#'
#' @return Invisible NULL (sets chunk options as a side effect)
#' @export
#'
#' @examples
#' \dontrun{
#' # In a setup chunk at the start of Appendix A
#' set_appendix("A")
#' appendix_tab_opts()
#'
#' # Or specify the letter directly
#' appendix_tab_opts("B")
#' }
appendix_tab_opts <- function(appendix_letter = NULL) {
  if (is.null(appendix_letter)) {
    appendix_letter <- getOption("csasdown2_current_appendix")
    if (is.null(appendix_letter)) {
      cli::cli_abort(
        c("No appendix letter specified",
          "i" = "Either provide {.arg appendix_letter} or call {.fn set_appendix} first")
      )
    }
  }

  # Set default chunk options for tables in this appendix
  knitr::opts_chunk$set(
    tab.cap.pre = paste0("Table ", appendix_letter, "."),
    tab.lp = paste0("tab:", appendix_letter),
    tab.autonum.start_at = 1
  )

  invisible()
}

#' Create appendix table caption
#'
#' @description
#' Helper function to create a properly formatted table caption for appendices.
#' This is useful when using `flextable::set_caption()` to ensure consistent
#' formatting with automatic numbering.
#'
#' @param caption Character. The table caption text
#' @param appendix_letter Character. The appendix letter (e.g., "A", "B"). If NULL,
#'   uses the value set by [set_appendix()]
#'
#' @return Character. A formatted caption string
#' @export
#'
#' @examples
#' \dontrun{
#' # Using set_appendix() first
#' set_appendix("A")
#' flextable(df) %>%
#'   set_caption(appendix_tab_cap("Summary statistics"))
#'
#' # Or specify the letter directly
#' flextable(df) %>%
#'   set_caption(appendix_tab_cap("Model parameters", "B"))
#' }
appendix_tab_cap <- function(caption, appendix_letter = NULL) {
  if (is.null(appendix_letter)) {
    appendix_letter <- getOption("csasdown2_current_appendix")
    if (is.null(appendix_letter)) {
      cli::cli_abort(
        c("No appendix letter specified",
          "i" = "Either provide {.arg appendix_letter} or call {.fn set_appendix} first")
      )
    }
  }

  # For now, just return the caption as-is
  # The numbering will be handled by chunk options
  caption
}
