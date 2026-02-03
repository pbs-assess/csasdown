#' Start a new appendix with auto-incremented letter
#'
#' @description
#' Automatically increments to the next appendix letter (A → B → C, etc.) and
#' configures figure and table numbering for that appendix. This is the primary
#' function users should call at the start of each appendix.
#'
#' @return Character. The appendix letter that was set.
#' @export
#'
#' @examples
#' \dontrun{
#' # At the start of your first appendix
#' new_appendix()  # Sets to "A"
#'
#' # At the start of your second appendix
#' new_appendix()  # Sets to "B"
#'
#' # Use in heading: # APPENDIX `r new_appendix()`. BIOLOGICAL DATA
#' }
new_appendix <- function() {
  current <- getOption("csasdown_current_appendix", NULL)

  if (is.null(current)) {
    next_letter <- "A"
  } else {
    # Convert letter to next letter (A→B, B→C, etc.)
    current_num <- utf8ToInt(current) - utf8ToInt("A") + 1
    if (current_num >= 26) {
      cli::cli_abort("Cannot create more than 26 appendices (A-Z)")
    }
    next_letter <- LETTERS[current_num + 1]
  }

  set_appendix(next_letter)
  next_letter
}

#' Get the current appendix letter
#'
#' @description
#' Returns the current appendix letter. Useful for including in appendix headings
#' to ensure the heading matches the automatic numbering.
#'
#' @return Character. The current appendix letter, or "A" if none has been set.
#' @export
#'
#' @examples
#' \dontrun{
#' new_appendix()
#' current_appendix()  # Returns "A"
#'
#' # Use in heading: # APPENDIX `r current_appendix()`. TITLE HERE
#' }
current_appendix <- function() {
  getOption("csasdown_current_appendix", "A")
}

#' Set the current appendix letter
#'
#' @description
#' Sets the current appendix context for automatic figure and table numbering.
#' This function stores the appendix letter and automatically configures both
#' figure and table numbering. For most users, [new_appendix()] is preferred
#' as it auto-increments. Use this function when you need to explicitly set
#' a specific appendix letter.
#'
#' @param letter Character. The appendix letter (e.g., "A", "B", "C")
#'
#' @return Invisible NULL
#' @export
#'
#' @examples
#' \dontrun{
#' # Explicitly set to Appendix A
#' set_appendix("A")
#'
#' # Explicitly set to Appendix B
#' set_appendix("B")
#' }
set_appendix <- function(letter) {
  if (!is.character(letter) || length(letter) != 1) {
    cli::cli_abort("{.arg letter} must be a single character string")
  }
  options(csasdown_current_appendix = letter)

  # Automatically configure both figure and table numbering
  appendix_fig_opts(letter)
  appendix_tab_opts(letter)

  invisible()
}

#' Set appendix figure chunk options
#'
#' @description
#' Internal function to set chunk options for appendix figures with automatic
#' numbering (e.g., A.1, A.2). This is called automatically by [set_appendix()]
#' and [new_appendix()]. Users should not need to call this directly.
#'
#' @param appendix_letter Character. The appendix letter (e.g., "A", "B"). If NULL,
#'   uses the value set by [set_appendix()]
#'
#' @return Invisible NULL (sets chunk options as a side effect)
#' @keywords internal
appendix_fig_opts <- function(appendix_letter = NULL) {
  if (is.null(appendix_letter)) {
    appendix_letter <- getOption("csasdown_current_appendix")
    if (is.null(appendix_letter)) {
      cli::cli_abort(
        c("No appendix letter specified",
          "i" = "Either provide {.arg appendix_letter} or call {.fn set_appendix} first")
      )
    }
  }

  # Store the current appendix letter for use by individual chunks
  options(csasdown_current_appendix = appendix_letter)

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
#' Internal function to set chunk options for appendix tables with automatic
#' numbering (e.g., A.1, A.2). This is called automatically by [set_appendix()]
#' and [new_appendix()]. Users should not need to call this directly.
#'
#' @param appendix_letter Character. The appendix letter (e.g., "A", "B"). If NULL,
#'   uses the value set by [set_appendix()]
#'
#' @return Invisible NULL (sets chunk options as a side effect)
#' @keywords internal
appendix_tab_opts <- function(appendix_letter = NULL) {
  if (is.null(appendix_letter)) {
    appendix_letter <- getOption("csasdown_current_appendix")
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
#'   uses the value set by [set_appendix()] or [new_appendix()]
#'
#' @return Character. A formatted caption string
#' @export
#'
#' @examples
#' \dontrun{
#' # Using new_appendix() first
#' new_appendix()
#' flextable(df) %>%
#'   set_caption(appendix_table_caption("Summary statistics"))
#'
#' # Or with set_appendix()
#' set_appendix("A")
#' flextable(df) %>%
#'   set_caption(appendix_table_caption("Model parameters"))
#' }
appendix_table_caption <- function(caption, appendix_letter = NULL) {
  if (is.null(appendix_letter)) {
    appendix_letter <- getOption("csasdown_current_appendix")
    if (is.null(appendix_letter)) {
      cli::cli_abort(
        c("No appendix letter specified",
          "i" = "Either provide {.arg appendix_letter} or call {.fn set_appendix} or {.fn new_appendix} first")
      )
    }
  }

  # For now, just return the caption as-is
  # The numbering will be handled by chunk options
  caption
}
