#' Creates an Microsoft Word CSAS-formatted document
#'
#' @description
#' This is a function called in output in the YAML of the `index.Rmd` file
#' to specify the creation of an FSAR word document.
#'
#' @param ... Other arguments to [officedown::rdocx_document()]
#' @rdname csas_docx
#' @return A Word Document in the `.docx` format based on the FSAR template.
#' @importFrom cli cli_inform cli_abort cli_warn
#' @export
fsar_docx <- function(...) {
  ## Several modifications were made to the fsar template
  ## 1) bookmarks were added to headers and footers for officer replacement below
  ## 2) created a Table Caption style since Caption - Table was not being applied. May be a bug (https://github.com/davidgohel/officedown/issues/112).
  ## 3) Added First Paragraph style to avoid issues with Body Text not being applied to the first paragraph of each section.
  file <- "fsar-template.docx"
  base <- officedown::rdocx_document(...,
    base_format = "bookdown::word_document2",
    number_sections = FALSE,
    tables = list(
      style = "Body Text",
      layout = "autofit",
      caption = list(
        style = "Table Caption",
        pre = "Table", sep = ". ",
        fp_text = officer::fp_text_lite(bold = FALSE)
      ),
      conditional = list(
        first_row = TRUE, first_column = FALSE, last_row = FALSE,
        last_column = FALSE, no_hband = FALSE, no_vband = TRUE
      )
    ),
    plots = list(
      style = "Caption - Figure",
      align = "center",
      caption = list(
        style = "Caption - Figure",
        pre = "Figure ", sep = ". ",
        fp_text = officer::fp_text_lite(bold = FALSE)
      )
    ),
    mapstyles = list(
      "Body Text" = c("Normal", "First Paragraph")
    ),
    pandoc_args = "--no-highlight",
    reference_docx = system.file("csas-docx",
      file,
      package = "csasdown2"
    )
  )

  base$knitr$opts_chunk$fig.align <- "center"
  base$knitr$opts_chunk$ft.align <- "center"
  base$knitr$opts_chunk$collapse <- TRUE
  base$knitr$opts_chunk$warning <- FALSE
  base$knitr$opts_chunk$message <- FALSE
  base$knitr$opts_chunk$echo <- FALSE
  base$knitr$opts_chunk$comment <- "#>"
  base$knitr$opts_chunk$dev <- "png"

  # Add post-processor to fix table caption alignment
  base <- add_caption_fix_postprocessor(base, reference_docx = "fsar-template.docx")

  base
}
