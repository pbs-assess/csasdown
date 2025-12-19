#' Create .docx CSAS-formatted documents
#'
#' @description This is a function called within the YAML of the
#' `index.Rmd` file to specify the creation of a .docx version of a
#' Research Document or Science Response.
#'
#' @param ... Other arguments to [officedown::rdocx_document()]
#' @import bookdown
#' @rdname csas_docx
#' @return A Word Document in the `.docx` format based on the CSAS Res Doc
#'   template
#' @export
#' @importFrom officer headers_replace_text_at_bkm body_replace_text_at_bkm
#'   footers_replace_text_at_bkm docx_set_settings body_add_docx

sr_docx <- function(...) {
  dots <- list(...)

  # Extract and merge pandoc_args
  default_pandoc_args <- c(get_pandoc_highlight_arg(), "--metadata", "link-citations=true", "--csl", "csl/csas.csl")
  user_pandoc_args <- dots$pandoc_args
  pandoc_args <- c(default_pandoc_args, user_pandoc_args)
  dots$pandoc_args <- NULL

  file <- "SRR-RS2026-eng-main.docx"

  args <- c(
    dots,
    list(
      base_format = "bookdown::word_document2",
      number_sections = FALSE,
      pandoc_args = pandoc_args,
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
    reference_docx = system.file("csas-docx",
      file,
      package = "csasdown2"
    ))
  )

  base <- do.call(officedown::rdocx_document, args)

  base$knitr$opts_chunk$fig.align <- "center"
  base$knitr$opts_chunk$ft.align <- "center"
  base$knitr$opts_chunk$collapse <- TRUE
  base$knitr$opts_chunk$warning <- FALSE
  base$knitr$opts_chunk$message <- FALSE
  base$knitr$opts_chunk$echo <- FALSE
  base$knitr$opts_chunk$comment <- "#>"
  base$knitr$opts_chunk$dev <- "png"

  # Add post-processor to fix table caption alignment
  base <- add_caption_fix_postprocessor(base, reference_docx = "resdoc-content-2026.docx")

  base
}

add_sr_end_matter <- function(index_fn, yaml_fn = "_bookdown.yml", verbose = FALSE, keep_files = FALSE) {

  x <- rmarkdown::yaml_front_matter(index_fn)

  french <- isTRUE(x$output[[1]]$french)
  if (french) cli::cli_abort("French SRs not implemented yet.")

  book_filename <- paste0("_book/", get_book_filename(yaml_fn), ".docx")

  french_title <- x$french_title
  english_title <- x$english_title
  english_short_title <- x$english_title
  region <- x$name_of_region
  address <- x$address
  email <- x$email

  end <- officer::read_docx(system.file("csas-docx", "SRR-RS2026-eng-end.docx", package = "csasdown2")) |>
    body_replace_text_at_bkm("english_name_of_region", x$english_region) |>
    body_replace_text_at_bkm("email", x$email) |>
    body_replace_text_at_bkm("english_address", x$english_csa_address) |>

    replace_bookmark_with_markdown("english_title", x$english_title) |>
    body_replace_text_at_bkm("english_year", x$year) |>
    body_replace_text_at_bkm("english_year2", x$year) |>

    replace_bookmark_with_markdown("french_title", x$french_title) |>
    body_replace_text_at_bkm("french_year", x$year) |>
    body_replace_text_at_bkm("french_year2", x$year) |>

    docx_set_settings(even_and_odd_headers = FALSE)
  print(end, target = "tmp-end.docx")

  full_doc <- officer::read_docx(book_filename) |>
    body_add_docx("tmp-end.docx") |>
    headers_replace_text_at_bkm("english_region", x$english_region) |>
    headers_replace_text_at_bkm("english_region_rest", x$english_region) |>
    headers_replace_text_at_bkm("english_short_title", x$english_title_short) |>
    headers_replace_text_at_bkm("year", x$year) |>
    docx_set_settings(even_and_odd_headers = FALSE)

  print(full_doc, target = book_filename)

  fix_table_caption_alignment(book_filename, reference_docx = "SRR-RS2026-eng-main.docx")

  if (!keep_files) {
    unlink(c(
      "tmp-end.docx"
    ))
  }

  invisible()
}
