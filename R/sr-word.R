#' Create .docx CSAS-formatted documents
#'
#' @description This is a function called within the YAML of the
#' `index.Rmd` file to specify the creation of a .docx version of a
#' CSAS report.
#'
#' @param ... Other arguments to [officedown::rdocx_document()]
#' @rdname csas_docx
#' @export
#' @importFrom officer headers_replace_text_at_bkm body_replace_text_at_bkm
#'   footers_replace_text_at_bkm docx_set_settings body_add_docx
#' @return A `.docx` file

sr_docx <- function(...) {
  .csasdown_docx_base(
    reference_docx = "SRR-RS2026-eng-main.docx",
    link_citations = TRUE,
    template_dir = "csas-docx",
    use_pandoc_highlight = TRUE,
    ...
  )
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
