#' Create .docx Tech Reports
#'
#' @description This is a function called within the YAML of the
#' `index.Rmd` file to specify the creation of a .docx version of a
#' Tech Report
#'
#' @param ... Other arguments to [officedown::rdocx_document()]
#' @import bookdown
#' @rdname techreport_docx
#' @return A Word Document in the `.docx` format based on the Tech Report
#'   template
#' @export

techreport_docx <- function(...) {

  file <- "tech-report-content.docx"
  ref_docx <- system.file("tech-report-docx",
    file,
    package = "csasdown2"
  )

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
    reference_docx = ref_docx
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
  base <- add_caption_fix_postprocessor(base, reference_docx = ref_docx)

  base
}

add_techreport_word_frontmatter <- function(index_fn, yaml_fn = "_bookdown.yml", verbose = FALSE, keep_files = FALSE) {
  if (verbose) cli_inform("Adding frontmatter to the Technical Report using the officer package...")

  x <- rmarkdown::yaml_front_matter(index_fn)
  french <- isTRUE(x$output[[1]]$french)

  # Get book filename from bookdown config
  book_filename <- paste0("_book/", get_book_filename(yaml_fn), ".docx")

  # Extract main content and remove placeholder title/abstract from beginning
  content <- officer::read_docx(book_filename) |>
    officer::cursor_begin() |>
    officer::body_remove() |>
    officer::body_remove() |>
    officer::body_remove()
  print(content, target = "tmp-content.docx")

  # Read the tech report cover template and replace bookmarks
  frontmatter <- officer::read_docx(system.file("tech-report-docx", "01-tech-report-cover-english.docx", package = "csasdown2")) |>
    replace_bookmark_with_markdown("title", x$english_title) |>
    replace_bookmark_with_markdown("authors", x$english_author) |>
    replace_bookmark_with_markdown("address", x$english_address) |>
    officer::body_replace_text_at_bkm("year", as.character(x$year)) |>
    officer::body_replace_text_at_bkm("number", x$report_number)

  # Save the frontmatter temporarily
  print(frontmatter, target = "tmp-frontmatter.docx")

  # Read and process the titlepage template
  # For multi-line fields, preserve backslash line breaks but collapse other newlines
  title_clean <- trimws(gsub("\n(?!\\s*$)", " ", x$english_title, perl = TRUE))
  authors_clean <- trimws(gsub("\n(?!\\s*$)", " ", x$english_author, perl = TRUE))
  # For address, keep the backslash line breaks
  address_clean <- x$english_address

  titlepage <- officer::read_docx(system.file("tech-report-docx", "03-tech-report-titlepage-english.docx", package = "csasdown2")) |>
    replace_bookmark_with_markdown("title", title_clean) |>
    replace_bookmark_with_markdown("authors", authors_clean) |>
    replace_bookmark_with_markdown("address", address_clean) |>
    replace_bookmark_with_markdown("year", as.character(x$year)) |>
    replace_bookmark_with_markdown("number", x$report_number)
  print(titlepage, target = "tmp-titlepage.docx")

  # Read and process the colophon template
  total_pages <- as.numeric(x$english_preamble_pages) + as.numeric(x$english_content_pages)

  # Build citation - ensure all components are single line, trimmed strings
  citation <- paste0(
    trimws(gsub("\n", " ", x$english_author_list)), " ",
    x$year, ". ",
    trimws(gsub("\n", " ", x$english_title)), ". ",
    "Can. Tech. Rep. Fish. Aquat. Sci. ", x$report_number, ": ",
    x$english_preamble_pages, " + ", x$english_content_pages, " p. ",
    "https://doi.org/", trimws(x$english_doi)
  )

  colophon <- officer::read_docx(system.file("tech-report-docx", "04-tech-report-colophon-english.docx", package = "csasdown2")) |>
    officer::body_replace_text_at_bkm("year", as.character(x$year)) |>
    officer::body_replace_text_at_bkm("cat_no_english", x$english_cat_no) |>
    officer::body_replace_text_at_bkm("isbn_english", x$english_isbn) |>
    officer::body_replace_text_at_bkm("doi", x$english_doi) |>
    officer::body_replace_text_at_bkm("doi_english", x$english_doi) |>
    replace_bookmark_with_markdown("citation_english", citation)
  print(colophon, target = "tmp-colophon.docx")

  # Merge frontmatter and content with page breaks between sections
  doc <- officer::read_docx("tmp-frontmatter.docx") |>
    officer::body_add_break(pos = "after") |>
    officer::body_add_docx("tmp-titlepage.docx") |>
    officer::body_add_break(pos = "after") |>
    officer::body_add_docx("tmp-colophon.docx") |>
    officer::body_add_break(pos = "after") |>
    officer::body_add_docx("tmp-content.docx") |>
    officer::body_set_default_section(
      officer::prop_section(
        page_size = officer::page_size(orient = "portrait", width = 8.5, height = 11),
        page_margins = officer::page_mar(
          bottom = 1,
          top = 1,
          right = 1,
          left = 1,
          header = 0.5,
          footer = 0.5
        )
      )
    )

  # Save final document
  print(doc, target = book_filename)

  # Cleanup temporary files
  if (!keep_files) {
    unlink(c("tmp-frontmatter.docx", "tmp-titlepage.docx", "tmp-colophon.docx", "tmp-content.docx"))
  }

  invisible()
}

