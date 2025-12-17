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
  dots <- list(...)

  # Extract and merge pandoc_args
  default_pandoc_args <- c("--metadata", "link-citations=true", "--csl", "csl/csas.csl")
  user_pandoc_args <- dots$pandoc_args
  pandoc_args <- c(default_pandoc_args, user_pandoc_args)
  dots$pandoc_args <- NULL

  file <- "tech-report-content.docx"
  ref_docx <- system.file("tech-report-docx",
    file,
    package = "csasdown2"
  )

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
    reference_docx = ref_docx)
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
  base <- add_caption_fix_postprocessor(base, reference_docx = ref_docx)

  base
}

add_techreport_word_frontmatter <- function(index_fn, yaml_fn = "_bookdown.yml", verbose = FALSE, keep_files = FALSE) {
  if (verbose) cli_inform("Adding frontmatter to the Technical Report using the officer package...")

  x <- rmarkdown::yaml_front_matter(index_fn)

  # Parse single author field into language-specific fields
  parsed <- parse_author_field(x$author)
  x$english_author <- parsed$english_author
  x$french_author <- parsed$french_author
  x$english_author_list <- parsed$english_author_list
  x$french_author_list <- parsed$french_author_list

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

  # Conditional field selection based on language
  title <- if (french) x$french_title else x$english_title
  author <- if (french) x$french_author else x$english_author
  address <- if (french) x$french_address else x$english_address
  author_list <- if (french) x$french_author_list else x$english_author_list
  preamble_pages <- if (french) x$french_preamble_pages else x$english_preamble_pages
  content_pages <- if (french) x$french_content_pages else x$english_content_pages
  doi <- if (french) x$french_doi else x$english_doi
  isbn <- if (french) x$french_isbn else x$english_isbn
  cat_no <- if (french) x$french_cat_no else x$english_cat_no

  # Read the tech report cover template and replace bookmarks
  cover_file <- if (french) "01-tech-report-cover-french.docx" else "01-tech-report-cover-english.docx"
  frontmatter <- officer::read_docx(system.file("tech-report-docx", cover_file, package = "csasdown2")) |>
    replace_bookmark_with_markdown("title", title) |>
    replace_bookmark_with_markdown("authors", author) |>
    replace_bookmark_with_markdown("address", address) |>
    officer::body_replace_text_at_bkm("year", as.character(x$year)) |>
    officer::body_replace_text_at_bkm("number", x$report_number)

  # Save the frontmatter temporarily
  print(frontmatter, target = "tmp-frontmatter.docx")

  # Read and process the titlepage template
  # For multi-line fields, preserve backslash line breaks but collapse other newlines
  title_clean <- trimws(gsub("\n(?!\\s*$)", " ", title, perl = TRUE))
  authors_clean <- trimws(gsub("\n(?!\\s*$)", " ", author, perl = TRUE))
  # For address, keep the backslash line breaks
  address_clean <- address

  titlepage_file <- if (french) "03-tech-report-titlepage-french.docx" else "03-tech-report-titlepage-english.docx"
  titlepage <- officer::read_docx(system.file("tech-report-docx", titlepage_file, package = "csasdown2")) |>
    replace_bookmark_with_markdown("title", title_clean) |>
    replace_bookmark_with_markdown("authors", authors_clean) |>
    replace_bookmark_with_markdown("address", address_clean) |>
    replace_bookmark_with_markdown("year", as.character(x$year)) |>
    replace_bookmark_with_markdown("number", x$report_number)
  print(titlepage, target = "tmp-titlepage.docx")

  # Read and process the colophon template
  total_pages <- as.numeric(preamble_pages) + as.numeric(content_pages)

  # Build citation - ensure all components are single line, trimmed strings
  citation_prefix <- if (french) "Rapp. tech. can. sci. halieut. aquat. " else "Can. Tech. Rep. Fish. Aquat. Sci. "
  citation <- paste0(
    trimws(gsub("\n", " ", author_list)), " ",
    x$year, ". ",
    trimws(gsub("\n", " ", title)), ". ",
    citation_prefix, x$report_number, ": ",
    preamble_pages, " + ", content_pages, " p. ",
    "https://doi.org/", trimws(doi)
  )

  # Conditional bookmark names for colophon
  cat_no_bkm <- if (french) "cat_no_french" else "cat_no_english"
  isbn_bkm <- if (french) "isbn_french" else "isbn_english"
  doi_bkm <- if (french) "doi_french" else "doi_english"
  citation_bkm <- if (french) "citation_french" else "citation_english"

  colophon_file <- if (french) "04-tech-report-colophon-french.docx" else "04-tech-report-colophon-english.docx"
  colophon <- officer::read_docx(system.file("tech-report-docx", colophon_file, package = "csasdown2")) |>
    officer::body_replace_text_at_bkm("year", as.character(x$year)) |>
    officer::body_replace_text_at_bkm(cat_no_bkm, cat_no) |>
    officer::body_replace_text_at_bkm(isbn_bkm, isbn) |>
    officer::body_replace_text_at_bkm("doi", doi) |>
    officer::body_replace_text_at_bkm(doi_bkm, doi) |>
    replace_bookmark_with_markdown(citation_bkm, citation)
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

