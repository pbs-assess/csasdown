#' @rdname csas_docx
#' @export

techreport_docx <- function(...) {
  .csasdown_docx_base(
    reference_docx = "tech-report-content.docx",
    link_citations = TRUE,
    template_dir = "tech-report-docx",
    use_pandoc_highlight = FALSE,
    ...
  )
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

  file.copy(book_filename, "tmp-content.docx", overwrite = TRUE)

  # Conditional field selection based on language
  title <- if (french) x$french_title else x$english_title
  author <- if (french) x$french_author else x$english_author
  address <- if (french) x$french_address else x$english_address
  doi <- if (french) x$french_doi else x$english_doi
  isbn <- if (french) x$french_isbn else x$english_isbn
  cat_no <- if (french) x$french_cat_no else x$english_cat_no

  # Read the tech report cover template and replace bookmarks
  cover_file <- if (french) "01-tech-report-cover-french.docx" else "01-tech-report-cover-english.docx"
  frontmatter <- officer::read_docx(system.file("tech-report-docx", cover_file, package = "csasdown")) |>
    replace_bookmark_with_markdown("title", title) |>
    replace_bookmark_with_markdown("authors", author) |>
    replace_bookmark_with_markdown("address", address) |>
    officer::body_replace_text_at_bkm("year", as.character(x$year)) |>
    officer::body_replace_text_at_bkm("number", as.character(x$report_number))

  # Save the frontmatter temporarily
  print(frontmatter, target = "tmp-frontmatter.docx")

  # Read and process the titlepage template
  # For multi-line fields, preserve backslash line breaks but collapse other newlines
  title_clean <- trimws(gsub("\n(?!\\s*$)", " ", title, perl = TRUE))
  authors_clean <- trimws(gsub("\n(?!\\s*$)", " ", author, perl = TRUE))
  # For address, keep the backslash line breaks
  address_clean <- address

  titlepage_file <- if (french) "03-tech-report-titlepage-french.docx" else "03-tech-report-titlepage-english.docx"
  titlepage <- officer::read_docx(system.file("tech-report-docx", titlepage_file, package = "csasdown")) |>
    replace_bookmark_with_markdown("title", title_clean) |>
    replace_bookmark_with_markdown("authors", authors_clean) |>
    replace_bookmark_with_markdown("address", address_clean) |>
    replace_bookmark_with_markdown("year", as.character(x$year)) |>
    replace_bookmark_with_markdown("number", as.character(x$report_number))
  print(titlepage, target = "tmp-titlepage.docx")

  prefixes <- c(
    english = "Can. Tech. Rep. Fish. Aquat. Sci.",
    french = "Rapp. tech. can. sci. halieut. aquat."
  )
  citation <- report_citation(x, french, prefixes[[if (french) "french" else "english"]])
  build_report_abstracts_docx(
    x, french, prefixes,
    reference_docx = system.file("tech-report-docx", "tech-report-content.docx", package = "csasdown"),
    target = "tmp-abstracts.docx"
  )

  # Conditional bookmark names for colophon
  cat_no_bkm <- if (french) "cat_no_french" else "cat_no_english"
  isbn_bkm <- if (french) "isbn_french" else "isbn_english"
  doi_bkm <- if (french) "doi_french" else "doi_english"
  citation_bkm <- if (french) "citation_french" else "citation_english"

  colophon_file <- if (french) "04-tech-report-colophon-french.docx" else "04-tech-report-colophon-english.docx"
  colophon <- officer::read_docx(system.file("tech-report-docx", colophon_file, package = "csasdown")) |>
    officer::body_replace_text_at_bkm("year", as.character(x$year)) |>
    officer::body_replace_text_at_bkm(cat_no_bkm, cat_no) |>
    officer::body_replace_text_at_bkm(isbn_bkm, isbn) |>
    officer::body_replace_text_at_bkm("doi", doi) |>
    officer::body_replace_text_at_bkm(doi_bkm, doi) |>
    replace_bookmark_with_markdown(citation_bkm, citation)
  print(colophon, target = "tmp-colophon.docx")

  assemble_report_docx(book_filename)

  # Cleanup temporary files
  if (!keep_files) {
    unlink(c("tmp-frontmatter.docx", "tmp-titlepage.docx", "tmp-colophon.docx", "tmp-abstracts.docx", "tmp-content.docx"))
  }

  invisible()
}
