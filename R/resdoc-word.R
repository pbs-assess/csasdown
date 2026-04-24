#' @import bookdown
#' @rdname csas_docx
#' @export

resdoc_docx <- function(...) {
  .csasdown_docx_base(
    reference_docx = "resdoc-content-2026.docx",
    link_citations = TRUE,
    template_dir = "csas-docx",
    use_pandoc_highlight = TRUE,
    ...
  )
}

#' Fix missing namespaces in merged document
#'
#' @param docx_path Path to the .docx file to fix
#' @keywords internal
#' @noRd
fix_missing_namespaces <- function(docx_path) {
  temp_dir <- tempfile()
  dir.create(temp_dir)

  utils::unzip(docx_path, exdir = temp_dir)

  doc_xml_path <- file.path(temp_dir, "word", "document.xml")
  doc_content <- readLines(doc_xml_path, warn = FALSE)

  # Add missing namespace declarations to the root element if not present
  if (!any(grepl('xmlns:a=', doc_content[1:5]))) {
    doc_content[2] <- gsub(
      '<w:document ',
      '<w:document xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture" ',
      doc_content[2]
    )
  }

  writeLines(doc_content, doc_xml_path)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  setwd(temp_dir)
  files <- list.files(recursive = TRUE, full.names = FALSE, include.dirs = FALSE)
  utils::zip(zipfile = file.path(old_wd, docx_path), files = files, flags = "-q")

  unlink(temp_dir, recursive = TRUE)

  invisible()
}

add_resdoc_word_frontmatter2 <- function(index_fn, yaml_fn = "_bookdown.yml", verbose = FALSE, keep_files = FALSE) {
  if (verbose) cli_inform("Adding frontmatter to the Research Document using the officer package...")

  x <- rmarkdown::yaml_front_matter(index_fn)

  # Parse single author field into language-specific fields
  parsed <- parse_author_field(x$author)
  x$english_author <- parsed$english_author
  x$french_author <- parsed$french_author
  x$english_author_list <- parsed$english_author_list
  x$french_author_list <- parsed$french_author_list

  french <- isTRUE(x$output[[1]]$french)

  front_filename <- if (french) "resdoc-frontmatter-french2.docx" else "resdoc-frontmatter-english2.docx"
  ref_front_file <- system.file("csas-docx", front_filename, package = "csasdown")
  toc_keyword <- if (french) "TABLE DES MATI\u00c8RES" else "TABLE OF CONTENTS"
  book_filename <- paste0("_book/", get_book_filename(yaml_fn), ".docx")
  abstract_file <- "tmp-abstract.md"
  if (!file.exists(abstract_file)) {
    cli::cli_abort("Expected abstract capture file '{abstract_file}' was not created during render.")
  }

  # first and second page
  title <- if (french) x$french_title else x$english_title
  region <- if (french) x$french_region else x$english_region
  authors <- if (french) x$french_author else x$english_author
  address <- if (french) x$french_address else x$english_address

  frontmatter <- officer::read_docx(ref_front_file) |>
    replace_bookmarks_with_markdown(
      title = title,
      authors = authors,
      address = address,
      english_authors_list = x$english_author_list,
      year_english_reference1 = x$year,
      year_english_reference = x$year,
      english_title = x$english_title,
      french_authors_list = x$french_author_list, # citation works for both languages because bookmarks are different
      year_french_reference1 = x$year,
      year_french_reference = x$year,
      french_title = x$french_title,
      abstract = readLines(abstract_file, warn = FALSE)
    ) |>
    officer::headers_replace_text_at_bkm("region", region) |>
    officer::headers_replace_text_at_bkm("year", as.character(x$year))

  # add table of contents
  frontmatter_with_toc <- frontmatter |>
    officer::cursor_reach(keyword = toc_keyword) |>
    officer::body_add_toc()
  print(frontmatter_with_toc, target = "tmp-frontmatter.docx")

  # fix missing namespaces
  fix_missing_namespaces("tmp-frontmatter.docx")

  full_doc <- officer::read_docx("tmp-frontmatter.docx") |>
    officer::cursor_end() |>
    officer::body_add_docx(book_filename, pos = "on") |>
    officer::docx_set_settings(even_and_odd_headers = FALSE)

  print(full_doc, target = book_filename)

  # fix table caption alignment in the final assembled document
  fix_table_caption_alignment(book_filename, reference_docx = "resdoc-content-2026.docx")

  if (!keep_files) {
    unlink(c(
      "tmp-content.docx", "tmp-frontmatter.docx", abstract_file
    ))
  }

  invisible()
}
