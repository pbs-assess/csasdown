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


#' Extract abstract body text from an officer Word document
#'
#' Extracts the paragraph text between the first and second Heading 1
#' sections in a Word document represented as an `rdocx` object from
#' the `officer` package (assumes the abstract is the first section).
#'
#' @param doc An `rdocx` object created by `officer::read_docx()`.
#'
#' @return A character vector containing abstract paragraphs.
#'
#' @noRd
extract_abstract <- function(doc) {

  s <- officer::docx_summary(doc)

  h1_idx <- s$doc_index[!is.na(s$style_name) & s$style_name == "heading 1"]

  abstract_text <- s$text[s$doc_index > h1_idx[1] & s$doc_index < h1_idx[2]]

  abstract_text
}

#' Remove abstract section from an officer Word document
#'
#' Removes the first Heading 1 section (assumed to be the abstract)
#' from an `rdocx` object created by `officer::read_docx()`.
#'
#' @param doc An `rdocx` object created by `officer::read_docx()`.
#'
#' @return An `rdocx` object with the abstract section removed.
#'
#' @noRd
remove_abstract <- function(doc) {

  s <- officer::docx_summary(doc)

  h1_idx <- s$doc_index[!is.na(s$style_name) & s$style_name == "heading 1"]

  doc <- officer::cursor_begin(doc)

  for (i in seq_len(h1_idx[2])) {
    doc <- officer::body_remove(doc)
  }

  doc
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
  abstract_keyword <- if (french) "R\u00c9SUM\u00c9" else "ABSTRACT"
  book_filename <- paste0("_book/", get_book_filename(yaml_fn), ".docx")

  # first page
  title <- if (french) x$french_title else x$english_title
  region <- if (french) x$french_region else x$english_region
  authors <- if (french) x$french_author else x$english_author
  address <- if (french) x$french_address else x$english_address
  frontmatter <- officer::read_docx(ref_front_file) |>
    replace_bookmark_with_markdown("region", region) |>
    replace_bookmark_with_markdown("title", title) |>
    replace_bookmark_with_markdown("authors", authors) |>
    replace_bookmark_with_markdown("address", address) |>
    officer::body_replace_text_at_bkm("year", as.character(x$year))

  # citation page (works for both languages because bookmarks are different)
  frontmatter <- frontmatter |>
    replace_bookmark_with_markdown("english_authors_list", x$english_author_list) |>
    replace_bookmark_with_markdown("year_english_reference1", x$year) |>
    replace_bookmark_with_markdown("year_english_reference", x$year) |>
    replace_bookmark_with_markdown("english_title", x$english_title)
  frontmatter <- frontmatter |>
    replace_bookmark_with_markdown("french_authors_list", x$french_author_list) |>
    replace_bookmark_with_markdown("year_french_reference1", x$year) |>
    replace_bookmark_with_markdown("year_french_reference", x$year) |>
    replace_bookmark_with_markdown("french_title", x$french_title)

  # add table of contents
  frontmatter_with_toc <- frontmatter |>
    officer::cursor_reach(keyword = toc_keyword) |>
    officer::body_add_toc()

  # add abstract
  content_with_abstract <- officer::read_docx(book_filename)
  abstract_pars <- extract_abstract(content_with_abstract)

  frontmatter_with_abstract <- frontmatter_with_toc |>
    officer::cursor_reach(keyword = abstract_keyword) |>
    officer::body_add_par(abstract_pars[1], style = "Body Text")

  if (length(abstract_pars) > 1) {
    for (i in 2:length(abstract_pars)) {
      frontmatter_with_abstract <- frontmatter_with_abstract |>
        officer::body_add_par(abstract_pars[i], style = "Body Text")
    }
  }

  print(frontmatter_with_abstract, target = "tmp-frontmatter.docx")

  # fix missing namespaces
  fix_missing_namespaces("tmp-frontmatter.docx")

  content <- remove_abstract(content_with_abstract) |>
    officer::docx_set_settings(even_and_odd_headers = FALSE)

  # FIXME: can we avoid writing here to save time?
  print(content, target = "tmp-content.docx")

  full_doc <- officer::read_docx("tmp-frontmatter.docx") |>
    officer::cursor_end() |>
    officer::body_add_docx("tmp-content.docx", pos = "on") |>
    officer::docx_set_settings(even_and_odd_headers = FALSE)

  print(full_doc, target = book_filename)

  # fix table caption alignment in the final assembled document
  fix_table_caption_alignment(book_filename, reference_docx = "resdoc-content-2026.docx")

  if (!keep_files) {
    unlink(c(
      "tmp-content.docx", "tmp-frontmatter-with-toc.docx"
    ))
  }

  invisible()
}
