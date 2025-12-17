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

resdoc_docx <- function(...) {
  dots <- list(...)

  # Extract and merge pandoc_args
  default_pandoc_args <- c(get_pandoc_highlight_arg(), "--metadata", "link-citations=true", "--csl", "csl/csas.csl")
  user_pandoc_args <- dots$pandoc_args
  pandoc_args <- c(default_pandoc_args, user_pandoc_args)
  dots$pandoc_args <- NULL

  file <- "resdoc-content-2026.docx"

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
      # "Heading 1 with numbers" = "heading 1",
      # "Heading 2 with numbers" = "heading 2",
      # "Heading 3 with numbers" = "heading 3",
      # "Heading 4 with numbers" = "heading 4",
      # "Heading 5 with numbers" = "heading 5"
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
  ref_front_file <- system.file("csas-docx", front_filename, package = "csasdown2")
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
  frontmatter <- frontmatter |>
    officer::cursor_reach(keyword = toc_keyword) |>
    officer::body_add_toc() |>
    officer::body_add_break(pos = "after")

  print(frontmatter, target = "tmp-frontmatter-with-toc.docx")

  # Set roman numbering for TOC/abstract section (section 3 of frontmatter)
  set_section_page_numbering(
    "tmp-frontmatter-with-toc.docx",
    format = "lowerRoman",
    start = 3,
    section_index = -1
  )

  # fix missing namespaces
  fix_missing_namespaces("tmp-frontmatter-with-toc.docx")

  content <- officer::read_docx(book_filename) |>
    officer::cursor_begin() |>
    officer::body_remove() |>
    officer::body_remove() |>
    officer::body_remove() |>
    officer::docx_set_settings(even_and_odd_headers = FALSE)

  print(content, target = "tmp-content.docx")

  # Set arabic numbering for main content section, starting at 1
  set_section_page_numbering(
    "tmp-content.docx",
    format = NULL,  # Use default (decimal)
    start = 1,
    section_index = 1
  )

  # join them together into full doc
  full_doc <- officer::read_docx("tmp-frontmatter-with-toc.docx") |>
    officer::cursor_end() |>
    officer::body_import_docx("tmp-content.docx") |>
    officer::docx_set_settings(even_and_odd_headers = FALSE)

  # apply Abstract Heading style by removing and re-adding with correct style
  # Handle case where template might have wrong language (e.g., ABSTRACT in template but rendering French)
  # Try to find the correct language keyword first, then fall back to the other language
  search_keyword <- tryCatch({
    content_tmp <- officer::cursor_reach(content, keyword = abstract_keyword)
    abstract_keyword  # Found the correct one
  }, error = function(e) {
    # Correct keyword not found, try the other language
    if (french) "ABSTRACT" else "R\u00c9SUM\u00c9"
  })
  # Style name is also language-specific
  abstract_style <- if (french) "R\u00e9sum\u00e9" else "Abstract Heading"
  # abstract_style <- "Abstract Heading"

  # Always replace with the correct language version (abstract_keyword)
  full_doc <- full_doc |>
    officer::cursor_reach(keyword = search_keyword) |>
    officer::body_remove() |>
    officer::body_add_par(value = abstract_keyword, style = abstract_style, pos = "before")

  print(full_doc, target = book_filename)

  # Insert section break between abstract and main content
  insert_section_break_after_abstract(book_filename, french = french)

  # fix table caption alignment in the final assembled document
  fix_table_caption_alignment(book_filename, reference_docx = "resdoc-content-2026.docx")

  if (!keep_files) {
    unlink(c(
      "tmp-content.docx", "tmp-frontmatter-with-toc.docx"
    ))
  }

  invisible()
}
