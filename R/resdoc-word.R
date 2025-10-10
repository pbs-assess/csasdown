#' Creates an Microsoft Word CSAS-formatted document
#'
#' @description This is a function called in output in the YAML of the
#' `index.Rmd` file to specify the creation of a Microsoft Word version of the
#' Research Document or Science Response.
#'
#' @param ... Other arguments to [officedown::rdocx_document()]
#' @import bookdown
#' @rdname csas_docx
#' @return A Word Document in the `.docx` format based on the CSAS Res Doc
#'   template
#' @export

resdoc_docx <- function(...) {
  ## Table caption style (Caption - Table) was not being applied using the standard
  ## reference docx. May be a bug (https://github.com/davidgohel/officedown/issues/112).
  ## Created another reference docx with "Table Caption" as a style (default name)
  ## and it worked. Revert to standard reference docx if issue is resolved.
  ##
  ## 2024-10-21: Copied ordered (ol style) and unordered list (ul style) styles made
  ##             for the fsar-template.docx into resdoc-content.docx
  ##
  # file <- if (fr()) "RES2021-fra-content.docx" else "RES2021-eng-content.docx"
  file <- "resdoc-content.docx"
  base <- officedown::rdocx_document(...,
    base_format = "bookdown::word_document2",
    number_sections = FALSE,
    tables = list(
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
      style = "Figure",
      align = "center",
      caption = list(
        style = "Caption - Figure",
        pre = "Figure ", sep = ". ",
        fp_text = officer::fp_text_lite(bold = FALSE)
      )
    ),
    lists = list(
      ol.style = "ol style",
      ul.style = "ul style"
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
  base <- add_caption_fix_postprocessor(base, reference_docx = "resdoc-content.docx")

  base
}

#' Add frontmatter to Res Doc word file
#'
#' Add title page and table of contents to a Res Doc Word document.
#'
#' @keywords internal
#' @param index_fn The name of the YAML file, typically 'index.Rmd' for bookdown
#' @param yaml_fn The Bookdown YAML file name. '_bookdown.yml' by default
#' @param keep_files If `TRUE`, keep the temporary files created by post
#'   processing (`tmp-*.md` and `tmp-*.docx`)
#'
#' @return A merged .docx
add_resdoc_word_frontmatter <- function(index_fn, yaml_fn = "_bookdown.yml", verbose = FALSE, keep_files = FALSE) {
  if (verbose) cli_inform("Adding frontmatter to the Research Document using the officer package...")

  x <- rmarkdown::yaml_front_matter(index_fn)

  ## This reference docx only includes styles; headers and footers were removed since
  ## they are included in the frontmatter docx.
  reference_fn <- system.file("csas-docx", "resdoc-blank-content.docx", package = "csasdown2")

  ## Extract yaml front matter and construct md files, then use pandoc_convert to
  ## export content to word. The officer package is latter used to add said content
  ## to one frontmatter document. Applying this approach rather than using officer
  ## text replacement since it is not set-up to interpret markdown syntax.

  md <- c(
    '::: {custom-style="Cover: Document title"}', x$english_title, ":::",
    '::: {custom-style="Cover: Author"}', x$english_author, ":::",
    '::: {custom-style="Cover: Address"}', x$english_address, ":::"
  )
  writeLines(md, "tmp-titlepage.md")
  rmarkdown::pandoc_convert("tmp-titlepage.md",
    to = "docx",
    output = "tmp-titlepage.docx",
    options = paste0("--reference-doc=", reference_fn)
  )

  md <- c(
    "\n**Correct citation for this publication:**\n",
    '::: {custom-style="citation"}',
    paste0(trimws(x$english_author_list), " ", x$year, ". ", trimws(x$english_title), ". DFO Can. Sci. Advis. Sec. Res. Doc. ", x$year, "/", x$report_number, ". ", x$english_pre_amble_pages, " + ", x$english_content_pages, " p."),
    ":::",
    "\n**Aussi disponible en fran\u00e7ais:**\n",
    '::: {custom-style="citation"}',
    paste0(trimws(x$french_author_list), " ", x$year, ". ", trimws(x$french_title), ". Secr. can. de consult. sci. du MPO. Doc. de rech. ", x$year, "/", x$report_number, ". ", x$french_preamble_pages, " + ", x$french_content_pages, "p."),
    ":::"
  )
  writeLines(md, "tmp-citation.md")
  rmarkdown::pandoc_convert("tmp-citation.md",
    to = "docx",
    output = "tmp-citation.docx",
    options = paste0("--reference-doc=", reference_fn)
  )

  writeLines(x$abstract, "tmp-abstract.md")
  rmarkdown::pandoc_convert("tmp-abstract.md",
    to = "docx",
    output = "tmp-abstract.docx",
    options = paste0("--reference-doc=", reference_fn)
  )

  ## Drop returns left from empty title and abstract entries
  book_filename <- paste0("_book/", get_book_filename(yaml_fn), ".docx")
  # book_filename <- "_book/resdoc.docx"
  content <- officer::read_docx(book_filename) |>
    officer::cursor_begin() |>
    officer::body_remove() |>
    officer::body_remove() |>
    officer::body_remove()
  print(content, target = "tmp-content.docx")

  # Fix table caption alignment in the extracted content
  fix_table_caption_alignment("tmp-content.docx", reference_docx = "resdoc-content.docx")

  frontmatter <- officer::read_docx(system.file("csas-docx", "resdoc-frontmatter.docx", package = "csasdown2")) |>
    officer::headers_replace_text_at_bkm("region", x$english_region) |>
    officer::headers_replace_text_at_bkm("year", as.character(x$year)) |>
    officer::headers_replace_text_at_bkm("report_number", as.character(x$report_number)) |>
    officer::footers_replace_text_at_bkm("date", paste(x$english_month, x$year)) |>
    officer::cursor_begin() |>
    officer::body_add_docx("tmp-titlepage.docx", pos = "before") |>
    officer::cursor_reach(keyword = "TABLE OF CONTENTS") |>
    officer::cursor_backward() |>
    officer::body_add_docx("tmp-citation.docx", pos = "before") |>
    officer::cursor_reach(keyword = "ABSTRACT") |>
    officer::body_add_docx("tmp-abstract.docx", pos = "after") |>
    officer::cursor_end() |>
    officer::body_remove()
  print(frontmatter, target = "tmp-frontmatter.docx")

  doc <- officer::read_docx("tmp-frontmatter.docx") |>
    officer::body_add_docx("tmp-content.docx") |>
    officer::cursor_reach(keyword = "TABLE OF CONTENTS") |>
    officer::body_add_toc()

  # print(doc, target = "tmp-doc.docx")
  print(doc, target = book_filename)

  # Fix table caption alignment in the final assembled document
  fix_table_caption_alignment(book_filename, reference_docx = "resdoc-content.docx")

  if (!keep_files) {
    unlink(c(
      "tmp-titlepage.md", "tmp-titlepage.docx",
      "tmp-citation.md", "tmp-citation.docx",
      "tmp-abstract.md", "tmp-abstract.docx",
      "tmp-frontmatter.docx", "tmp-content.docx"
    ))
  }

  invisible()
}
