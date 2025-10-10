#' Fix table caption alignment in Word document
#'
#' @description Removes center justification override from table captions when
#' ft.align="center" is used. This is a workaround for an officedown issue where
#' ft.align applies to both the table and its caption.
#'
#' @param docx_file Path to the .docx file to fix
#' @keywords internal
fix_table_caption_alignment <- function(docx_file) {
  # Create temporary directory for extraction
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Extract the .docx (which is a zip file)
  utils::unzip(docx_file, exdir = temp_dir)

  # Function to process XML content
  fix_xml_content <- function(xml_content) {
    # Remove center justification from TableCaption styles (resdoc)
    # Do this BEFORE renaming to Caption-Table
    xml_content <- gsub(
      '<w:pStyle w:val="TableCaption"/>\\s*<w:jc w:val="center"/>',
      '<w:pStyle w:val="TableCaption"/>',
      xml_content,
      perl = TRUE
    )

    # Rename TableCaption references to Caption-Table (which inherits from TableCaption)
    xml_content <- gsub(
      '<w:pStyle w:val="TableCaption"/>',
      '<w:pStyle w:val="Caption-Table"/>',
      xml_content,
      fixed = TRUE
    )

    xml_content
  }

  # Process main document.xml
  doc_xml_path <- file.path(temp_dir, "word", "document.xml")
  if (file.exists(doc_xml_path)) {
    xml_content <- readLines(doc_xml_path, warn = FALSE)
    xml_content <- paste(xml_content, collapse = "\n")
    xml_content <- fix_xml_content(xml_content)
    writeLines(xml_content, doc_xml_path)
  }

  # Copy "Caption - Table" style from reference template to styles.xml
  styles_xml_path <- file.path(temp_dir, "word", "styles.xml")
  if (file.exists(styles_xml_path)) {
    # Extract reference template to get the Caption - Table style
    ref_docx <- system.file("csas-docx", "resdoc-content.docx", package = "csasdown")
    ref_temp <- tempfile()
    dir.create(ref_temp)
    utils::unzip(ref_docx, exdir = ref_temp)

    ref_styles_path <- file.path(ref_temp, "word", "styles.xml")
    if (file.exists(ref_styles_path)) {
      ref_styles <- readLines(ref_styles_path, warn = FALSE)
      ref_styles <- paste(ref_styles, collapse = "\n")

      # Extract both TableCaption and Caption-Table style definitions
      table_caption_style <- regmatches(ref_styles, regexpr(
        '<w:style[^>]*w:styleId="TableCaption"[^>]*>.*?</w:style>',
        ref_styles,
        perl = TRUE
      ))

      caption_table_style <- regmatches(ref_styles, regexpr(
        '<w:style[^>]*w:styleId="Caption-Table"[^>]*>.*?</w:style>',
        ref_styles,
        perl = TRUE
      ))

      if (length(table_caption_style) > 0 || length(caption_table_style) > 0) {
        # Read current styles
        styles_content <- readLines(styles_xml_path, warn = FALSE)
        styles_content <- paste(styles_content, collapse = "\n")

        # Remove existing TableCaption style if present (will be replaced with reference version)
        styles_content <- gsub(
          '<w:style[^>]*w:styleId="TableCaption"[^>]*>.*?</w:style>',
          '',
          styles_content,
          perl = TRUE
        )

        # Insert both styles before </w:styles>
        styles_to_add <- paste0(
          if (length(table_caption_style) > 0) table_caption_style else "",
          "\n",
          if (length(caption_table_style) > 0) caption_table_style else "",
          "\n</w:styles>"
        )

        styles_content <- sub(
          '</w:styles>',
          styles_to_add,
          styles_content,
          fixed = TRUE
        )

        writeLines(styles_content, styles_xml_path)
      }
    }

    unlink(ref_temp, recursive = TRUE)
  }

  # Process embedded .docx files (for resdoc with tmp-content.docx, etc.)
  embedded_docx <- list.files(file.path(temp_dir, "word"), pattern = "\\.docx$", full.names = TRUE)
  for (embedded_file in embedded_docx) {
    # Create temp dir for this embedded file
    embedded_temp <- tempfile()
    dir.create(embedded_temp)

    # Extract embedded .docx
    utils::unzip(embedded_file, exdir = embedded_temp)

    # Process its document.xml
    embedded_doc_xml <- file.path(embedded_temp, "word", "document.xml")
    if (file.exists(embedded_doc_xml)) {
      xml_content <- readLines(embedded_doc_xml, warn = FALSE)
      xml_content <- paste(xml_content, collapse = "\n")
      xml_content <- fix_xml_content(xml_content)
      writeLines(xml_content, embedded_doc_xml)
    }

    # Copy "Caption - Table" style to embedded styles.xml
    embedded_styles_path <- file.path(embedded_temp, "word", "styles.xml")
    if (file.exists(embedded_styles_path)) {
      # Extract reference template to get the Caption - Table style
      ref_docx <- system.file("csas-docx", "resdoc-content.docx", package = "csasdown")
      ref_temp <- tempfile()
      dir.create(ref_temp)
      utils::unzip(ref_docx, exdir = ref_temp)

      ref_styles_path <- file.path(ref_temp, "word", "styles.xml")
      if (file.exists(ref_styles_path)) {
        ref_styles <- readLines(ref_styles_path, warn = FALSE)
        ref_styles <- paste(ref_styles, collapse = "\n")

        # Extract both TableCaption and Caption-Table style definitions
        table_caption_style <- regmatches(ref_styles, regexpr(
          '<w:style[^>]*w:styleId="TableCaption"[^>]*>.*?</w:style>',
          ref_styles,
          perl = TRUE
        ))

        caption_table_style <- regmatches(ref_styles, regexpr(
          '<w:style[^>]*w:styleId="Caption-Table"[^>]*>.*?</w:style>',
          ref_styles,
          perl = TRUE
        ))

        if (length(table_caption_style) > 0 || length(caption_table_style) > 0) {
          # Read current styles
          styles_content <- readLines(embedded_styles_path, warn = FALSE)
          styles_content <- paste(styles_content, collapse = "\n")

          # Remove existing TableCaption style if present (will be replaced with reference version)
          styles_content <- gsub(
            '<w:style[^>]*w:styleId="TableCaption"[^>]*>.*?</w:style>',
            '',
            styles_content,
            perl = TRUE
          )

          # Insert both styles before </w:styles>
          styles_to_add <- paste0(
            if (length(table_caption_style) > 0) table_caption_style else "",
            "\n",
            if (length(caption_table_style) > 0) caption_table_style else "",
            "\n</w:styles>"
          )

          styles_content <- sub(
            '</w:styles>',
            styles_to_add,
            styles_content,
            fixed = TRUE
          )

          writeLines(styles_content, embedded_styles_path)
        }
      }

      unlink(ref_temp, recursive = TRUE)
    }

    # Re-zip the embedded .docx
    curr_dir <- getwd()
    setwd(embedded_temp)
    unlink(embedded_file)
    files_to_zip <- list.files(recursive = TRUE, all.files = TRUE, include.dirs = FALSE)
    utils::zip(embedded_file, files_to_zip, flags = "-r9Xq")
    setwd(curr_dir)

    unlink(embedded_temp, recursive = TRUE)
  }

  # Re-zip the modified files back to .docx
  curr_dir <- getwd()
  setwd(temp_dir)

  # Create new zip with all files in a temp location
  temp_zip <- tempfile(fileext = ".zip")
  files_to_zip <- list.files(recursive = TRUE, all.files = TRUE, include.dirs = FALSE)
  utils::zip(temp_zip, files_to_zip, flags = "-r9Xq")

  setwd(curr_dir)

  # Replace original file with the fixed version
  unlink(docx_file)
  file.copy(temp_zip, docx_file, overwrite = TRUE)
  unlink(temp_zip)

  invisible(docx_file)
}

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
      package = "csasdown"
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
  # This runs after pandoc creates the .docx file
  base_post_processor <- base$post_processor
  base$post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    # Call the original post-processor first
    if (!is.null(base_post_processor)) {
      output_file <- base_post_processor(metadata, input_file, output_file, clean, verbose)
    }

    # Fix table caption centering issue
    fix_table_caption_alignment(output_file)

    output_file
  }

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
  reference_fn <- system.file("csas-docx", "resdoc-blank-content.docx", package = "csasdown")

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
    paste0(x$english_author_list, ". ", x$year, ". ", x$title, ". DFO Can. Sci. Advis. Sec. Res. Doc. ", x$year, "/", x$report_number, ". iv + xx p."),
    ":::",
    "\n**Aussi disponible en fran\u00e7ais:**\n",
    '::: {custom-style="citation"}',
    paste0(x$french_author_list, ". ", x$year, ". ", x$french_title, ". Secr. can. de consult. sci. du MPO. Doc. de rech. ", x$year, "/", x$report_number, ". iv + xx p."),
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
  fix_table_caption_alignment("tmp-content.docx")

  frontmatter <- officer::read_docx(system.file("csas-docx", "resdoc-frontmatter.docx", package = "csasdown")) |>
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
  fix_table_caption_alignment(book_filename)

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
