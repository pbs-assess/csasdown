#' Replace bookmark content with markdown-formatted text
#'
#' Replaces the content of a Word document bookmark with text that can include
#' markdown formatting (italics using `*text*` syntax, superscript using `^text^`
#' syntax, line breaks using `\` at end of line). The function preserves the
#' original formatting (font, size, bold, etc.) from the template while adding
#' italic/superscript/line break formatting where specified.
#'
#' @param doc An officer rdocx object
#' @param bookmark Character string specifying the bookmark name
#' @param text Character string with markdown text (use `*text*` for italics,
#'   `^text^` for superscript, `\` at end of line for line breaks)
#'
#' @return An officer rdocx object with the bookmark content replaced
#'
#' @keywords internal
#' @noRd
coerce_bookmark_markdown_text <- function(text, bookmark) {
  if (is.null(text)) {
    return(character())
  }

  if (is.list(text)) {
    text <- unlist(text, recursive = TRUE, use.names = FALSE)
    cli::cli_warn("Bookmark {.val {bookmark}} received a list value; coercing to character text.")
  }

  if (!is.atomic(text)) {
    cli::cli_abort(
      "Bookmark {.val {bookmark}} must be scalar/vector text-like input; received {.val {paste(class(text), collapse = '/')}}."
    )
  }

  if (!is.character(text)) {
    text <- as.character(text)
  }

  text[is.na(text)] <- ""
  text
}

#' @keywords internal
#' @noRd
replace_bookmark_with_markdown <- function(doc, bookmark, text) {
  # Use pandoc to convert markdown to Word XML, then extract and inject the runs
  temp_md <- tempfile(fileext = ".md")
  temp_docx <- tempfile(fileext = ".docx")

  text <- coerce_bookmark_markdown_text(text, bookmark)
  writeLines(text, temp_md)

  # Convert markdown to docx using pandoc (preserves *italics*)
  rmarkdown::pandoc_convert(temp_md,
    to = "docx",
    output = temp_docx
  )

  # Extract the XML with formatted runs from pandoc output
  temp_dir <- tempfile()
  dir.create(temp_dir)
  utils::unzip(temp_docx, exdir = temp_dir)

  temp_doc_xml <- readLines(file.path(temp_dir, "word", "document.xml"), warn = FALSE)
  temp_doc_xml_full <- paste(temp_doc_xml, collapse = "")

  # Extract text and italic info from pandoc runs
  runs_pattern <- "<w:r>(.*?)</w:r>"
  runs <- regmatches(temp_doc_xml_full, gregexpr(runs_pattern, temp_doc_xml_full, perl = TRUE))[[1]]

  # Parse each run to extract text/line breaks and whether it's italic/superscript
  run_info <- list()
  for (run in runs) {
    is_italic <- grepl("<w:i\\s*/?>", run)
    is_superscript <- grepl('<w:vertAlign\\s+w:val="superscript"', run)

    # Check if this is a line break run
    if (grepl("<w:br\\s*/?>", run)) {
      run_info[[length(run_info) + 1]] <- list(is_break = TRUE)
    } else {
      # Extract text between <w:t> tags (with any attributes)
      text_pattern <- "<w:t[^>]*>([^<]*)</w:t>"
      text_match <- regmatches(run, regexpr(text_pattern, run, perl = TRUE))
      if (length(text_match) > 0 && nchar(text_match) > 0) {
        text_content <- gsub(text_pattern, "\\1", text_match, perl = TRUE)
        if (nchar(text_content) > 0) {
          run_info[[length(run_info) + 1]] <- list(text = text_content, italic = is_italic, superscript = is_superscript, is_break = FALSE)
        }
      }
    }
  }

  # Now modify the original document's XML
  doc_dir <- tempfile()
  dir.create(doc_dir)

  # Save current document
  temp_current <- tempfile(fileext = ".docx")
  print(doc, target = temp_current)

  # Extract it
  utils::unzip(temp_current, exdir = doc_dir)

  # Read document.xml
  doc_xml <- readLines(file.path(doc_dir, "word", "document.xml"), warn = FALSE)
  doc_xml_full <- paste(doc_xml, collapse = "")

  # Extract content between bookmarks (handles cross-paragraph bookmarks)
  # Use (?s) flag to make . match newlines
  bookmark_extract_pattern <- sprintf(
    '(?s)(<w:bookmarkStart[^>]*w:name="%s"[^>]*/>)(.*?)(<w:bookmarkEnd[^>]*/>)',
    bookmark
  )
  bookmark_match <- regmatches(doc_xml_full, regexpr(bookmark_extract_pattern, doc_xml_full, perl = TRUE))

  # Extract the rPr (run properties) from the first run to preserve formatting
  original_rpr <- ""
  if (length(bookmark_match) > 0) {
    rpr_match <- regmatches(bookmark_match, regexpr("<w:rPr>(.*?)</w:rPr>", bookmark_match, perl = TRUE))
    if (length(rpr_match) > 0) {
      original_rpr <- rpr_match
    }
  }

  # Build new runs with original formatting + italic/superscript where needed
  new_runs <- character(0)
  for (info in run_info) {
    # Handle line breaks
    if (isTRUE(info$is_break)) {
      if (nchar(original_rpr) > 0) {
        new_run <- sprintf('<w:r>%s<w:br/></w:r>', original_rpr)
      } else {
        new_run <- '<w:r><w:br/></w:r>'
      }
    } else {
      # Handle text runs with formatting
      if ((info$italic || info$superscript) && nchar(original_rpr) > 0) {
        modified_rpr <- original_rpr
        if (info$italic) {
          modified_rpr <- sub("</w:rPr>", "<w:i/><w:iCs/></w:rPr>", modified_rpr)
        }
        if (info$superscript) {
          modified_rpr <- sub("</w:rPr>", '<w:vertAlign w:val="superscript"/></w:rPr>', modified_rpr)
        }
        new_run <- sprintf('<w:r>%s<w:t xml:space="preserve">%s</w:t></w:r>',
                          modified_rpr, info$text)
      } else if (nchar(original_rpr) > 0) {
        new_run <- sprintf('<w:r>%s<w:t xml:space="preserve">%s</w:t></w:r>',
                          original_rpr, info$text)
      } else {
        formatting_tags <- ""
        if (info$italic) {
          formatting_tags <- paste0(formatting_tags, "<w:i/><w:iCs/>")
        }
        if (info$superscript) {
          formatting_tags <- paste0(formatting_tags, '<w:vertAlign w:val="superscript"/>')
        }

        if (nchar(formatting_tags) > 0) {
          new_run <- sprintf('<w:r><w:rPr>%s</w:rPr><w:t xml:space="preserve">%s</w:t></w:r>',
                            formatting_tags, info$text)
        } else {
          new_run <- sprintf('<w:r><w:t xml:space="preserve">%s</w:t></w:r>', info$text)
        }
      }
    }
    new_runs <- c(new_runs, new_run)
  }

  # Replace content between bookmarks - use (?s) to match across newlines
  new_runs_xml <- paste(new_runs, collapse = "")

  replacement_pattern <- sprintf(
    '(?s)(<w:bookmarkStart[^>]*w:name="%s"[^>]*/>)(.*?)(<w:bookmarkEnd[^>]*/>)',
    bookmark
  )
  doc_xml_full <- gsub(replacement_pattern,
                      paste0('\\1', new_runs_xml, '\\3'),
                      doc_xml_full, perl = TRUE)

  # Write back
  writeLines(doc_xml_full, file.path(doc_dir, "word", "document.xml"))

  # Rezip - delete old file first
  unlink(temp_current)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(doc_dir)
  files <- list.files(recursive = TRUE, full.names = FALSE, include.dirs = FALSE, all.files = TRUE, no.. = TRUE)

  output_zip <- normalizePath(temp_current, mustWork = FALSE)
  utils::zip(zipfile = output_zip, files = files, flags = "-q")

  # Return to original directory before reading
  setwd(old_wd)

  # Verify the zip file exists and has content
  if (!file.exists(output_zip)) {
    stop("Modified docx file was not created at: ", output_zip)
  }

  # Read modified document back
  doc <- officer::read_docx(output_zip)

  # Clean up
  unlink(c(temp_md, temp_docx, temp_dir, doc_dir), recursive = TRUE)

  doc
}
