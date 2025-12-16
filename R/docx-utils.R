#' Fix list indentation in numbering XML
#'
#' @description Removes indentation from list level definitions in numbering.xml.
#' This removes the <w:ind> tags from within <w:lvl> elements that cause unwanted
#' list indentation in the rendered document.
#'
#' @param xml_content Character string containing numbering XML content
#' @return Modified XML content as character string
#' @keywords internal
#' @noRd
fix_list_indentation_xml <- function(xml_content) {
  xml_content <- gsub(
    '<w:ind w:left="[0-9]+" w:hanging="[0-9]+"/>',
    '<w:ind w:left="360" w:hanging="360"/>',
    xml_content,
    perl = TRUE
  )

  xml_content
}

#' Fix table caption XML content
#'
#' @description Removes center justification from TableCaption styles and
#' renames them to Caption-Table. This is the core XML transformation.
#'
#' @param xml_content Character string containing XML content
#' @return Modified XML content as character string
#' @keywords internal
#' @noRd
fix_table_caption_xml <- function(xml_content) {
  # Remove center justification from TableCaption styles
  # Do this BEFORE renaming to Caption-Table
  xml_content <- gsub(
    '<w:pStyle w:val="TableCaption"/>\\s*<w:jc w:val="center"/>',
    '<w:pStyle w:val="TableCaption"/>',
    xml_content,
    perl = TRUE
  )

  # Rename TableCaption references to Caption-Table
  xml_content <- gsub(
    '<w:pStyle w:val="TableCaption"/>',
    '<w:pStyle w:val="Caption-Table"/>',
    xml_content,
    fixed = TRUE
  )

  # Fix appendix caption spacing: remove trailing space after "Table A." in <w:t> tags
  # Pattern: <w:t ...>Table A. </w:t> -> <w:t ...>Table A.</w:t>
  xml_content <- gsub(
    '(Table [A-Z])\\. </w:t>',
    '\\1.</w:t>',
    xml_content,
    perl = TRUE
  )

  # Fix appendix figure spacing: remove trailing space after "Figure A." in <w:t> tags
  xml_content <- gsub(
    '(Figure [A-Z])\\. </w:t>',
    '\\1.</w:t>',
    xml_content,
    perl = TRUE
  )

  # Fix appendix caption separator spacing: remove extra space in ". " separator
  # The pattern is: after a SEQ field ends, there's a text element with ".  " (period + 2 spaces)
  # We want to change it to ". " (period + single space)
  # Pattern: <w:t ...>.  </w:t> -> <w:t ...>. </w:t>
  xml_content <- gsub(
    '<w:t([^>]*)>\\.  </w:t>',
    '<w:t\\1>. </w:t>',
    xml_content,
    perl = TRUE
  )

  # Fix appendix cross-references
  xml_content <- fix_appendix_crossrefs_xml(xml_content)

  # Remove empty paragraphs (moves page breaks into following paragraph)
  # xml_content <- remove_empty_paragraphs_xml(xml_content)

  xml_content
}

#' Remove empty paragraphs from Word XML
#'
#' @description Removes paragraph elements that contain no meaningful text content.
#' An empty paragraph is defined as one that has no <w:t> tags with actual text,
#' though it may contain bookmarks, formatting, and other non-text elements.
#'
#' @param xml_content Character string containing XML content
#' @return Modified XML content as character string
#' @keywords internal
#' @noRd
remove_empty_paragraphs_xml <- function(xml_content) {
  # Move page breaks from standalone paragraphs into the following paragraph
  # This removes the extra line while preserving the page break
  # Pattern: <w:p><w:r><w:br w:type="page"/></w:r></w:p> followed by anything then <w:p>
  # The "anything" can include bookmarks, section breaks, etc.
  # Replace with: <w:p><w:r><w:br w:type="page"/></w:r> (merge into next paragraph)

  xml_content <- gsub(
    '(<w:p>)\\s*<w:r>\\s*<w:br w:type="page"/>\\s*</w:r>\\s*</w:p>((?:(?!<w:p>).)*?)(<w:p>)',
    '\\2\\3<w:r><w:br w:type="page"/></w:r>',
    xml_content,
    perl = TRUE
  )

  # Also remove truly empty paragraphs (just in case)
  # Pattern 1: <w:p><w:pPr/></w:p> (self-closing pPr)
  xml_content <- gsub('<w:p><w:pPr/></w:p>', '', xml_content, fixed = TRUE)

  # Pattern 2: <w:p><w:pPr></w:pPr></w:p> (empty pPr)
  xml_content <- gsub('<w:p><w:pPr></w:pPr></w:p>', '', xml_content, fixed = TRUE)

  xml_content
}

#' Fix table cell paragraph styles
#'
#' @description Replaces Normal style with BodyText style in table cell content.
#' The officedown mapstyles parameter should handle this, but it doesn't apply
#' to table cell content, only to regular paragraphs.
#'
#' @param xml_content Character string containing XML content
#' @return Modified XML content as character string
#' @keywords internal
#' @noRd
fix_table_cell_styles_xml <- function(xml_content) {
  # Inject appropriate styles into table cell paragraphs:
  # - First row (header): Caption-Table (font size removed to use style default)
  # - Other rows (body): BodyText (keep 10pt font size)

  # Step 1: Remove duplicate styles (function may be called multiple times)
  xml_content <- gsub(
    '(<w:pPr>)(?:<w:pStyle w:val="(?:BodyText|Caption-Table)"/>)+',
    '\\1',
    xml_content,
    perl = TRUE
  )

  # Step 2: Process each table individually to track row positions
  # Split content by tables
  table_pattern <- '<w:tbl[^>]*>.*?</w:tbl>'
  tables <- gregexpr(table_pattern, xml_content, perl = TRUE)

  if (tables[[1]][1] != -1) {
    table_matches <- regmatches(xml_content, tables)[[1]]

    # Process each table
    for (i in seq_along(table_matches)) {
      original_table <- table_matches[i]
      modified_table <- original_table

      # Find all rows in this table
      row_pattern <- '<w:tr[^>]*>.*?</w:tr>'
      rows <- gregexpr(row_pattern, modified_table, perl = TRUE)

      if (rows[[1]][1] != -1) {
        row_matches <- regmatches(modified_table, rows)[[1]]

        # Process each row
        for (row_idx in seq_along(row_matches)) {
          original_row <- row_matches[row_idx]
          modified_row <- original_row

          # Determine style based on row position
          is_header <- row_idx == 1
          style_name <- if (is_header) "Caption-Table" else "BodyText"

          # Inject style into table cell paragraphs that don't have a style
          # Pattern: Find <w:pPr> immediately following </w:tcPr><w:p>
          modified_row <- gsub(
            '(</w:tcPr><w:p><w:pPr>)(?!<w:pStyle)',
            paste0('\\1<w:pStyle w:val="', style_name, '"/>'),
            modified_row,
            perl = TRUE
          )

          # Remove font size from header row only (let Caption-Table style control it)
          if (is_header) {
            modified_row <- gsub('<w:sz w:val="20"/>', '', modified_row, fixed = TRUE)
            modified_row <- gsub('<w:szCs w:val="20"/>', '', modified_row, fixed = TRUE)
          }

          # Remove Helvetica font specifications from all table cells
          # Let the Word styles (Caption-Table or BodyText) control the font
          modified_row <- gsub(
            '<w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/>',
            '<w:rFonts/>',
            modified_row,
            fixed = TRUE
          )

          # Replace the row in the table
          modified_table <- sub(
            fixed = TRUE,
            pattern = original_row,
            replacement = modified_row,
            x = modified_table
          )
        }
      }

      # Replace the table in the content
      xml_content <- sub(
        fixed = TRUE,
        pattern = original_table,
        replacement = modified_table,
        x = xml_content
      )
    }
  }

  xml_content
}

#' Fix appendix cross-references in Word XML
#'
#' @description Modifies REF fields that point to appendix figures/tables to display
#' the correct appendix-prefixed numbers (e.g., "A.1" instead of just "1"). Bookdown
#' generates cross-references using Word REF fields, but these only capture the
#' sequential number, not the "Figure A." prefix. This function adds a format switch
#' to include the prefix.
#'
#' @param xml_content Character string containing XML content
#' @return Modified XML content as character string
#' @keywords internal
#' @noRd
fix_appendix_crossrefs_xml <- function(xml_content) {
  # Strategy:
  # 1. Find all figure/table captions with appendix numbering (e.g., "Figure A.")
  # 2. Build a mapping: bookmark name -> appendix prefix (e.g., "app-fig1" -> "A.")
  # 3. Find REF fields pointing to those bookmarks
  # 4. Add Word format switch \# "A.0" to display prefix + number

  bookmark_prefixes <- list()

  # Find figure captions with appendix prefixes
  # Pattern: "Figure A." in a text run, followed by a bookmark
  # More lenient pattern that doesn't require SEQ field (which might be far away)
  fig_caption_pattern <- '<w:t[^>]*>Figure ([A-Z])\\.</w:t>[^<]*</w:r>\\s*<w:bookmarkStart[^>]+w:name="([^"]+)"'

  fig_matches <- gregexpr(fig_caption_pattern, xml_content, perl = TRUE)
  if (fig_matches[[1]][1] != -1) {
    # Use regmatches with capture=TRUE to get captured groups
    match_list <- regmatches(xml_content, fig_matches, invert = FALSE)
    # Extract capture groups manually by re-matching each full match
    for (full_match in match_list[[1]]) {
      # Extract the letter (first capture group)
      letter_match <- regexpr('>Figure ([A-Z])\\.', full_match, perl = TRUE)
      letter_start <- attr(letter_match, "capture.start")[1]
      letter_length <- attr(letter_match, "capture.length")[1]
      appendix_letter <- substr(full_match, letter_start, letter_start + letter_length - 1)

      # Extract the bookmark name (second capture group)
      bookmark_match <- regexpr('w:name="([^"]+)"', full_match, perl = TRUE)
      bookmark_start <- attr(bookmark_match, "capture.start")[1]
      bookmark_length <- attr(bookmark_match, "capture.length")[1]
      bookmark_name <- substr(full_match, bookmark_start, bookmark_start + bookmark_length - 1)

      bookmark_prefixes[[bookmark_name]] <- paste0(appendix_letter, ".")
    }
  }

  # Find table captions with appendix prefixes
  tab_caption_pattern <- '<w:t[^>]*>Table ([A-Z])\\.</w:t>[^<]*</w:r>\\s*<w:bookmarkStart[^>]+w:name="([^"]+)"'

  tab_matches <- gregexpr(tab_caption_pattern, xml_content, perl = TRUE)
  if (tab_matches[[1]][1] != -1) {
    match_list <- regmatches(xml_content, tab_matches, invert = FALSE)
    for (full_match in match_list[[1]]) {
      # Extract the letter
      letter_match <- regexpr('>Table ([A-Z])\\.', full_match, perl = TRUE)
      letter_start <- attr(letter_match, "capture.start")[1]
      letter_length <- attr(letter_match, "capture.length")[1]
      appendix_letter <- substr(full_match, letter_start, letter_start + letter_length - 1)

      # Extract the bookmark name
      bookmark_match <- regexpr('w:name="([^"]+)"', full_match, perl = TRUE)
      bookmark_start <- attr(bookmark_match, "capture.start")[1]
      bookmark_length <- attr(bookmark_match, "capture.length")[1]
      bookmark_name <- substr(full_match, bookmark_start, bookmark_start + bookmark_length - 1)

      bookmark_prefixes[[bookmark_name]] <- paste0(appendix_letter, ".")
    }
  }

  # Build a mapping of bookmarks to their actual caption numbers
  # Strategy: For each bookmark with an appendix prefix, find the SEQ field that follows it
  # and determine what number it represents by counting preceding SEQ fields of the same type
  bookmark_numbers <- list()

  for (bookmark_name in names(bookmark_prefixes)) {
    prefix <- bookmark_prefixes[[bookmark_name]]
    letter <- sub('\\.$', '', prefix)  # e.g., "A." -> "A"

    # Determine if this is a figure or table
    seq_type <- if (grepl('^(fig-|app.*fig)', bookmark_name)) "fig" else "tab"

    # Find the bookmark position
    bookmark_pattern <- sprintf('<w:bookmarkStart[^>]+w:name="%s"', bookmark_name)
    bookmark_match <- regexpr(bookmark_pattern, xml_content, perl = TRUE)

    if (bookmark_match > 0) {
      # Extract text from bookmark onward to find the following SEQ field
      text_after_bookmark <- substr(xml_content, bookmark_match, nchar(xml_content))

      # Find the first SEQ field for this type after the bookmark
      # Pattern: SEQ fig:A or SEQ tab:A (the sequence type specific to this appendix)
      seq_pattern_after <- sprintf('SEQ %s:%s', seq_type, letter)
      seq_match_after <- regexpr(seq_pattern_after, text_after_bookmark, fixed = TRUE)

      if (seq_match_after > 0) {
        # Now count how many SEQ fields of this type appear in the document
        # up to and including this one
        seq_position_in_doc <- bookmark_match + seq_match_after - 1

        # Count all SEQ fields of this specific type up to this position
        text_up_to_seq <- substr(xml_content, 1, seq_position_in_doc + nchar(seq_pattern_after))
        all_seq_matches <- gregexpr(seq_pattern_after, text_up_to_seq, fixed = TRUE)
        seq_number <- length(regmatches(text_up_to_seq, all_seq_matches)[[1]])

        if (seq_number > 0) {
          bookmark_numbers[[bookmark_name]] <- paste0(prefix, seq_number)
        }
      }
    }
  }

  # Fix REF fields by replacing them with the actual text
  # Instead of modifying the field code, replace the entire hyperlink structure
  # with plain text showing the correct number
  for (bookmark in names(bookmark_numbers)) {
    number_text <- bookmark_numbers[[bookmark]]

    # Find and replace the entire hyperlink structure that contains the REF field
    # Pattern: <w:hyperlink w:anchor="bookmark">...<w:instrText>REF bookmark \h</w:instrText>...</w:hyperlink>
    # Replace with: <w:r><w:t>A.1</w:t></w:r>

    # Use (?s) flag to enable DOTALL mode so . matches newlines
    hyperlink_pattern <- sprintf(
      '(?s)<w:hyperlink w:anchor="%s">.*?</w:hyperlink>',
      bookmark
    )

    replacement <- sprintf(
      '<w:r><w:t>%s</w:t></w:r>',
      number_text
    )

    xml_content <- gsub(hyperlink_pattern, replacement, xml_content, perl = TRUE)
  }

  # Fix appendix section cross-references (e.g., "Appendix 3" -> "Appendix A")
  xml_content <- fix_appendix_section_refs_xml(xml_content)

  xml_content
}

#' Fix appendix section cross-references in Word XML
#'
#' @description Replaces chapter numbers with appendix letters in cross-references
#' to appendix sections (e.g., "Appendix 3" becomes "Appendix A"). Bookdown treats
#' appendices as regular chapters, so \@ref(app:biology) resolves to a chapter
#' number rather than a letter. This function identifies which chapters are
#' appendices and replaces their numbers with the corresponding letters.
#'
#' @param xml_content Character string containing XML content
#' @return Modified XML content as character string
#' @keywords internal
#' @noRd
fix_appendix_section_refs_xml <- function(xml_content) {
  # Strategy:
  # 1. Find appendix headings with bookmarks (e.g., app:biology) and their letters
  # 2. Find cross-references to those bookmarks that appear as "Appendix <number>"
  # 3. Replace the number with the corresponding letter

  # Build a mapping from bookmark names to appendix letters
  # Pattern: Find bookmarks like app:biology that are near "APPENDIX A." text
  bookmark_to_letter <- list()

  # Find all appendix bookmarks (bookmarks that start with "app:")
  bookmark_pattern <- '<w:bookmarkStart[^>]+w:name="(app:[^"]+)"'
  bookmark_matches <- gregexpr(bookmark_pattern, xml_content, perl = TRUE)

  if (bookmark_matches[[1]][1] != -1) {
    match_starts <- bookmark_matches[[1]]

    for (i in seq_along(match_starts)) {
      pos <- match_starts[i]

      # Extract the full match to get the bookmark name
      match_len <- attr(bookmark_matches[[1]], "match.length")[i]
      full_match <- substr(xml_content, pos, pos + match_len - 1)

      # Extract bookmark name using capture group
      name_match <- regexpr('w:name="(app:[^"]+)"', full_match, perl = TRUE)
      if (name_match > 0) {
        name_start <- attr(name_match, "capture.start")[1]
        name_len <- attr(name_match, "capture.length")[1]
        bookmark_name <- substr(full_match, name_start, name_start + name_len - 1)

        # Look ahead in the document for "APPENDIX [LETTER]." text
        # It should appear within ~500 characters after the bookmark
        text_chunk <- substr(xml_content, pos, min(nchar(xml_content), pos + 500))

        appendix_pattern <- '<w:t[^>]*>APPENDIX ([A-Z])\\.'
        appendix_match <- regexpr(appendix_pattern, text_chunk, perl = TRUE)

        if (appendix_match > 0) {
          # Extract the letter
          letter_start <- attr(appendix_match, "capture.start")[1]
          letter_len <- attr(appendix_match, "capture.length")[1]
          appendix_letter <- substr(text_chunk, letter_start, letter_start + letter_len - 1)

          bookmark_to_letter[[bookmark_name]] <- appendix_letter
        }
      }
    }
  }

  # Now we have a mapping of bookmark names to letters
  # But cross-references in the text don't show the bookmark name - they just show a number
  # We need to figure out which numbers correspond to which appendices

  # The challenge: "Appendix 3" doesn't tell us which bookmark it refers to
  # We need to find patterns and infer the mapping

  # Alternative: Find all "Appendix X" patterns and look at the surrounding context
  # to see if there are any clues (like nearby hyperlinks or REF fields)

  # Actually, let's try a simpler approach:
  # Find appendix headings with their letters and count which chapter number they are
  # But count ONLY numbered chapters (exclude those that might be unnumbered)

  # Better approach: Build chapter-to-letter mapping by looking at actual appendix headings
  # and inferring their sequential position among appendices

  appendix_letters_in_order <- c()

  # Find appendix headings in document order
  appendix_heading_pattern <- '<w:t[^>]*>APPENDIX ([A-Z])\\.'
  appendix_heading_matches <- gregexpr(appendix_heading_pattern, xml_content, perl = TRUE)

  if (appendix_heading_matches[[1]][1] != -1) {
    positions <- appendix_heading_matches[[1]]

    for (i in seq_along(positions)) {
      pos <- positions[i]
      match_len <- attr(appendix_heading_matches[[1]], "match.length")[i]
      full_match <- substr(xml_content, pos, pos + match_len - 1)

      # Extract letter
      letter_match <- regexpr('>APPENDIX ([A-Z])\\.', full_match, perl = TRUE)
      if (letter_match > 0) {
        letter_start <- attr(letter_match, "capture.start")[1]
        letter_len <- attr(letter_match, "capture.length")[1]
        letter <- substr(full_match, letter_start, letter_start + letter_len - 1)

        appendix_letters_in_order <- c(appendix_letters_in_order, letter)
      }
    }
  }

  # Now find all "Appendix X" cross-references and figure out what X values exist
  # Pattern: "Appendix" followed by a space and a number
  # Note: There can be a lot of XML/whitespace between "Appendix" and the number
  # Use (?s) to make . match newlines (DOTALL mode)
  ref_pattern <- '(?s)Appendix</w:t>.*?<w:t[^>]*>(\\d+)</w:t>'
  ref_matches <- gregexpr(ref_pattern, xml_content, perl = TRUE)

  chapter_numbers_found <- c()

  if (ref_matches[[1]][1] != -1) {
    for (i in seq_along(ref_matches[[1]])) {
      pos <- ref_matches[[1]][i]
      match_len <- attr(ref_matches[[1]], "match.length")[i]
      full_match <- substr(xml_content, pos, pos + match_len - 1)

      # Extract the number
      num_match <- regexpr('<w:t[^>]*>(\\d+)</w:t>', full_match, perl = TRUE)
      if (num_match > 0) {
        num_start <- attr(num_match, "capture.start")[1]
        num_len <- attr(num_match, "capture.length")[1]
        chapter_num <- substr(full_match, num_start, num_start + num_len - 1)

        if (!(chapter_num %in% chapter_numbers_found)) {
          chapter_numbers_found <- c(chapter_numbers_found, chapter_num)
        }
      }
    }
  }

  # Sort the chapter numbers and map them to letters in order
  chapter_numbers_found <- sort(as.numeric(chapter_numbers_found))
  chapter_to_letter <- list()

  for (i in seq_along(chapter_numbers_found)) {
    if (i <= length(appendix_letters_in_order)) {
      chapter_num <- as.character(chapter_numbers_found[i])
      chapter_to_letter[[chapter_num]] <- appendix_letters_in_order[i]
    }
  }

  # Now replace "Appendix X" with "Appendix LETTER"
  for (chapter_num in names(chapter_to_letter)) {
    letter <- chapter_to_letter[[chapter_num]]

    # Pattern: Appendix</w:t> ... <w:t>CHAPTER_NUM</w:t>
    # Use (?s) to make . match newlines in DOTALL mode
    # The (?:(?!</w:p>).) construct ensures we don't cross paragraph boundaries
    # even with DOTALL mode enabled
    pattern <- sprintf(
      '(?s)(Appendix</w:t>)((?:(?!</w:p>).)*?)(<w:t[^>]*>)%s(</w:t>)',
      chapter_num
    )

    replacement <- sprintf(
      '\\1\\2\\3%s\\4',
      letter
    )

    xml_content <- gsub(pattern, replacement, xml_content, perl = TRUE)
  }

  xml_content
}

#' Apply Abstract Heading style to ABSTRACT section in XML
#'
#' @description Replaces Heading1 style with Abstract Heading style for
#' paragraphs containing "ABSTRACT" or "RÉSUMÉ" text.
#'
#' @param xml_content Character string containing XML content
#' @return Modified XML content as character string
#' @keywords internal
#' @noRd
apply_abstract_heading_style_xml <- function(xml_content) {
  # More flexible pattern that handles various XML structures
  # Matches: <w:pPr>...<w:pStyle w:val="Heading1"/>...</w:pPr><w:r...>...<w:t>ABSTRACT</w:t>
  pattern <- paste0(
    '(<w:pPr[^>]*>(?:(?!</w:pPr>).)*)',
    '<w:pStyle w:val="Heading1"/>',
    '((?:(?!</w:pPr>).)*</w:pPr>\\s*<w:r[^>]*>(?:(?!</w:r>).)*<w:t[^>]*>)',
    'ABSTRACT',
    '(</w:t>)'
  )

  replacement <- paste0(
    '\\1',
    '<w:pStyle w:val="Abstract Heading"/>',
    '\\2',
    'ABSTRACT',
    '\\3'
  )

  xml_content <- gsub(pattern, replacement, xml_content, perl = TRUE)

  # French version
  pattern_fr <- paste0(
    '(<w:pPr[^>]*>(?:(?!</w:pPr>).)*)',
    '<w:pStyle w:val="Heading1"/>',
    '((?:(?!</w:pPr>).)*</w:pPr>\\s*<w:r[^>]*>(?:(?!</w:r>).)*<w:t[^>]*>)',
    'R\u00c9SUM\u00c9',
    '(</w:t>)'
  )

  replacement_fr <- paste0(
    '\\1',
    '<w:pStyle w:val="Abstract Heading"/>',
    '\\2',
    'R\u00c9SUM\u00c9',
    '\\3'
  )

  xml_content <- gsub(pattern_fr, replacement_fr, xml_content, perl = TRUE)

  xml_content
}

#' Extract style definitions from reference template
#'
#' @param ref_docx_path Path to reference .docx file
#' @param style_ids Character vector of style IDs to extract
#' @return Named list of style definitions (XML strings)
#' @keywords internal
#' @noRd
extract_styles_from_reference <- function(ref_docx_path, style_ids = c("TableCaption", "Caption-Table")) {
  ref_temp <- tempfile()
  dir.create(ref_temp)
  on.exit(unlink(ref_temp, recursive = TRUE), add = TRUE)

  utils::unzip(ref_docx_path, exdir = ref_temp)

  ref_styles_path <- file.path(ref_temp, "word", "styles.xml")
  if (!file.exists(ref_styles_path)) {
    return(list())
  }

  ref_styles <- readLines(ref_styles_path, warn = FALSE)
  ref_styles <- paste(ref_styles, collapse = "\n")

  styles <- list()
  for (style_id in style_ids) {
    pattern <- sprintf(
      '<w:style[^>]*w:styleId="%s"[^>]*>.*?</w:style>',
      style_id
    )
    style_def <- regmatches(ref_styles, regexpr(pattern, ref_styles, perl = TRUE))
    if (length(style_def) > 0) {
      styles[[style_id]] <- style_def
    }
  }

  styles
}

#' Inject style definitions into styles.xml
#'
#' @param styles_xml_path Path to styles.xml file
#' @param style_definitions Named list of style definitions (XML strings)
#' @param replace_existing Logical, whether to replace existing styles with same ID
#' @keywords internal
#' @noRd
inject_styles_into_styles_xml <- function(styles_xml_path, style_definitions, replace_existing = TRUE) {
  if (!file.exists(styles_xml_path) || length(style_definitions) == 0) {
    return(invisible(NULL))
  }

  styles_content <- readLines(styles_xml_path, warn = FALSE)
  styles_content <- paste(styles_content, collapse = "\n")

  # Remove existing styles if requested
  if (replace_existing) {
    for (style_id in names(style_definitions)) {
      pattern <- sprintf(
        '<w:style[^>]*w:styleId="%s"[^>]*>.*?</w:style>',
        style_id
      )
      styles_content <- gsub(pattern, '', styles_content, perl = TRUE)
    }
  }

  # Insert all styles before </w:styles>
  styles_to_add <- paste0(
    paste(unlist(style_definitions), collapse = "\n"),
    "\n</w:styles>"
  )

  styles_content <- sub(
    '</w:styles>',
    styles_to_add,
    styles_content,
    fixed = TRUE
  )

  writeLines(styles_content, styles_xml_path)
  invisible(NULL)
}

#' Process document.xml with table fixes
#'
#' @param doc_xml_path Path to document.xml file
#' @keywords internal
#' @noRd
process_document_xml <- function(doc_xml_path) {
  if (!file.exists(doc_xml_path)) {
    return(invisible(NULL))
  }

  xml_content <- readLines(doc_xml_path, warn = FALSE)
  xml_content <- paste(xml_content, collapse = "")
  xml_content <- fix_table_caption_xml(xml_content)
  xml_content <- fix_table_cell_styles_xml(xml_content)
  writeLines(xml_content, doc_xml_path)

  invisible(NULL)
}

#' Copy table caption styles from reference template to document
#'
#' @param temp_dir Directory where .docx has been extracted
#' @param ref_docx_path Path to reference .docx template
#' @keywords internal
#' @noRd
copy_styles_to_docx <- function(temp_dir, ref_docx_path) {
  styles_xml_path <- file.path(temp_dir, "word", "styles.xml")
  if (!file.exists(styles_xml_path)) {
    return(invisible(NULL))
  }

  # Extract both TableCaption and Caption-Table styles from reference
  style_definitions <- extract_styles_from_reference(
    ref_docx_path,
    style_ids = c("TableCaption", "Caption-Table")
  )

  # Inject them into the document's styles.xml
  inject_styles_into_styles_xml(
    styles_xml_path,
    style_definitions,
    replace_existing = TRUE
  )

  invisible(NULL)
}

#' Process embedded .docx files within a parent .docx
#'
#' @param parent_temp_dir Directory where parent .docx has been extracted
#' @param ref_docx_path Path to reference .docx template
#' @keywords internal
#' @noRd
process_embedded_docx_files <- function(parent_temp_dir, ref_docx_path) {
  embedded_docx <- list.files(
    file.path(parent_temp_dir, "word"),
    pattern = "\\.docx$",
    full.names = TRUE
  )

  for (embedded_file in embedded_docx) {
    # Create temp dir for this embedded file
    embedded_temp <- tempfile()
    dir.create(embedded_temp)
    on.exit(unlink(embedded_temp, recursive = TRUE), add = TRUE)

    # Extract embedded .docx
    utils::unzip(embedded_file, exdir = embedded_temp)

    # Process its document.xml
    embedded_doc_xml <- file.path(embedded_temp, "word", "document.xml")
    process_document_xml(embedded_doc_xml)

    # Copy styles to embedded document
    copy_styles_to_docx(embedded_temp, ref_docx_path)

    # Process numbering.xml in embedded document
    embedded_numbering_xml <- file.path(embedded_temp, "word", "numbering.xml")
    if (file.exists(embedded_numbering_xml)) {
      numbering_content <- readLines(embedded_numbering_xml, warn = FALSE)
      numbering_content <- paste(numbering_content, collapse = "")
      numbering_content <- fix_list_indentation_xml(numbering_content)
      writeLines(numbering_content, embedded_numbering_xml)
    }

    # Re-zip the embedded .docx
    curr_dir <- getwd()
    setwd(embedded_temp)
    unlink(embedded_file)
    files_to_zip <- list.files(recursive = TRUE, all.files = TRUE, include.dirs = FALSE)
    utils::zip(embedded_file, files_to_zip, flags = "-r9Xq")
    setwd(curr_dir)
  }

  invisible(NULL)
}

#' Fix table caption alignment in Word document
#'
#' @description Removes center justification override from table captions when
#' ft.align="center" is used. This is a workaround for an officedown issue where
#' ft.align applies to both the table and its caption.
#'
#' @param docx_file Path to the .docx file to fix
#' @param reference_docx Path to reference .docx template to copy styles from.
#'   If NULL, will try to detect based on context. Can be either a full path
#'   or just the filename (will look in inst/csas-docx/).
#' @keywords internal
#' @noRd
fix_table_caption_alignment <- function(docx_file, reference_docx = NULL) {
  # Determine reference template if not provided
  if (is.null(reference_docx)) {
    # Default to resdoc template
    reference_docx <- "resdoc-content.docx"
  }

  # If only filename provided, get full path from inst/csas-docx/
  if (!file.exists(reference_docx)) {
    reference_docx <- system.file(
      "csas-docx",
      reference_docx,
      package = "csasdown2"
    )
  }

  if (!file.exists(reference_docx)) {
    warning("Reference template not found: ", reference_docx)
    return(invisible(docx_file))
  }

  # Create temporary directory for extraction
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Extract the .docx (which is a zip file)
  utils::unzip(docx_file, exdir = temp_dir)

  # Process main document.xml
  doc_xml_path <- file.path(temp_dir, "word", "document.xml")
  process_document_xml(doc_xml_path)

  # Copy styles from reference template to main document
  copy_styles_to_docx(temp_dir, reference_docx)

  # Process numbering.xml to remove list indentation
  numbering_xml_path <- file.path(temp_dir, "word", "numbering.xml")
  if (file.exists(numbering_xml_path)) {
    numbering_content <- readLines(numbering_xml_path, warn = FALSE)
    numbering_content <- paste(numbering_content, collapse = "")
    numbering_content <- fix_list_indentation_xml(numbering_content)
    writeLines(numbering_content, numbering_xml_path)
  }

  # Process embedded .docx files (for resdoc with tmp-content.docx, etc.)
  process_embedded_docx_files(temp_dir, reference_docx)

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

#' Add table caption fix post-processor to output format
#'
#' @description Wraps an officedown output format with a post-processor that
#' fixes table caption alignment issues. This eliminates code duplication
#' between different document types.
#'
#' @param base_format An officedown-based output format (e.g., from rdocx_document())
#' @param reference_docx Path or filename of reference .docx template
#' @return The modified output format with post-processor added
#' @keywords internal
#' @noRd
add_caption_fix_postprocessor <- function(base_format, reference_docx) {
  base_post_processor <- base_format$post_processor

  base_format$post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    # Call the original post-processor first
    if (!is.null(base_post_processor)) {
      output_file <- base_post_processor(metadata, input_file, output_file, clean, verbose)
    }

    # Fix table caption centering issue
    fix_table_caption_alignment(output_file, reference_docx = reference_docx)

    output_file
  }

  base_format
}

#' Set page numbering format and start value for a document section
#'
#' @param docx_path Path to .docx file
#' @param format Page numbering format: "lowerRoman", "decimal", etc. NULL to use default
#' @param start Starting page number (integer)
#' @param section_index Which section to modify (-1 = last section, 1 = first)
#' @keywords internal
#' @noRd
set_section_page_numbering <- function(docx_path, format = NULL, start = 1, section_index = -1) {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  utils::unzip(docx_path, exdir = temp_dir)

  doc_xml_path <- file.path(temp_dir, "word", "document.xml")
  doc_lines <- readLines(doc_xml_path, warn = FALSE)
  doc_content <- paste(doc_lines, collapse = "\n")

  # (?s) enables DOTALL mode so . matches newlines
  sectpr_pattern <- '(?s)<w:sectPr[^>]*>.*?</w:sectPr>'
  sections <- gregexpr(sectpr_pattern, doc_content, perl = TRUE)

  if (sections[[1]][1] == -1) {
    warning("No section properties found in document")
    return(invisible(docx_path))
  }

  num_sections <- length(sections[[1]])
  target_index <- if (section_index == -1) num_sections else section_index

  if (target_index < 1 || target_index > num_sections) {
    warning(sprintf("Section index %d out of range (1-%d)", target_index, num_sections))
    return(invisible(docx_path))
  }

  match_starts <- sections[[1]]
  match_lengths <- attr(sections[[1]], "match.length")
  target_start <- match_starts[target_index]
  target_length <- match_lengths[target_index]
  target_section <- substr(doc_content, target_start, target_start + target_length - 1)

  target_section_modified <- gsub('<w:pgNumType[^>]*/?>', '', target_section, perl = TRUE)

  if (!is.null(format)) {
    pgnum_tag <- sprintf('<w:pgNumType w:fmt="%s" w:start="%d"/>', format, start)
  } else {
    pgnum_tag <- sprintf('<w:pgNumType w:start="%d"/>', start)
  }

  target_section_modified <- sub(
    '(<w:sectPr[^>]*>)',
    paste0('\\1\n      ', pgnum_tag),
    target_section_modified,
    perl = TRUE
  )

  escaped_pattern <- gsub("([\\[\\](){}^$.|*+?\\\\])", "\\\\\\1", target_section, perl = TRUE)
  doc_content <- gsub(escaped_pattern, target_section_modified, doc_content, perl = TRUE)

  writeLines(doc_content, doc_xml_path)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  setwd(temp_dir)
  files <- list.files(recursive = TRUE, full.names = FALSE, include.dirs = FALSE)
  utils::zip(zipfile = file.path(old_wd, docx_path), files = files, flags = "-q")

  invisible(docx_path)
}

#' Insert a section break after the abstract section
#'
#' @param docx_path Path to .docx file
#' @param french Logical, is this a French document?
#' @keywords internal
#' @noRd
insert_section_break_after_abstract <- function(docx_path, french = FALSE) {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  utils::unzip(docx_path, exdir = temp_dir)

  doc_xml_path <- file.path(temp_dir, "word", "document.xml")
  doc_lines <- readLines(doc_xml_path, warn = FALSE)
  doc_content <- paste(doc_lines, collapse = "\n")

  # First, find the abstract heading
  abstract_keyword <- if (french) "RÉSUMÉ" else "ABSTRACT"
  abstract_pattern <- paste0('(?s)<w:p>.*?<w:t[^>]*>', abstract_keyword, '</w:t>.*?</w:p>')
  abstract_match <- regexpr(abstract_pattern, doc_content, perl = TRUE)

  if (abstract_match[1] == -1) {
    warning("Could not find abstract heading")
    return(invisible(docx_path))
  }

  # Now find the first Heading1 AFTER the abstract
  # Find ALL Heading1 paragraphs in the document
  # Use negative lookahead to prevent matching across paragraph boundaries
  heading1_pattern <- '(?s)<w:p[^>]*>(?:(?!</w:p>).)*?<w:pPr>(?:(?!</w:p>).)*?<w:pStyle w:val="Heading1[^"]*"(?:(?!</w:p>).)*?</w:pPr>(?:(?!</w:p>).)*?</w:p>'
  all_h1_matches <- gregexpr(heading1_pattern, doc_content, perl = TRUE)

  if (all_h1_matches[[1]][1] == -1) {
    warning("Could not find any Heading1 paragraphs")
    return(invisible(docx_path))
  }

  # Find the first Heading1 that comes AFTER the abstract
  abstract_end_pos <- abstract_match[1] + attr(abstract_match, "match.length")
  h1_positions <- all_h1_matches[[1]]

  intro_match_absolute <- NA
  for (h1_pos in h1_positions) {
    if (h1_pos > abstract_end_pos) {
      intro_match_absolute <- h1_pos
      break
    }
  }

  if (is.na(intro_match_absolute)) {
    warning("Could not find Heading1 after abstract")
    return(invisible(docx_path))
  }

  # Find the last section's footer reference to reuse
  sectpr_pattern <- '(?s)<w:sectPr[^>]*>.*?</w:sectPr>'
  sections <- gregexpr(sectpr_pattern, doc_content, perl = TRUE)

  footer_ref <- ""
  if (sections[[1]][1] != -1) {
    num_sections <- length(sections[[1]])
    match_starts <- sections[[1]]
    match_lengths <- attr(sections[[1]], "match.length")
    last_start <- match_starts[num_sections]
    last_length <- match_lengths[num_sections]
    last_section <- substr(doc_content, last_start, last_start + last_length - 1)

    # Extract footer reference from last section
    footer_match <- regexpr('<w:footerReference w:type="default"[^>]*/>', last_section, perl = TRUE)
    if (footer_match[1] != -1) {
      footer_ref <- paste0('\n      ', substr(last_section, footer_match[1], footer_match[1] + attr(footer_match, "match.length") - 1))
    }
  }

  # Insert a section break with roman numbering and footer reference before the first heading
  section_break <- paste0(
    '<w:p>\n',
    '  <w:pPr>\n',
    '    <w:sectPr>',
    footer_ref, '\n',
    '      <w:pgNumType w:fmt="lowerRoman" w:start="3"/>\n',
    '      <w:pgSz w:w="12240" w:h="15840"/>\n',
    '      <w:pgMar w:top="1440" w:right="1440" w:bottom="1440" w:left="1440" w:header="720" w:footer="720"/>\n',
    '    </w:sectPr>\n',
    '  </w:pPr>\n',
    '</w:p>\n'
  )

  # Insert the section break before the first heading
  doc_content <- paste0(
    substr(doc_content, 1, intro_match_absolute - 1),
    section_break,
    substr(doc_content, intro_match_absolute, nchar(doc_content))
  )

  # Now update the last section (which will become section 4) to have arabic numbering
  # Find the last section
  sectpr_pattern <- '(?s)<w:sectPr[^>]*>.*?</w:sectPr>'
  sections <- gregexpr(sectpr_pattern, doc_content, perl = TRUE)

  if (sections[[1]][1] != -1) {
    num_sections <- length(sections[[1]])
    match_starts <- sections[[1]]
    match_lengths <- attr(sections[[1]], "match.length")
    last_start <- match_starts[num_sections]
    last_length <- match_lengths[num_sections]
    last_section <- substr(doc_content, last_start, last_start + last_length - 1)

    # Remove any existing pgNumType and add arabic numbering
    last_section_modified <- gsub('<w:pgNumType[^>]*/?>', '', last_section, perl = TRUE)
    last_section_modified <- sub(
      '(<w:sectPr[^>]*>)',
      '\\1\n      <w:pgNumType w:fmt="decimal" w:start="1"/>',
      last_section_modified,
      perl = TRUE
    )

    # Replace the last section in the document
    escaped_pattern <- gsub("([\\[\\](){}^$.|*+?\\\\])", "\\\\\\1", last_section, perl = TRUE)
    doc_content <- gsub(escaped_pattern, last_section_modified, doc_content, perl = TRUE)
  }

  writeLines(doc_content, doc_xml_path)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  setwd(temp_dir)
  files <- list.files(recursive = TRUE, full.names = FALSE, include.dirs = FALSE)
  utils::zip(zipfile = file.path(old_wd, docx_path), files = files, flags = "-q")

  invisible(docx_path)
}
