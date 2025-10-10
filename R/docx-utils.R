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

#' Process document.xml with table caption fixes
#'
#' @param doc_xml_path Path to document.xml file
#' @keywords internal
#' @noRd
process_document_xml <- function(doc_xml_path) {
  if (!file.exists(doc_xml_path)) {
    return(invisible(NULL))
  }

  xml_content <- readLines(doc_xml_path, warn = FALSE)
  xml_content <- paste(xml_content, collapse = "\n")
  xml_content <- fix_table_caption_xml(xml_content)
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
