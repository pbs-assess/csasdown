#' Replace bookmark content with markdown-formatted text
#'
#' @param doc An officer rdocx object
#' @param bookmark Character string specifying the bookmark name
#' @param text Character/string vector with markdown text
#'
#' @return An officer rdocx object with the bookmark content replaced
#'
#' @keywords internal
#' @noRd

normalize_markdown_text <- function(text) {
  if (is.null(text)) text <- character()
  if (is.list(text)) text <- unlist(text, recursive = TRUE, use.names = FALSE)
  text <- as.character(text)
  text[is.na(text)] <- ""
  paste(text, collapse = "\n")
}

escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\?.-])", "\\\\\\1", x)
}

replace_once <- function(x, m, replacement) {
  start <- as.integer(m)
  if (start < 0) return(x)
  end <- start + attr(m, "match.length") - 1L
  paste0(substr(x, 1L, start - 1L), replacement, substr(x, end + 1L, nchar(x)))
}

zip_docx_dir <- function(dir, target) {
  if (file.exists(target)) unlink(target)

  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)

  files <- list.files(
    recursive = TRUE,
    full.names = FALSE,
    include.dirs = FALSE,
    all.files = TRUE,
    no.. = TRUE
  )

  utils::zip(
    zipfile = normalizePath(target, winslash = "/", mustWork = FALSE),
    files = files,
    flags = "-q"
  )

  invisible(target)
}

merge_run_properties <- function(fragment, original_rpr = "") {
  if (!nzchar(fragment) || !nzchar(original_rpr)) return(fragment)

  inner <- sub("^<w:rPr>", "", original_rpr)
  inner <- sub("</w:rPr>$", "", inner)

  fragment <- gsub(
    "<w:r>(?!<w:rPr>)",
    paste0("<w:r>", original_rpr),
    fragment,
    perl = TRUE
  )

  gsub("<w:rPr>", paste0("<w:rPr>", inner), fragment, fixed = TRUE)
}

trim_pandoc_fragment_spaces <- function(fragment) {
  if (is.null(fragment) || !nzchar(fragment)) return(fragment)

  fragment <- sub(
    '^(<w:r\\b[^>]*>.*?<w:t[^>]*>)\\s+',
    "\\1",
    fragment,
    perl = TRUE
  )

  sub(
    '\\s+(</w:t>.*?</w:r>)$',
    "\\1",
    fragment,
    perl = TRUE
  )
}

extract_pandoc_fragment <- function(doc_xml, start_marker, end_marker) {
  marker_run <- function(marker) {
    sprintf(
      paste0(
        "<w:r\\b[^>]*>",
        "(?:(?!</w:r>).)*?",
        "<w:t[^>]*>%s</w:t>",
        "(?:(?!</w:r>).)*?",
        "</w:r>"
      ),
      escape_regex(marker)
    )
  }

  start_m <- regexpr(marker_run(start_marker), doc_xml, perl = TRUE)
  if (start_m[1] < 0) return(NULL)

  after_start <- start_m[1] + attr(start_m, "match.length")
  rest <- substr(doc_xml, after_start, nchar(doc_xml))

  end_m <- regexpr(marker_run(end_marker), rest, perl = TRUE)
  if (end_m[1] < 0) return(NULL)

  end_start <- after_start + end_m[1] - 2L
  if (end_start < after_start) return("")

  trim_pandoc_fragment_spaces(substr(doc_xml, after_start, end_start))
}

markdown_to_pandoc_fragments <- function(values) {
  nms <- names(values)
  ids <- sprintf("CSASDOWN_BKM_%03d", seq_along(nms))
  start_markers <- paste0(ids, "_START")
  end_markers <- paste0(ids, "_END")

  tmp_md <- tempfile(fileext = ".md")
  tmp_docx <- tempfile(fileext = ".docx")
  tmp_dir <- tempfile()
  dir.create(tmp_dir)

  on.exit(unlink(c(tmp_md, tmp_docx, tmp_dir), recursive = TRUE), add = TRUE)

  blocks <- unlist(lapply(seq_along(nms), function(i) {
    text <- normalize_markdown_text(values[[nms[i]]])
    c(sprintf("`%s` %s `%s`", start_markers[i], text, end_markers[i]), "")
  }))

  writeLines(blocks, tmp_md, useBytes = TRUE)

  rmarkdown::pandoc_convert(tmp_md, to = "docx", output = tmp_docx)
  utils::unzip(tmp_docx, exdir = tmp_dir)

  md_xml <- paste(
    readLines(file.path(tmp_dir, "word", "document.xml"), warn = FALSE),
    collapse = ""
  )

  out <- stats::setNames(vector("list", length(nms)), nms)

  for (i in seq_along(nms)) {
    out[[nms[i]]] <- extract_pandoc_fragment(
      md_xml,
      start_markers[i],
      end_markers[i]
    )
  }

  out
}

bookmark_match_pattern <- function(bookmark) {
  sprintf(
    paste0(
      '(?s)<w:bookmarkStart\\b',
      '(?=[^>]*\\bw:name="%s")',
      '(?=[^>]*\\bw:id="([^"]+)")',
      '[^>]*/>',
      '.*?',
      '<w:bookmarkEnd\\b',
      '(?=[^>]*\\bw:id="\\1")',
      '[^>]*/>'
    ),
    escape_regex(bookmark)
  )
}

replace_bookmarks_with_markdown <- function(doc, ...) {
  vals <- list(...)
  if (!length(vals)) return(doc)

  nms <- names(vals)

  if (is.null(nms) || any(!nzchar(nms))) {
    stop("All replacements must be named as bookmark = text pairs.", call. = FALSE)
  }

  if (anyDuplicated(nms)) {
    dup <- unique(nms[duplicated(nms)])
    stop(sprintf("Duplicate bookmark name(s): %s", paste(dup, collapse = ", ")), call. = FALSE)
  }

  tmp_docx <- tempfile(fileext = ".docx")
  tmp_dir <- tempfile()
  dir.create(tmp_dir)

  on.exit(unlink(c(tmp_docx, tmp_dir), recursive = TRUE), add = TRUE)

  print(doc, target = tmp_docx)
  utils::unzip(tmp_docx, exdir = tmp_dir)

  xml_path <- file.path(tmp_dir, "word", "document.xml")
  doc_xml <- paste(readLines(xml_path, warn = FALSE), collapse = "")

  pandoc_fragments <- markdown_to_pandoc_fragments(vals)

  for (bookmark in nms) {
    m <- regexpr(bookmark_match_pattern(bookmark), doc_xml, perl = TRUE)

    if (m[1] < 0) {
      warning(sprintf("Bookmark '%s' not found.", bookmark), call. = FALSE)
      next
    }

    hit <- regmatches(doc_xml, m)
    start_tag <- regmatches(hit, regexpr("<w:bookmarkStart\\b[^>]*/>", hit, perl = TRUE))
    end_tag <- regmatches(hit, regexpr("<w:bookmarkEnd\\b[^>]*/>", hit, perl = TRUE))

    repl <- pandoc_fragments[[bookmark]]

    if (is.null(repl)) {
      warning(sprintf("Could not extract pandoc content for bookmark '%s'.", bookmark), call. = FALSE)
      next
    }

    rpr <- regmatches(hit, regexpr("<w:rPr>.*?</w:rPr>", hit, perl = TRUE))
    if (!length(rpr) || !nzchar(rpr)) rpr <- ""

    repl <- merge_run_properties(repl, rpr)
    doc_xml <- replace_once(doc_xml, m, paste0(start_tag, repl, end_tag))
  }

  writeLines(doc_xml, xml_path)

  zip_docx_dir(tmp_dir, tmp_docx)

  officer::read_docx(tmp_docx)
}

replace_bookmark_with_markdown <- function(doc, bookmark, text) {
  args <- list(doc = doc)
  args[[bookmark]] <- text
  do.call(replace_bookmarks_with_markdown, args)
}