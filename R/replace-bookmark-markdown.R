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
  gsub("([][{}()+*^$|\\?.-])", "\\\\\\1", x)
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

  gsub(
    "<w:rPr>",
    paste0("<w:rPr>", inner),
    fragment,
    fixed = TRUE
  )
}

markdown_to_pandoc_fragments <- function(values) {
  marks <- names(values)
  tmp_md <- tempfile(fileext = ".md")
  tmp_docx <- tempfile(fileext = ".docx")
  tmp_dir <- tempfile()
  dir.create(tmp_dir)

  blocks <- vapply(marks, function(mark) {
    text <- normalize_markdown_text(values[[mark]])
    start <- sprintf("START:%s", mark)
    end <- sprintf("END:%s", mark)
    sprintf("`%s` %s `%s`", start, text, end)
  }, character(1))

  writeLines(blocks, tmp_md, useBytes = TRUE)
  rmarkdown::pandoc_convert(tmp_md, to = "docx", output = tmp_docx)
  utils::unzip(tmp_docx, exdir = tmp_dir)

  md_xml_path <- file.path(tmp_dir, "word", "document.xml")
  md_xml <- paste(readLines(md_xml_path, warn = FALSE), collapse = "")

  out <- stats::setNames(vector("list", length(marks)), marks)

  for (mark in marks) {
    start <- sprintf("START:%s", mark)
    end <- sprintf("END:%s", mark)
    pattern <- sprintf("(?s)%s(.*?)%s", escape_regex(start), escape_regex(end))
    hit <- regmatches(md_xml, regexpr(pattern, md_xml, perl = TRUE))

    if (!length(hit) || !nzchar(hit)) {
      out[[mark]] <- NULL
      next
    }

    fragment <- sub(sprintf("(?s)^%s", escape_regex(start)), "", hit, perl = TRUE)
    fragment <- sub(sprintf("(?s)%s$", escape_regex(end)), "", fragment, perl = TRUE)

    fragment <- sub("^</w:t>", "", fragment)
    fragment <- sub("<w:t[^>]*>$", "", fragment)
    out[[mark]] <- fragment
  }

  unlink(c(tmp_md, tmp_docx, tmp_dir), recursive = TRUE)
  out
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

  print(doc, target = tmp_docx)
  utils::unzip(tmp_docx, exdir = tmp_dir)

  xml_path <- file.path(tmp_dir, "word", "document.xml")
  doc_xml <- paste(readLines(xml_path, warn = FALSE), collapse = "")
  pandoc_fragments <- markdown_to_pandoc_fragments(vals)

  for (bookmark in nms) {
    pattern <- sprintf(
      '(?s)(<w:bookmarkStart[^>]*w:name="%s"[^>]*/>)(.*?)(<w:bookmarkEnd[^>]*/>)',
      bookmark
    )
    hit <- regmatches(doc_xml, regexpr(pattern, doc_xml, perl = TRUE))

    if (!length(hit) || !nzchar(hit)) {
      warning(sprintf("Bookmark '%s' not found.", bookmark), call. = FALSE)
      next
    }

    repl <- pandoc_fragments[[bookmark]]
    if (is.null(repl) || !nzchar(repl)) {
      warning(sprintf("Could not extract pandoc content for bookmark '%s'.", bookmark), call. = FALSE)
      next
    }

    rpr <- regmatches(hit, regexpr("<w:rPr>.*?</w:rPr>", hit, perl = TRUE))
    if (!length(rpr) || !nzchar(rpr)) rpr <- ""

    repl <- merge_run_properties(repl, rpr)
    doc_xml <- sub(pattern, paste0("\\1", repl, "\\3"), doc_xml, perl = TRUE)
  }

  writeLines(doc_xml, xml_path)

  old <- setwd(tmp_dir)
  on.exit(setwd(old), add = TRUE)
  files <- list.files(
    recursive = TRUE,
    full.names = FALSE,
    include.dirs = FALSE,
    all.files = TRUE,
    no.. = TRUE
  )
  utils::zip(zipfile = tmp_docx, files = files, flags = "-q")

  out <- officer::read_docx(tmp_docx)
  unlink(c(tmp_docx, tmp_dir), recursive = TRUE)
  out
}

replace_bookmark_with_markdown <- function(doc, bookmark, text) {
  args <- list(doc = doc)
  args[[bookmark]] <- text
  do.call(replace_bookmarks_with_markdown, args)
}
