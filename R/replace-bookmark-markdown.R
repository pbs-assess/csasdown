#' Replace bookmark content with markdown-formatted text
#'
#' Supported syntax:
#' - *italic*
#' - ^superscript^
#' - plain newline -> space
#' - trailing "\" -> line break
#' - line containing only "\" -> one extra blank line separator
#'
#' @param doc An officer rdocx object
#' @param bookmark Character string specifying the bookmark name
#' @param text Character/string vector with limited markdown text
#'
#' @return An officer rdocx object with the bookmark content replaced
#'
#' @keywords internal
#' @noRd

xml_escape_text <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

markdown_to_tokens <- function(text) {
  if (is.null(text)) text <- character()
  if (is.list(text)) text <- unlist(text, recursive = TRUE, use.names = FALSE)
  text <- as.character(text)
  text[is.na(text)] <- ""
  lines <- strsplit(paste(text, collapse = "\n"), "\n", fixed = TRUE)[[1]]

  out <- list()
  push <- function(type, text = "", italic = FALSE, superscript = FALSE) {
    out[[length(out) + 1]] <<- list(
      type = type, text = text, italic = italic, superscript = superscript
    )
  }

  for (i in seq_along(lines)) {
    line <- sub("^\\s+", "", lines[[i]])

    if (grepl("^\\\\$", line)) {
      push("break")
      next
    }

    hard_break <- grepl("\\\\$", line)
    if (hard_break) line <- sub("\\\\$", "", line)

    while (nzchar(line)) {
      m <- regexpr("\\*[^*]+\\*|\\^[^\\^]+\\^", line, perl = TRUE)
      if (m[1] < 0) {
        push("text", line)
        break
      }
      if (m[1] > 1) push("text", substr(line, 1, m[1] - 1))
      tag <- regmatches(line, m)
      inner <- substr(tag, 2, nchar(tag) - 1)
      push("text", inner, italic = startsWith(tag, "*"), superscript = startsWith(tag, "^"))
      line <- substr(line, m[1] + attr(m, "match.length"), nchar(line))
    }

    if (i < length(lines)) {
      push(if (hard_break) "break" else "text", if (hard_break) "" else " ")
    }
  }

  out
}

tokens_to_word_xml <- function(tokens, original_rpr = "") {
  add_rpr <- function(rpr, italic = FALSE, superscript = FALSE) {
    if (!nzchar(rpr)) {
      if (!italic && !superscript) return("")
      rpr <- "<w:rPr></w:rPr>"
    }
    if (italic) rpr <- sub("</w:rPr>", "<w:i/><w:iCs/></w:rPr>", rpr, fixed = TRUE)
    if (superscript) {
      rpr <- sub("</w:rPr>", '<w:vertAlign w:val="superscript"/></w:rPr>', rpr, fixed = TRUE)
    }
    rpr
  }

  paste(vapply(tokens, function(tok) {
    if (identical(tok$type, "break")) {
      return(if (nzchar(original_rpr)) sprintf("<w:r>%s<w:br/></w:r>", original_rpr) else "<w:r><w:br/></w:r>")
    }
    if (!nzchar(tok$text)) return("")
    rpr <- add_rpr(original_rpr, tok$italic, tok$superscript)
    txt <- xml_escape_text(tok$text)
    if (nzchar(rpr)) {
      sprintf('<w:r>%s<w:t xml:space="preserve">%s</w:t></w:r>', rpr, txt)
    } else {
      sprintf('<w:r><w:t xml:space="preserve">%s</w:t></w:r>', txt)
    }
  }, character(1)), collapse = "")
}

replace_bookmark_with_markdown <- function(doc, bookmark, text) {

  browser()

  tmp_docx <- tempfile(fileext = ".docx")
  tmp_dir <- tempfile()
  dir.create(tmp_dir)

  print(doc, target = tmp_docx)
  utils::unzip(tmp_docx, exdir = tmp_dir)

  xml_path <- file.path(tmp_dir, "word", "document.xml")
  doc_xml <- paste(readLines(xml_path, warn = FALSE), collapse = "")

  pattern <- sprintf('(?s)(<w:bookmarkStart[^>]*w:name="%s"[^>]*/>)(.*?)(<w:bookmarkEnd[^>]*/>)', bookmark)
  hit <- regmatches(doc_xml, regexpr(pattern, doc_xml, perl = TRUE))

  if (!length(hit) || !nzchar(hit)) {
    unlink(c(tmp_docx, tmp_dir), recursive = TRUE)
    warning(sprintf("Bookmark '%s' not found.", bookmark), call. = FALSE)
    return(doc)
  }

  rpr <- regmatches(hit, regexpr("<w:rPr>.*?</w:rPr>", hit, perl = TRUE))
  if (!length(rpr) || !nzchar(rpr)) rpr <- ""

  repl <- tokens_to_word_xml(markdown_to_tokens(text), rpr)
  doc_xml <- sub(pattern, paste0("\\1", repl, "\\3"), doc_xml, perl = TRUE)
  writeLines(doc_xml, xml_path)

  old <- setwd(tmp_dir)
  on.exit(setwd(old), add = TRUE)
  utils::zip(
    zipfile = tmp_docx,
    files = list.files(recursive = TRUE, full.names = FALSE, all.files = TRUE, no.. = TRUE),
    flags = "-q"
  )

  out <- officer::read_docx(tmp_docx)
  unlink(c(tmp_docx, tmp_dir), recursive = TRUE)
  out
}
