#' @rdname csas_docx
#' @export
sr_word <- function(...) {
  file <- if (fr()) "SRR-RS2024-fra.docx" else "SRR-RS2024-eng.docx"
  base <- word_document2(...,
                         reference_docx = system.file("csas-docx",
                                                      file,
                                                      package = "csasdown"))

  base$knitr$opts_chunk$comment <- NA
  base$knitr$opts_chunk$fig.align <- "center"
  base
}

