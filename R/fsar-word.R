#' @rdname csas_docx
#' @importFrom cli cli_inform cli_abort cli_warn
#' @export
fsar_docx <- function(...) {
  .csasdown_docx_base(
    reference_docx = "fsar-template.docx",
    link_citations = FALSE,
    template_dir = "csas-docx",
    use_pandoc_highlight = TRUE,
    ...
  )
}
