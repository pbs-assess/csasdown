#' Copy the csas-style directory from the local library to the current
#' directory
#'
#' @description
#' Copy the csas-style directory from the local library location to
#' the current directory, overwriting and editing the style file if
#' necessary
#'
#' @keywords internal
#'
#' @param copy Logical. If `TRUE`, copy and overwrite if the directory
#' already exists. If `FALSE`, only copy if the directory does not exist in
#' the current directory
#' @param line_nums Logical. If `TRUE`, include line numbering in the document
#' @param line_nums_mod Numerical. Which modulo line numbers to label,
#' e.g. 2 = every second line, 3 = every 3rd line, etc.
#' @param which_sty Name of the style file to modify
#' @param lot_lof Logical. If `TRUE`, include list of tables and list of
#' figures in the document. Implemented only for 'resdoc' and 'techreport'
#' @param draft_watermark Logical. If `TRUE`, show a DRAFT watermark on all
#' pages of the output document
#'
#' @importFrom utils tail
#' @return Nothing
update_csasstyle <- function(copy = TRUE,
                             line_nums = TRUE,
                             line_nums_mod = 1,
                             lot_lof = FALSE,
                             draft_watermark = FALSE,
                             which_sty = "res-doc.sty") {

  fn <- system.file("csas-style", package = "csasdown")
  if(!copy && line_nums){
    bail("You have set `copy` to `FALSE` and `line_nums` to `TRUE` in ",
         "the index.Rmd YAML header. The permanent style file cannot be ",
         "modified as needed to include line numbering. ",
         "Either set `copy` to `TRUE` or `line_nums` to `FALSE` to build.")
  }
  if(!copy && lot_lof){
    bail("You have set `copy` to `FALSE` and `lot_lof` to `TRUE` in the ",
         "index.Rmd YAML header. The permanent style file cannot be ",
         "modified as needed to include the lists of tables and figures. ",
         "Either set `copy` to `TRUE` or `lot_lof` to `FALSE` to build.")
  }
  if(!copy && draft_watermark){
    bail("You have set `copy` to `FALSE` and `draft_watermark` to `TRUE` ",
         "in the index.Rmd YAML header. The permanent style file cannot be ",
         "modified as needed to include the DRAFT watermark. ",
         "Either set `copy` to `TRUE` or `draft_watermark` to `FALSE` to build.")
  }

  if (copy || !dir.exists("csas-style")) {
    dir.create("csas-style", showWarnings = FALSE)
    ignore <- file.copy(fn, ".", overwrite = TRUE, recursive = TRUE)
    if(line_nums || lot_lof || draft_watermark){
      csas_style <- readLines(file.path("csas-style", which_sty))
    }
    if (line_nums) {
      if (grepl("res-doc", which_sty)) {
        frontmatter_loc <- grep("frontmatter\\{", csas_style)
        beg_of_file <- csas_style[seq(1, (frontmatter_loc - 1))]
        end_of_file <- csas_style[seq(frontmatter_loc, length(csas_style))]
        modulo <- paste0("\\modulolinenumbers[", line_nums_mod, "]")
        csas_style <- c(beg_of_file, "\\linenumbers", modulo, end_of_file)
        writeLines(csas_style, file.path("csas-style", which_sty))
      } else {
        modulo <- paste0("\\modulolinenumbers[", line_nums_mod, "]")
        csas_style <- c(csas_style, "\\linenumbers", modulo)
        writeLines(csas_style, file.path("csas-style", which_sty))
      }
    }
    if (lot_lof) {
      if (grepl("res-doc", which_sty) | grepl("tech-report", which_sty)) {
        pagenumbering_loc <- grep("pagenumbering\\{arabic", csas_style)
        beg_of_file <- csas_style[seq(1, (pagenumbering_loc - 1))]
        end_of_file <- csas_style[seq(pagenumbering_loc, length(csas_style))]
        lot <- "\\listoftables"
        cp <- "\\clearpage"
        lof <- "\\listoffigures"
        csas_style <- c(beg_of_file, lot, cp, lof, cp, end_of_file)
        writeLines(csas_style, file.path("csas-style", which_sty))
      } else { # nocov start
        alert("`lot_lof` is only implemented for Res Docs and TechReports.")
      } # nocov end
    }
    if(draft_watermark){
      last_usepackage_ind <- tail(grep("usepackage", csas_style), 1)
      beg_of_file <- csas_style[seq(1, last_usepackage_ind)]
      end_of_file <- csas_style[seq(last_usepackage_ind + 1, length(csas_style))]
      draft_watermark_include <- "\\usepackage{draftwatermark}"
      if(last_usepackage_ind == length(csas_style)){
        csas_style <- c(beg_of_file, draft_watermark_include) # nocov
      }else{
        csas_style <- c(beg_of_file, draft_watermark_include, end_of_file)
      }
      writeLines(csas_style, file.path("csas-style", which_sty))
    }
  }
}
