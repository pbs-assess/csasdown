#' Create a new CSAS document based on a template
#'
#' Create a draft of an R Markdown CSAS document
#'
#' @param type The type of document to draft. Must be one of `resdoc` or
#' `fsar`.
#' @param directory The directory to place the draft document files.
#' Current directory by default
#' @param edit `TRUE` to edit the template immediately.
#' @param ... Other arguments to pass to [rmarkdown::draft()].
#'
#' @details This is a light wrapper around [rmarkdown::draft()]. Consult that
#' function for further details.
#'
#' @examples
#' \dontrun{
#' csasdown::draft("resdoc")
#' csasdown::draft("fsar")
#' }
#' @export
draft <- function(
    type = c("resdoc", "fsar"),
    directory = ".",
    edit = FALSE,
    ...) {
  if (!dir.exists(directory)) {
    cli_abort("The directory {directory} does not exist")
  }
  wd <- getwd()
  on.exit(setwd(wd))
  setwd(directory)

  cli_inform("Drafting a new {type} project ...")

  rmarkdown::draft("index.Rmd",
    template = type,
    package = "csasdown2",
    edit = edit,
    ...
  )

  # rmarkdown::draft() does not copy files that begin with a dot (on Windows)
  # so we just rename the git ignore file
  # if (file.exists("_gitignore")) {
  #   file.rename("_gitignore", ".gitignore")
  # }
  # if (file.exists("_here")) {
  #   file.rename("_here", ".here")
  # }
  # if (create_rstudio_file) create_rstudio_project_file()
}
