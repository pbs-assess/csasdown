#' Create a new CSAS document based on a template
#'
#' Create a draft of an R Markdown CSAS document.
#'
#' @param type The type of document to draft. Must be one of `resdoc`, `fsar`,
#' `sr`, `techreport`, `manureport` or `datareport`.
#' @param directory The directory to place the draft document files.
#' Current directory by default
#' @param edit `TRUE` to edit the template immediately.
#' @param create_rstudio_project `TRUE` to create an RStudio project file.
#' @param ... Other arguments to pass to [rmarkdown::draft()].
#'
#' @details This is a light wrapper around [rmarkdown::draft()]. Consult that
#' function for further details.
#' @importFrom cli cli_alert_success
#'
#' @seealso [render()]
#' @returns A new project folder with template files for creating a csasdown
#' report.
#' @examples
#' \donttest{
#' # create a temporary example folder:
#' wd <- getwd()
#' example_path <- file.path(tempdir(), "csasdown-example")
#' dir.create(example_path)
#' setwd(example_path)
#'
#' # create a draft template:
#' csasdown::draft("resdoc")
#'
#' # return to original working directory after running example:
#' setwd(wd)
#'
#' # clean up:
#' unlink(example_path, recursive = TRUE, force = TRUE)
#' }
#' @export
draft <- function(
    type = c("resdoc", "fsar", "sr", "techreport", "manureport", "datareport"),
    directory = ".",
    edit = FALSE,
    create_rstudio_project = TRUE,
    ...) {
  if (!dir.exists(directory)) {
    cli_abort("The directory {directory} does not exist")
  }
  wd <- getwd()
  on.exit(setwd(wd))
  setwd(directory)

  cli_alert_success("Drafting a new {type} project")

  rmarkdown::draft("index.Rmd",
    template = type,
    package = "csasdown",
    edit = edit,
    ...
  )

  if (!file.exists(".gitignore")) {
    gitignore_content <- ".Rproj.user
.Rhistory
.RData
.DS_Store
_book
*.docx
*.pdf
"
    writeLines(gitignore_content, ".gitignore")
    cli_alert_success("Created .gitignore file")
  }

  if (!file.exists(".here")) {
    file.create(".here")
    cli_alert_success("Created .here file")
  }

  if (create_rstudio_project) {
    project_name <- basename(normalizePath("."))
    rproj_file <- paste0(project_name, ".Rproj")

    if (!file.exists(rproj_file)) {
      rproj_content <- "Version: 1.0

RestoreWorkspace: Default
SaveWorkspace: Default
AlwaysSaveHistory: Default

EnableCodeIndexing: Yes
UseSpacesForTab: Yes
NumSpacesForTab: 2
Encoding: UTF-8

RnwWeave: knitr
LaTeX: pdfLaTeX
"
      writeLines(rproj_content, rproj_file)
      cli_alert_success("Created RStudio project file: {rproj_file}")
    } else {
      cli_alert_success("RStudio project file already exists: {rproj_file}")
    }
  }
}
