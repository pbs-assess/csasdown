% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set-language-option.R
\name{set_language_option}
\alias{set_language_option}
\title{Parse the file which has YAML option 'french' in it and set option 'french'
accordingly in the current workspace}
\usage{
set_language_option(
  fn = get_index_filename(system.file("rmarkdown", "templates", "resdoc", "skeleton",
    "_bookdown.yml", package = "csasdown")),
  verbose = FALSE
)
}
\arguments{
\item{fn}{The bookdown index filename, typically index.Rmd. This file
must have a YAML option called 'french' set to either 'true' or 'false'}

\item{verbose}{Logical. If \code{TRUE}, print messages}
}
\value{
Nothing
}
\description{
Parse the file which has YAML option 'french' in it and set option 'french'
accordingly in the current workspace
}
\details{
If the YAML option is not found or there is some other issue, the 'french'
option will be set to \code{FALSE}
}
\keyword{internal}
