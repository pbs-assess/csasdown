% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-render-type.R
\name{get_render_type}
\alias{get_render_type}
\title{Get the the type of document to render by extracting from the YAML tags in
the index file}
\usage{
get_render_type(
  fn = get_index_filename(system.file("rmarkdown", "templates", "resdoc", "skeleton",
    "_bookdown.yml", package = "csasdown")),
  verbose = FALSE
)
}
\arguments{
\item{fn}{The name of the YAML file, typically 'index.Rmd' for bookdown}

\item{verbose}{Logical. If \code{TRUE}, print messages}
}
\value{
A character string representing the render type. One of
\code{resdoc_pdf}, \code{resdoc_word}, \code{sr_pdf}, \code{sr_word}, \code{techreport_pdf},
\code{techreport_word}, \code{manureport_pdf},  or \code{manureport_word}
}
\description{
Get the the type of document to render by extracting from the YAML tags in
the index file
}
\details{
Will overwrite the line in the file \code{fn} that contains
\code{csasdown::render_type} or \code{csasdown:::render_type} with
\code{csasdown:::render_type}. This is for backwards compatibility with versions
where those functions were exported.
}
\keyword{internal}
