% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fsar-word.R, R/manureport-word.R,
%   R/resdoc-word.R, R/sr-word.R, R/techreport-word.R
\name{fsar_word}
\alias{fsar_word}
\alias{manureport_word}
\alias{resdoc_word}
\alias{sr_word}
\alias{techreport_word}
\title{Creates an Microsoft Word CSAS-formatted document}
\usage{
fsar_word(...)

manureport_word(...)

resdoc_word(...)

sr_word(...)

techreport_word(...)
}
\arguments{
\item{...}{Other arguments to \code{\link[bookdown:html_document2]{bookdown::word_document2()}}}
}
\value{
A Word Document in the \code{.docx} format based on the FSAR template.

A Word Document in the \code{.docx} format based on the CSAS Res Doc
template
}
\description{
This is a function called in output in the YAML of the \code{index.Rmd} file
to specify the creation of an FSAR word document.

This is a function called in output in the YAML of the \code{index.Rmd} file
to specify the creation of a Microsoft Word version of the Research
Document or Science Response.
}
