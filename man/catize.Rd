% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/catize.R
\name{catize}
\alias{catize}
\title{Convert a string including inline knitr code chunks to a cat-like string}
\usage{
catize(str)
}
\arguments{
\item{str}{The string containing inline knitr-style R code
(backtick-enclosed)}
}
\value{
A non-quoted (see \code{\link[=noquote]{noquote()}}) string which can be
enclosed with double-quotes and copy/pasted into a \code{\link[=cat]{cat()}}
or \code{\link[=paste]{paste()}} command
}
\description{
Formats the supplied string so that inline knitr code chunks are replaced
with the format used in commands such as \code{\link[=cat]{cat()}} and \code{\link[=paste]{paste()}} which is a
series of comma-separated strings and R code. Use this function to format
strings from Rmarkdown to something that can be copy/pasted into a \code{\link[=cat]{cat()}}
command inside a knitr chunk.
}
\details{
This is used primarily to convert sections of code that have been written
in Rmarkdown into simple strings with R code embedded between quoted,
comma-separated strings that can be passed to \code{\link[=cat]{cat()}}. Here is a simple
example of the conversion this function does.

Original rmarkdown string:

\if{html}{\out{<div class="sourceCode">}}\preformatted{"The date is `r as.character(Sys.Date())` today. You are on
a `r Sys.info()["sysname"]` machine."
}\if{html}{\out{</div>}}

Modified cat-like string (can be passed to \code{\link[=cat]{cat()}}):

\if{html}{\out{<div class="sourceCode">}}\preformatted{"The date is ", as.character(Sys.Date()), " today. You are on a ",
Sys.info()["sysname"], " machine."
}\if{html}{\out{</div>}}
}
\keyword{internal}
