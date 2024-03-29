% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chunk-header.R
\name{chunk_header_remove}
\alias{chunk_header_remove}
\title{Remove an option and its value from a knitr chunk header}
\usage{
chunk_header_remove(header = NULL, option = NULL)
}
\arguments{
\item{header}{The knitr chunk header string}

\item{option}{The option to remove}
}
\value{
The modified knitr chunk header string
}
\description{
Remove an option and its value from a knitr chunk header
}
\details{
Here is an example of how the option is added. If \code{option} is
"needs_trans" then the text in \code{option} will be removed along with
preceeding comma and space, up to the next comma or closing \verb{\}}
\verb{\{r example, eval = !fr(), needs_trans = TRUE\}` with call: `chunk_header_remove(header, "needs_trans")` }{r example, eval = !fr()}\code{If}header\verb{is not matched by the regular expression used by knitr for beginning chunks [knitr::all_patterns$md$chunk.begin] then a warning will be issued and}header` will be returned.
}
\seealso{
Other knitr-chunk-header: 
\code{\link{chunk_header_add}()},
\code{\link{chunk_header_contains}()}
}
\concept{knitr-chunk-header}
\keyword{internal}
