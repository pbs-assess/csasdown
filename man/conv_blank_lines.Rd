% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conv-blank-lines.R
\name{conv_blank_lines}
\alias{conv_blank_lines}
\title{Convert blank lines in Rmd code to have WYSIWYG newlines
(WYSIWYG = What You See Is What You Get)}
\usage{
conv_blank_lines(chunk)
}
\arguments{
\item{chunk}{A vector of character strings representing lines for RMD code}
}
\value{
A list of two elements, 1) The corrected part of the chunk and
2) the rest of the chunk starting with the line after the last blank line
}
\description{
Convert (leading) blank lines in Rmd code to WYSIWYG newlines.
The provided chunk will start with a blank line and possibly more or return
\code{NULL} as the converted chunk, and the whole \code{chunk} as the rest. The series
of blank lines will be converted into a mini-chunk, which will be returned
as the first element of a two-element list, the second element is the rest
of the Rmd.
}
\details{
This function should only be used for Rmd code where there are leading blank
lines. The other functions (\code{\link[=conv_list_lines]{conv_list_lines()}}, \code{\link[=conv_header_lines]{conv_header_lines()}},
\code{\link[=conv_type_1_table_lines]{conv_type_1_table_lines()}}, \code{\link[=conv_type_2_table_lines]{conv_type_2_table_lines()}}, and
\code{\link[=conv_paragraph_lines]{conv_paragraph_lines()}} deal with all the blank lines that follow those
pieces of rmarkdown code.
}
\seealso{
Other rmd_conversion_functions: 
\code{\link{conv_header_lines}()},
\code{\link{conv_list_lines}()},
\code{\link{conv_paragraph_lines}()},
\code{\link{conv_type_1_table_lines}()},
\code{\link{conv_type_2_table_lines}()},
\code{\link{convert_newlines_rmd}()}
}
\concept{rmd_conversion_functions}
\keyword{internal}
