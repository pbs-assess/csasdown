% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tables.R
\name{csas_table}
\alias{csas_table}
\title{Custom table for csasdown}
\usage{
csas_table(
  x,
  format = "pandoc",
  booktabs = TRUE,
  linesep = "",
  longtable = TRUE,
  font_size = NULL,
  bold_header = TRUE,
  repeat_header = TRUE,
  col_names = NULL,
  col_names_align = "c",
  escape = FALSE,
  hold_position = TRUE,
  extra_header = NULL,
  ex_bold = FALSE,
  ex_italic = FALSE,
  ex_monospace = FALSE,
  ex_underline = FALSE,
  ex_strikeout = FALSE,
  ex_align = "c",
  ex_color = NULL,
  ex_background = NULL,
  ex_font_size = NULL,
  ex_angle = NULL,
  ex_escape = TRUE,
  ex_line = TRUE,
  ex_line_sep = 3,
  ...
)
}
\arguments{
\item{x}{An R object, typically a matrix or data frame.}

\item{format}{As defined by \code{\link[knitr:kable]{knitr::kable()}}.}

\item{booktabs}{As defined by \code{\link[knitr:kable]{knitr::kable()}}.}

\item{linesep}{As defined by \code{\link[knitr:kable]{knitr::kable()}}.}

\item{longtable}{As defined by \code{\link[knitr:kable]{knitr::kable()}}.}

\item{font_size}{Font size in pts. If NULL, document font size is used.}

\item{bold_header}{Make headers bold. Logical}

\item{repeat_header}{If landscape, repeat the header on subsequent pages?}

\item{col_names}{Names for the columns to show on table.}

\item{col_names_align}{As defined in \code{\link[kableExtra:linebreak]{kableExtra::linebreak()}}.}

\item{escape}{As defined by \code{\link[kableExtra:kable_styling]{kableExtra::kable_styling()}}.}

\item{hold_position}{force the table placement to be where the code is called
(don't let latex position the table where it wants)}

\item{extra_header}{character vector of extra headers to be placed above the headers}

\item{ex_bold}{See \code{bold} in kableExtra:::pdfTable_add_header_above()}

\item{ex_italic}{See \code{italic} in kableExtra:::pdfTable_add_header_above()}

\item{ex_monospace}{See \code{monospace} in kableExtra:::pdfTable_add_header_above()}

\item{ex_underline}{See \code{underline} in kableExtra:::pdfTable_add_header_above()}

\item{ex_strikeout}{See \code{strikeout} in kableExtra:::pdfTable_add_header_above()}

\item{ex_align}{See \code{align} in kableExtra:::pdfTable_add_header_above()}

\item{ex_color}{See \code{color} in kableExtra:::pdfTable_add_header_above()}

\item{ex_background}{See \code{background} in kableExtra:::pdfTable_add_header_above()}

\item{ex_font_size}{See \code{font_size} in kableExtra:::pdfTable_add_header_above()}

\item{ex_angle}{See \code{angle} in kableExtra:::pdfTable_add_header_above()}

\item{ex_escape}{See \code{escape} in kableExtra:::pdfTable_add_header_above()}

\item{ex_line}{See \code{line} in kableExtra:::pdfTable_add_header_above()}

\item{ex_line_sep}{See \code{line_sep} in kableExtra:::pdfTable_add_header_above()}

\item{...}{Other arguments passed to \code{\link[knitr:kable]{knitr::kable()}} and kableExtra:::pdfTable_add_header_above()}
}
\description{
This is a custom wrapper for \code{\link[knitr:kable]{knitr::kable()}} with some arguments set so that
the tables work with CSAS formatting in both LaTeX and Word documents.
}
\examples{
csas_table(head(iris))
}
