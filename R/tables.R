#' Custom table for csasdown
#'
#' This is a custom wrapper for [knitr::kable()] with some arguments set so that
#' the tables work with CSAS formatting in both LaTeX and Word documents.
#'
#' @param x An R object, typically a matrix or data frame.
#' @param format As defined by [knitr::kable()].
#' @param booktabs As defined by [knitr::kable()].
#' @param linesep As defined by [knitr::kable()].
#' @param longtable As defined by [knitr::kable()].
#' @param font_size Font size in pts. If NULL, document font size is used.
#' @param bold_header Make headers bold. Logical
#' @param repeat_header If landscape, repeat the header on subsequent pages?
#' @param col_names Names for the columns to show on table.
#' @param col_names_align As defined in [kableExtra::linebreak()].
#' @param escape As defined by [kableExtra::kable_styling()].
#' @param hold_position force the table placement to be where the code is
#' called (don't let latex position the table where it wants)
#' @param show_continued_text Logical. If `TRUE`, show the
#' 'Continued on next page ...' and '... Continued from previous page'.
#' If this function is called from within an RMarkdown build, this will
#' be set to the value of the YAML option of the same name unless a call
#' to this function within that build supplies this argument as the
#' opposite of the YAML-set value
#' if `format` = "latex"
#' @param extra_header character vector of extra headers to be placed above
#' the headers
#' @param ... Other arguments passed to [knitr::kable()] and
#' kableExtra:::pdfTable_add_header_above()
#' @param ex_bold See `bold` in kableExtra:::pdfTable_add_header_above()
#' @param ex_italic See `italic` in kableExtra:::pdfTable_add_header_above()
#' @param ex_monospace See `monospace` in kableExtra:::pdfTable_add_header_above()
#' @param ex_underline See `underline` in kableExtra:::pdfTable_add_header_above()
#' @param ex_strikeout See `strikeout` in kableExtra:::pdfTable_add_header_above()
#' @param ex_align See `align` in kableExtra:::pdfTable_add_header_above()
#' @param ex_color See `color` in kableExtra:::pdfTable_add_header_above()
#' @param ex_background See `background` in kableExtra:::pdfTable_add_header_above()
#' @param ex_font_size See `font_size` in kableExtra:::pdfTable_add_header_above()
#' @param ex_angle See `angle` in kableExtra:::pdfTable_add_header_above()
#' @param ex_escape See `escape` in kableExtra:::pdfTable_add_header_above()
#' @param ex_line See `line` in kableExtra:::pdfTable_add_header_above()
#' @param ex_line_sep See `line_sep` in kableExtra:::pdfTable_add_header_above()
#' @param cols_no_format A vector of names of numeric columns to not
#' apply `big.mark` and `decimal.mark` to. The function will attempt to
#' detect year columns by default and not apply formatting to those, but
#' this argument, if present, will be added to those
#' @param cols_to_format A vector of the names of columns to format in case
#' they are left unformatted by the year detection algorithm. As long as the
#' columns are numeric, (`is.numeric() == TRUE`), formatting will be applied
#' to these columns
#'
#' @importFrom knitr kable
#' @importFrom kableExtra row_spec kable_styling landscape linebreak
#' @importFrom purrr map map2 map_chr map_lgl map_df
#' @importFrom rlang `%||%`
#' @examples
#' csasdown::csas_table(head(iris))
#' @export
csas_table <- function(x,
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
                       show_continued_text =
                         rmarkdown::metadata$show_continued_text %||% TRUE,
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
                       cols_no_format = NULL,
                       cols_to_format = NULL,
                       ...) {

  # Language format list
  dec_format <- list(decimal.mark = ifelse(fr(), ",", "."),
                     big.mark = ifelse(fr(), " ", ","))

  # Check to make sure the names supplied by cols_no_format are actually in the data frame
  names_exist <- map_lgl(cols_no_format, ~{
    .x %in% names(x)
  })
  if(!all(names_exist)){
    bail("One or more of the columns supplied in ",
         csas_color("cols_no_format"), " are not in the data frame.",
         "The column(s) are:\n",
         csas_color(paste(cols_no_format[!names_exist], collapse = ", ")))
  }
  # Check to make sure the names supplied by cols_to_format are actually in the data frame
  names_exist <- map_lgl(cols_to_format, ~{
    .x %in% names(x)
  })
  if(!all(names_exist)){
    bail("One or more of the columns supplied in ",
         csas_color("cols_no_format"), " are not in the data frame.",
         "The column(s) are:\n",
         csas_color(paste(cols_to_format[!names_exist], collapse = ", ")))
  }

  year_col_names <- unique(c(year_cols(x), cols_no_format))
  year_col_names <- setdiff(year_col_names, cols_to_format)

  if(!is.null(year_col_names)){
    # Apply type change to the year columns from numeric to character
    # so that formatting is not applied to the year cols
    j <- map_lgl(names(x), ~{
      .x %in% year_col_names
    })
    x[j] <- map_df(x[j], ~{
      .x <- as.character(.x)
    })
  }
  if (!is.null(col_names)) {
    # Check for newlines in column headers and convert to proper latex linebreaks
    # See 'Insert linebreak in table' section in the following
    # http://haozhu233.github.io/kableExtra/best_practice_for_newline_in_latex_table.pdf
    if (length(grep("\n", col_names))) {
      # Only use kableExtra if there are newlines
      col_names <- linebreak(col_names, align = col_names_align)
    }
    k <- kable(x = x,
               format = format,
               booktabs = booktabs,
               linesep = linesep,
               longtable = longtable,
               col.names = col_names,
               escape = escape,
               format.args = dec_format,
               ...
    )
    suppressWarnings(k <- kable_styling(k, font_size = font_size))
  } else {
    k <- kable(x = x,
               format = format,
               booktabs = booktabs,
               linesep = linesep,
               longtable = longtable,
               escape = escape,
               format.args = dec_format,
               ...
    )
    suppressWarnings(k <- kable_styling(k, font_size = font_size))
  }

  if (bold_header) {
    if(format == "latex"){
      alert("Bold headers not supported for the ",
            csas_color("latex"), " format.\n",
            "You must bold them manually by pasting latex macros around ",
            " them and passing using the `col.names` argument.")
    }else{
      suppressWarnings(k <- row_spec(k, 0, bold = TRUE))
    }
  }
  if (repeat_header) {
    suppressWarnings(
      k <- kable_styling(k,
        latex_options = "repeat_header",
        repeat_header_continued = FALSE,
        repeat_header_text = "",
        repeat_header_method = "replace"
      )
    )
  }
  suppressWarnings(k <- kable_styling(k, font_size = font_size))
  if (hold_position) {
    suppressWarnings(k <- kable_styling(k, latex_options = "hold_position"))
  }
  k <- sub("\\caption\\[\\]\\{\\}", "\\caption*{}", k)
  if (!is.null(extra_header)) {
    kable_format <- attr(k, "format")
    if (kable_format != "latex") {
      bail("Adding an extra header is only supported for latex tables ",
           "(format = ", csas_color("latex"), ").")
    }
    k <- add_extra_header(k,
      header = extra_header,
      ex_bold,
      ex_italic,
      ex_monospace,
      ex_underline,
      ex_strikeout,
      ex_align,
      ex_color,
      ex_background,
      ex_font_size,
      ex_angle,
      ex_escape,
      ex_line,
      ex_line_sep
    )
  }

  # Insert `Continued on next page ...` and
  # `... Continued from previous page` for latex
  if(!is.logical(show_continued_text)){
    stop("The `show_continued_text` value is non-logical. If setting in ",
         "YAML, use `true` or `false`.")
  }
  if(format == "latex" && show_continued_text){

    k_lines <- strsplit(k, "\n")[[1]]

    # Add Continued on next page...
    j <- grep("endhead", k_lines)
    if(length(j) > 1){
      alert(csas_color("endhead"), " found more than once in the table latex, ",
            "cannot add ", csas_color("Continued on next page..."),
            " text to table")
      return(k)
    }
    if(!length(j)){
      attributes(k_lines) <- attributes(k)
      return(k_lines)
    }
    k_lines_pre <- k_lines[1:j]
    k_lines_post <- k_lines[(j + 1):length(k_lines)]
    if(getOption("french", default = FALSE)){
      new_line_latex <- paste0("\\hline \\multicolumn{",
                               ncol(x),
                               "}{l}{\\textit{Suite \u00e0 la page suivante ...}}")
    }else{
      new_line_latex <- paste0("\\hline \\multicolumn{",
                               ncol(x),
                               "}{l}{\\textit{Continued on next page ...}}")
    }
    if(k_lines_post[1] == ""){
      k_lines_post <- k_lines_post[-1]
    }
    k_lines_post <- c("\\\\", k_lines_post)
    k_lines <- c(k_lines_pre, new_line_latex, k_lines_post)

    # Add Continued from previous page...
    j <- grep("endfirsthead", k_lines)
    if(length(j) > 1){
      alert(csas_color("endfirsthead"), " found more than once in the table ",
            "latex, cannot add ",
            csas_color("Continued from previous page..."), " text to table")
      return(k)
    }
    k_lines_pre <- k_lines[1:j]
    k_lines_post <- k_lines[(j + 1):length(k_lines)]
    if(getOption("french", default = FALSE)){
      new_line_latex <- paste0("\\multicolumn{",
                               ncol(x),
                               "}{l}{\\textit{... Suite de la page pr\u00e9c\u00e9dente}} \\\\ \\hline")
    }else{
      new_line_latex <- paste0("\\multicolumn{",
                               ncol(x),
                               "}{l}{\\textit{... Continued from previous page}} \\\\ \\hline")
    }
    k_lines <- c(k_lines_pre, new_line_latex, k_lines_post)

    k_lines_str <- paste(k_lines, collapse = " ")
    attributes(k_lines_str) <- attributes(k)
    k <- k_lines_str
  }

  k
}

#' Adds an extra header to the top of a [csas_table()]. Works for longpages.
#'
#' @param kable_input An R object, typically a matrix or data frame.
#' @param header a vector of character strings to use for the extra header names
#' @param bold See kableExtra:::pdfTable_add_header_above()
#' @param italic See kableExtra:::pdfTable_add_header_above()
#' @param monospace See kableExtra:::pdfTable_add_header_above()
#' @param underline See kableExtra:::pdfTable_add_header_above()
#' @param strikeout See kableExtra:::pdfTable_add_header_above()
#' @param align See kableExtra:::pdfTable_add_header_above()
#' @param color See kableExtra:::pdfTable_add_header_above()
#' @param background See kableExtra:::pdfTable_add_header_above()
#' @param font_size See kableExtra:::pdfTable_add_header_above()
#' @param angle See kableExtra:::pdfTable_add_header_above()
#' @param escape See kableExtra:::pdfTable_add_header_above()
#' @param line See kableExtra:::pdfTable_add_header_above()
#' @param line_sep See kableExtra:::pdfTable_add_header_above()
#'
#' @importFrom kableExtra magic_mirror
#' @return See kableExtra:::pdfTable_add_header_above()
add_extra_header <- function(kable_input,
                             header = NULL,
                             bold = FALSE,
                             italic = FALSE,
                             monospace = FALSE,
                             underline = FALSE,
                             strikeout = FALSE,
                             align = c("c", "l", "r"),
                             color = NULL,
                             background,
                             font_size,
                             angle,
                             escape,
                             line = TRUE,
                             line_sep = 3) {

  tryCatch({align <- match.arg(align)
  }, error = function(e){
    bail(csas_color("align"), " must be one of ",
         csas_color("c"), ", ", csas_color("l"), ", or ",
         csas_color("r"), ".\n",
         "You tried: ", csas_color(align))
  })

  table_info <- magic_mirror(kable_input)
  header <- standardize_header_input(header)
  if (length(table_info$colnames) != nrow(header)) {
    # nocov start
    bail("The number of extra headers supplied is not the same as the ",
         "number of columns in the table")
    # nocov end
  }
  if (escape) {
    header$header <- input_escape(header$header, align)
  }

  hline_type <- switch(table_info$booktabs + 1,
    "\\\\hline",
    "\\\\toprule"
  )
  new_header_split <-
    pdfTable_new_header_generator(header,
      table_info$booktabs,
      bold,
      italic,
      monospace,
      underline,
      strikeout,
      align,
      color,
      background,
      font_size,
      angle,
      line_sep,
      border_left = FALSE,
      border_right = FALSE
    )
  if (line) {
    new_header <- paste0(
      new_header_split[1], "\n",
      new_header_split[2]
    )
  } else {
    new_header <- new_header_split[1] # nocov
  }
  j <- utf8_inp <- solve_enc(kable_input)
  out <- stringr::str_replace_all(
    utf8_inp,
    hline_type,
    paste0(hline_type, "\n", new_header)
  )
  out <- structure(out,
    format = "latex",
    class = "knitr_kable"
  )

  if (is.null(table_info$new_header_row)) {
    table_info$new_header_row <- new_header_split[1]
    table_info$header_df <- list(header)
  } else {
    # nocov start
    table_info$new_header_row <- c(
      table_info$new_header_row,
      new_header_split[1])
    table_info$header_df[[length(table_info$header_df) + 1]] <- header
    # nocov end
  }
  attr(out, "kable_meta") <- table_info
  out
}
