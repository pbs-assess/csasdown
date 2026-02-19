#' Render a CSAS document
#'
#' Automatically detects document type from YAML and renders appropriately.
#'
#' @param config_file YAML configuration file.
#' @param verbose Verbose?
#' @param ... Arguments to pass to [bookdown::render_book()].
#'
#' @importFrom cli cli_alert_success cli_inform cli_abort
#'
#' @export
render <- function(
    config_file = "_bookdown.yml",
    verbose = FALSE,
    ...) {

  type <- detect_doc_type("index.Rmd")
  cli_alert_success("Detected document type: {type}")

  check_yaml(index_fn = "index.Rmd", type = type, verbose = verbose)
  cli_alert_success("YAML validation passed")

  if (type == "fsar") {
    return(render_sar(config_file = config_file, ...))
  }

  output_options <- list(pandoc_args = c("--metadata=title:", "--metadata=abstract:"))

  cli_inform("Rendering document with bookdown...")
  bookdown::render_book("index.Rmd",
    config_file = config_file,
    envir = parent.frame(n = 2L), # FIXME: needed??
    output_options = output_options,
    ...
  )
  cli_alert_success("Bookdown rendering complete")

  # officedown outputs to the root, not the _book folder like bookdown
  book_filename <- paste0(get_book_filename(config_file), ".docx")
  file.rename(book_filename, file.path("_book", book_filename))
  cli_alert_success("Moved output to _book/{book_filename}")

  if (type == "resdoc") {
    add_resdoc_word_frontmatter2("index.Rmd", yaml_fn = config_file, verbose = verbose, keep_files = FALSE)
  } else if (type == "techreport") {
    add_techreport_word_frontmatter("index.Rmd", yaml_fn = config_file, verbose = verbose, keep_files = FALSE)
  } else if (type == "sr") {
    add_sr_end_matter("index.Rmd", yaml_fn = config_file, verbose = verbose, keep_files = FALSE)
  } else {
    cli_abort("Detected type ({type}) is not supported.")
  }

  # Reset appendix counter for next render
  options(csasdown_current_appendix = NULL)

  # Clean up bookdown artifacts
  unlink(file.path("_book", "reference-keys.txt"))

  cli_alert_success("Render complete!")
  invisible()
}

#' Render a SAR/FSAR
#'
#' @param config_file YAML configuration file.
#' @param ... Arguments to pass to [bookdown::render_book()].
#'
#' @keywords internal
#' @noRd
render_sar <- function(config_file = "_bookdown.yml", ...) {
  cat("\n")

  check_yaml(index_fn = "index.Rmd", type = "fsar", verbose = TRUE)
  cli_alert_success("YAML validation passed")

  # Find out what language is set to and set the option 'french' here
  # so that it works on the first compilation in a workspace
  # It sets `options(french)` to the value in the file
  # set_language_option(index_fn, verbose)

  # Set up the console Render message
  cat("\n")

  y <- yaml::read_yaml("_bookdown.yml")
  first_content_fn <- y$rmd_files[!grepl("index", y$rmd_files)][[1]]

  cli_inform("Pre-processing Rmd files...")

  x <- rmarkdown::yaml_front_matter("index.Rmd")

  extra_context <- paste0(
    "This Science Advisory Report is from the ", x$meeting_date, " ", x$english_title, ". ",
    "Additional publications from this meeting will be posted on the [Fisheries and Oceans Canada (DFO) Science Advisory Schedule](http://www.isdm-gdsi.gc.ca/csas-sccs/applications/events-evenements/index-eng.asp) as they become available."
  )

  title_and_context <- c(
    '::: {custom-style="Heading 1"}', x$english_title, ":::\n",
    '::: {custom-style="Heading 2"}', "Context", ":::\n",
    '::: {custom-style="Body Text"}', paste0(x$context, extra_context), ":::\n"
  )

  content <- readLines(con = first_content_fn, warn = FALSE)

  sources <- c(
    "\n## SOURCES OF INFORMATION {-}\n",
    '<div id="refs" custom-style = "citation"></div>\n',
    extra_context,
    "\n\\pagebreak\n"
  )

  backmatter <- c(
    "## THIS REPORT IS AVAILABLE FROM THE:{-}\n",
    '::: {custom-style="Body Text + Centered"}',
    x$english_csa_address,
    "E-Mail:", x$email, "\\",
    "Internet address: [www.dfo-mpo.gc.ca/csas-sccs/](www.dfo-mpo.gc.ca/csas-sccs/)\n",
    "ISSN xxxx-xxxx\\",
    paste0("ISBN 978-0-660-xxxxx-x&#9;Cat. No. Fs70-6/", x$year, "-nnnE-PDF\\"),
    "\u00a9 His Majesty the King in Right of Canada, as represented by the Minister of the\\
                  Department of Fisheries and Oceans,", x$year, "\n",
    "This report is published under the [Open Government Licence - Canada](https://open.canada.ca/en/open-government-licence-canada)\n",
    "MOBIUS", # To be replaced below with image using officer
    ":::\n",
    "\nCorrect citation for this publication:\n",
    '::: {custom-style="citation"}',
    paste0("DFO. ", x$year, ". ", x$english_title, ". DFO Can. Sci. Advis. Sec. Sci. Advis. Rep. ", x$year, "/", x$report_number, ". iv + xx p."),
    ":::",
    "\n*Aussi disponible en fran\u00e7ais:*\n",
    '::: {custom-style="citation"}',
    paste0("*MPO. ", x$year, ". ", x$french_title, ". Secr. can. des avis sci. du MPO. Avis sci. ", x$year, "/", x$report_number, ". iv + xx p.*"),
    ":::",
    "\nInuktitut Atuinnaummijuq:\n",
    '::: {custom-style="citation"}',
    x$inuktitut_citation,
    ":::"
  )

  writeLines(c(title_and_context, content, sources, backmatter), con = first_content_fn)
  cli_alert_success("Pre-processing complete")

  cli_inform("Rendering the FSAR document with bookdown...")

  render_book("index.Rmd",
    config_file = "_bookdown.yml",
    ...
  )

  ## officedown outputs to the root, not the _book folder like bookdown
  if (file.exists("fsar.docx")) {
    file.rename("fsar.docx", file.path("_book", "fsar.docx"))
  }

  fn <- "_book/fsar.docx"
  if (file.exists(fn)) {
    cli_alert_success("Bookdown rendering complete")
  } else {
    cli_abort("Document not created; something went wrong.")
  }

  cli_inform("Updating headers and footers...")
  doc <- officer::read_docx("_book/fsar.docx")
  doc <- officer::headers_replace_text_at_bkm(doc, "region_name", x$english_region)
  doc <- officer::headers_replace_text_at_bkm(doc, "region_name_rest", x$english_region) # non-first page
  doc <- officer::headers_replace_text_at_bkm(doc, "short_title", x$english_title_short) # non-first page
  doc <- officer::headers_replace_text_at_bkm(doc, "report_year", as.character(x$year))
  doc <- officer::headers_replace_text_at_bkm(doc, "report_number", as.character(x$report_number))
  doc <- officer::footers_replace_text_at_bkm(doc, "release_month", x$release_month)
  doc <- officer::footers_replace_text_at_bkm(doc, "release_year", as.character(x$year))
  cli_alert_success("Headers and footers updated")

  cli_inform("Adding Mobius loop graphic...")
  ## Insert mobius loop image using doc with just the image with the proper style and alt text
  doc <- officer::cursor_reach(doc, keyword = "MOBIUS") |>
    officer::body_remove() |>
    officer::cursor_backward() |>
    officer::body_add_docx(src = system.file("graphics", "mobius_loop.docx", package = "csasdown"))

  print(doc, target = "_book/fsar.docx")
  cli_alert_success("Mobius loop graphic added")

  # Clean up bookdown artifacts
  unlink(file.path("_book", "reference-keys.txt"))

  cli_alert_success("Render complete!")

  # Reset appendix counter for next render
  options(csasdown_current_appendix = NULL)

  invisible()
}

render_fsar <- render_sar

detect_doc_type <- function(index_fn = "index.Rmd") {
  if (!file.exists(index_fn)) {
    cli_abort("The file {.var index_fn} does not exist.")
  }

  x <- rmarkdown::yaml_front_matter(index_fn)

  if (is.null(x$output)) {
    cli_abort("No output field found in YAML front matter of {index_fn}")
  }

  output_names <- names(x$output)

  if (is.null(output_names)) {
    output_names <- as.character(x$output)
  }

  if (any(grepl("resdoc_docx", output_names))) {
    return("resdoc")
  } else if (any(grepl("fsar_docx", output_names))) {
    return("fsar")
  } else if (any(grepl("sr_docx", output_names))) {
    return("sr")
  } else if (any(grepl("techreport_docx", output_names))) {
    return("techreport")
  } else {
    cli_abort("Could not detect document type from YAML output field. Expected one of: resdoc_docx, fsar_docx, or techreport_docx")
  }
}

get_book_filename <- function(fn = "_bookdown.yml") {
  if (!file.exists(fn)) {
    cli_abort("The file {.var fn} does not exist.")
  }
  yml <- readLines(fn)
  trim_yml <- trimws(yml)

  # Get the document type from the `book_filename:` YAML tag
  filename_type_pat <- 'book_filename: \\"([^\\"]*)\\"'
  filename_ind <- grep(filename_type_pat, trim_yml)
  gsub(filename_type_pat, "\\1", trimws(yml[filename_ind]))
}
