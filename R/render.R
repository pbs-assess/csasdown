#' Render a CSAS report
#'
#' This is the main rendering function for reports.
#'
#' @param config_file YAML configuration file.
#' @param verbose Verbose?
#' @param ... Arguments to pass to [bookdown::render_book()].
#'
#' @importFrom cli cli_alert_success cli_inform cli_abort
#'
#' @details
#' `render()` automatically detects
#' document type from the YAML in `index.Rmd` and renders appropriately.
#'
#' `render()` can be called from the command line as `csasdown::render()` or via
#' clicking the RStudio Knit button assuming the following YAML argument is set
#' in `index.Rmd`:
#'
#' ```
#' knit: (function(input, ...) {csasdown::render()})
#' ```
#'
#' @export
#' @returns A rendered `.docx` report.
#' @examples
#' # create a temporary example folder:
#' wd <- getwd()
#' example_path <- file.path(tempdir(), "csasdown-example")
#' dir.create(example_path)
#' setwd(example_path)
#'
#' # create a draft template:
#' csasdown::draft("resdoc")
#'
#' # render:
#' csasdown::render()
#'
#' # return to original working directory after running example:
#' setwd(wd)
#'
#' # clean up:
#' unlink(example_path, recursive = TRUE, force = TRUE)

render <- function(
    config_file = "_bookdown.yml",
    verbose = FALSE,
    ...) {

  type <- detect_doc_type("index.Rmd")
  cli_alert_success("Detected document type: {type}")

  check_yaml(index_fn = "index.Rmd", type = type, verbose = verbose)
  cli_alert_success("YAML validation passed")
  check_bibliography_for_unescaped_doi_angles("index.Rmd")

  if (type == "fsar") {
    return(render_sar(config_file = config_file, validate_bibliography = FALSE, ...))
  }

  output_options <- list(pandoc_args = c("--metadata=title:", "--metadata=abstract:"))
  abstract_state <- NULL
  old_abstract_engine <- NULL

  if (type == "resdoc") {
    abstract_state <- preprocess_resdoc_abstract(config_file = config_file)
    old_abstract_engine <- set_resdoc_abstract_engine(path = abstract_state$abstract_file)

    on.exit({
      writeLines(abstract_state$source_content, con = abstract_state$source_file, useBytes = TRUE)
      restore_resdoc_abstract_engine(old_abstract_engine)
    }, add = TRUE)
  }

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

  cli_alert_success(positive_affirmation())
  invisible()
}

preprocess_resdoc_abstract <- function(config_file = "_bookdown.yml") {
  config <- yaml::read_yaml(config_file)
  rmd_files <- config$rmd_files
  source_file <- NULL
  source_content <- NULL
  abstract_file <- "tmp-abstract.md"

  if (file.exists(abstract_file)) {
    unlink(abstract_file)
  }

  for (candidate in rmd_files) {
    if (identical(basename(candidate), "index.Rmd")) {
      next
    }
    if (!file.exists(candidate)) {
      next
    }

    content <- readLines(candidate, warn = FALSE)
    original_content <- content

    heading_idx <- grep("^#\\s+\\S", content)

    if (length(heading_idx) < 2) {
      next
    }

    start_idx <- heading_idx[1]
    end_idx <- heading_idx[2] - 1

    abstract_body <- content[(start_idx + 1):end_idx]

    # Remove leading empty lines
    while (length(abstract_body) && !nzchar(abstract_body[1])) {
      abstract_body <- abstract_body[-1]
    }

    # Optional: remove trailing empty lines
    while (length(abstract_body) && !nzchar(abstract_body[length(abstract_body)])) {
      abstract_body <- abstract_body[-length(abstract_body)]
    }

    abstract_chunk <- c("```{abstract}", abstract_body, "```")

    before <- if (start_idx > 1) content[seq_len(start_idx - 1)] else character()
    after <- if (end_idx < length(content)) content[(end_idx + 1):length(content)] else character()

    content <- c(before, abstract_chunk, after)

    writeLines(content, con = candidate, useBytes = TRUE)
    source_file <- candidate
    source_content <- original_content
    break
  }

  list(
    source_file = source_file,
    source_content = source_content,
    abstract_file = abstract_file
  )
}

set_resdoc_abstract_engine <- function(path = "tmp-abstract.md") {
  old_engine <- knitr::knit_engines$get("abstract")

  knitr::knit_engines$set(abstract = function(options) {
    processed <- knitr::knit_child(
      text = options$code,
      quiet = TRUE,
      envir = knitr::knit_global()
    )
    writeLines(processed, con = path, useBytes = TRUE)
    ""
  })

  old_engine
}

restore_resdoc_abstract_engine <- function(old_engine) {
  if (is.null(old_engine)) {
    engines <- knitr::knit_engines$get()
    engines$abstract <- NULL
    do.call(knitr::knit_engines$set, engines)
    return(invisible())
  }

  knitr::knit_engines$set(abstract = old_engine)
  invisible()
}

positive_affirmation <- function(success = TRUE) {

  success_msgs <- c(
    "Beautifully done \u2014 you make this look easy \u2728",
    "Excellent work \u2014 the report rendered perfectly \U0001F44F",
    "You're on a roll \u2014 flawless output \U0001F4D8",
    "Nicely handled \u2014 everything compiled smoothly \U0001F44D",
    "You've got the magic touch \u2014 great render \U0001FA84",
    "Calm, competent, and fully rendered \U0001F60C",
    "Another polished document thanks to you \U0001F31F",
    "Steady hands, sharp mind \u2014 perfect output \U0001F4C4",
    "You're making reproducible science look stylish \U0001F4CA",
    "Look at you go \u2014 beautifully knitted \U0001F9F6",
    "Reliable as ever \u2014 fantastic work \U0001F64C",
    "You've done it again \u2014 smooth rendering all the way \U0001F30A",
    "Top-tier work \u2014 the document came together beautifully \U0001F4C4",
    "You make complex things look effortless \u2728",
    "Another clean render under your belt \U0001F44F",
    "Steady, skilled, and successfully compiled \U0001F44D",
    "You've got this down to a science \U0001F52C",
    "Precision work \u2014 it rendered perfectly \U0001F4CA",
    "Dependable as ever \u2014 fantastic output \U0001F31F",
    "You really stitched that together beautifully \U0001F9F6",
    "Threaded the needle and nailed the render \U0001F3AF"
  )

  failure_msgs <- c(
    "Rendering paused its journey \u2014 worth another try.",
    "The document build took an unscheduled break.",
    "Not quite this time \u2014 a rerun may do the trick.",
    "Rendering stopped short of the finish line."
  )

  pick_message <- function(messages) {
    idx <- as.integer(((unclass(Sys.time()) * 1e6) + Sys.getpid()) %% length(messages)) + 1L
    messages[[idx]]
  }

  if (isTRUE(success)) {
    pick_message(success_msgs)
  } else {
    pick_message(failure_msgs)
  }
}
#' Render a SAR/FSAR
#'
#' @param config_file YAML configuration file.
#' @param ... Arguments to pass to [bookdown::render_book()].
#'
#' @keywords internal
#' @noRd
render_sar <- function(config_file = "_bookdown.yml", validate_bibliography = TRUE, ...) {
  cat("\n")

  check_yaml(index_fn = "index.Rmd", type = "fsar", verbose = TRUE)
  cli_alert_success("YAML validation passed")
  if (validate_bibliography) {
    check_bibliography_for_unescaped_doi_angles("index.Rmd")
  }

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

  title_and_context <- c(
    '::: {custom-style="Heading 1"}', x$english_title, ":::\n",
    '::: {custom-style="Heading 2"}', "Context", ":::\n",
    '::: {custom-style="Body Text"}', x$context, ":::\n"
  )

  content <- readLines(con = first_content_fn, warn = FALSE)

  sources <- c(
    "\n## SOURCES OF INFORMATION {-}\n",
    '<div id="refs" custom-style = "citation"></div>\n',
    "\n\\pagebreak\n"
  )

  backmatter <- c(
    "## THIS REPORT IS AVAILABLE FROM THE:{-}\n",
    '::: {custom-style="Body Text + Centered"}',
    x$english_csa_address,
    "E-Mail:", x$email, "\\",
    "Internet address: [www.dfo-mpo.gc.ca/csas-sccs/](www.dfo-mpo.gc.ca/csas-sccs/)\n",
    "ISSN 1919-5087\\",
    paste0("ISBN 978-0-660-xxxxx-x&#9;Cat. No. Fs70-6/", x$year, "-nnnE-PDF\\"),
    "\u00a9 His Majesty the King in Right of Canada, as represented by the Minister of the\\
                  Department of Fisheries and Oceans,", x$year, "\n",
    "DOI: xxx.xxx.xxx\n",
    "This report is published under the [Open Government Licence - Canada](https://open.canada.ca/en/open-government-licence-canada)\n",
    "MOBIUS", # To be replaced below with image using officer
    ":::\n",
    "\nCorrect citation for this publication:\n",
    '::: {custom-style="citation"}',
    paste0("DFO. ", x$year, ". ", x$english_title, ". DFO Can. Sci. Advis. Sec. Sci. Advis. Rep. ", x$year, "/", x$report_number, ". doi: xxx.xxx.xxx."),
    ":::",
    "\n*Aussi disponible en fran\u00e7ais:*\n",
    '::: {custom-style="citation"}',
    paste0("*MPO. ", x$year, ". ", x$french_title, ". Secr. can. des avis sci. du MPO. Avis sci. ", x$year, "/", x$report_number, ". doi: xxx.xxx.xxx.*"),
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

  ## Revert to user input content to avoid stacking of front and backmatter
  writeLines(content, con = first_content_fn)

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
