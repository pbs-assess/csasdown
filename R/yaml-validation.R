# Optional fields that should not be validated
OPTIONAL_FIELDS <- c("knit", "bibliography", "title")

#' Parse skeleton YAML and extract required fields
#'
#' @param type Document type (resdoc, fsar, sr, techreport, manureport, datareport)
#' @return Character vector of required field names
#' @keywords internal
get_skeleton_fields <- function(type) {
  skeleton_path <- system.file(
    "rmarkdown", "templates", type, "skeleton", "skeleton.Rmd",
    package = "csasdown"
  )

  if (!file.exists(skeleton_path)) {
    cli::cli_abort("Skeleton file not found for document type: {type}")
  }

  yaml_data <- rmarkdown::yaml_front_matter(skeleton_path)

  all_fields <- names(yaml_data)

  required_fields <- setdiff(all_fields, OPTIONAL_FIELDS)

  return(required_fields)
}

#' Validate YAML fields for CSAS documents
#'
#' Dynamically parses `skeleton.Rmd` to determine required fields.
#'
#' @param index_fn Path to the index R Markdown file. Default: "index.Rmd"
#' @param type Document type ("resdoc", "fsar", "sr", "techreport", "manureport", "datareport").
#'   If NULL (default), auto-detects from YAML output field.
#' @param verbose Print informative message on success? Default: FALSE
#'
#' @return Invisibly returns TRUE if validation passes, aborts with
#'   informative error message if validation fails.
#'
#' @details
#' Required fields are dynamically determined by parsing the skeleton.Rmd
#' files for each document type. This ensures the skeletons remain the
#' single source of truth for required YAML fields.
#'
#' All fields in the skeleton are required, including both English and French
#' variants, as they are used in citations and references regardless of the
#' document language setting.
#'
#' @examples
#' \donttest{
#' # Validate before rendering
#' wd <- getwd()
#' example_path <- file.path(tempdir(), "csasdown-example")
#' dir.create(example_path)
#' setwd(example_path)
#' csasdown::draft("resdoc")
#' check_yaml("index.Rmd", verbose = TRUE)
#' setwd(wd)
#' unlink(example_path, recursive = TRUE, force = TRUE)
#' }
#'
#' @export
check_yaml <- function(index_fn = "index.Rmd", type = NULL, verbose = FALSE) {
  if (!file.exists(index_fn)) {
    cli::cli_abort("The file {.file {index_fn}} does not exist.")
  }

  yaml_data <- rmarkdown::yaml_front_matter(index_fn)

  if (is.null(type)) {
    type <- detect_doc_type(index_fn)
  }

  required_fields <- get_skeleton_fields(type)

  missing <- setdiff(required_fields, names(yaml_data))

  if (length(missing) > 0) {
    report_missing_fields(missing, type, index_fn)
  }

  french <- get_language_setting(yaml_data, type)
  if (type == "sr" && french) {
    cli::cli_abort("French Science Responses are not yet supported.")
  }

  if (verbose) {
    cli::cli_inform("YAML validation passed for {type} document.")
  }

  invisible(TRUE)
}

check_bibliography_for_unescaped_doi_angles <- function(index_fn = "index.Rmd") {
  if (!file.exists(index_fn)) {
    cli::cli_abort("The file {.file {index_fn}} does not exist.")
  }

  yaml_data <- rmarkdown::yaml_front_matter(index_fn)
  bibliography <- yaml_data$bibliography

  if (is.null(bibliography)) {
    return(invisible(TRUE))
  }

  bibliography_files <- as.character(unlist(bibliography, use.names = FALSE))
  if (length(bibliography_files) == 0) {
    return(invisible(TRUE))
  }

  index_dir <- normalizePath(dirname(index_fn), winslash = "/", mustWork = TRUE)
  issues <- list()

  for (bib_file in bibliography_files) {
    bib_path <- resolve_bibliography_path(bib_file, index_dir)
    if (!file.exists(bib_path)) {
      next
    }

    bib_text <- paste(readLines(bib_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    file_issues <- find_doi_angle_bracket_issues(bib_text, bib_path)
    if (length(file_issues) > 0) {
      issues <- c(issues, file_issues)
    }
  }

  if (length(issues) > 0) {
    locations <- vapply(
      issues,
      function(issue) paste0(issue$file, ":", issue$line, " (`", issue$value, "`)"),
      character(1)
    )
    cli::cli_abort(c(
      "Found unescaped angle brackets in DOI field(s) in bibliography file(s).",
      "x" = "Raw `<` or `>` in `.bib` DOI values can break Word rendering.",
      "i" = "Replace `<` and `>` with `&lt;` and `&gt;` in DOI fields.",
      "i" = "This often appears later as: `Unescaped '<' not allowed in attributes values`.",
      "i" = paste0("Problem locations:\n- ", paste(locations, collapse = "\n- "))
    ))
  }

  invisible(TRUE)
}

resolve_bibliography_path <- function(path, base_dir) {
  expanded <- path.expand(path)
  if (grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", expanded)) {
    return(normalizePath(expanded, winslash = "/", mustWork = FALSE))
  }

  normalizePath(file.path(base_dir, path), winslash = "/", mustWork = FALSE)
}

find_doi_angle_bracket_issues <- function(text, bib_path) {
  matches <- gregexpr("(?i)\\bdoi\\s*=\\s*([{\"])", text, perl = TRUE)[[1]]
  if (matches[1] == -1) {
    return(list())
  }

  capture_starts <- attr(matches, "capture.start")[, 1]
  capture_lengths <- attr(matches, "capture.length")[, 1]

  issues <- list()

  for (i in seq_along(matches)) {
    if (capture_starts[i] < 0 || capture_lengths[i] < 1) {
      next
    }

    delimiter <- substr(text, capture_starts[i], capture_starts[i] + capture_lengths[i] - 1)
    value_start <- capture_starts[i] + capture_lengths[i]
    bounds <- get_doi_value_bounds(text, delimiter, value_start)
    if (is.null(bounds)) {
      next
    }

    value <- substr(text, bounds$start, bounds$end)
    bad_positions <- gregexpr("[<>]", value, perl = TRUE)[[1]]
    if (bad_positions[1] == -1) {
      next
    }

    first_bad_abs_pos <- bounds$start + bad_positions[1] - 1
    preview <- trimws(gsub("\\s+", " ", value))
    if (nchar(preview) > 80) {
      preview <- paste0(substr(preview, 1, 77), "...")
    }

    issues[[length(issues) + 1]] <- list(
      file = bib_path,
      line = get_line_number(text, first_bad_abs_pos),
      value = preview
    )
  }

  issues
}

get_doi_value_bounds <- function(text, delimiter, value_start) {
  text_len <- nchar(text)
  if (value_start > text_len) {
    return(NULL)
  }

  if (delimiter == "{") {
    depth <- 1L
    pos <- value_start

    while (pos <= text_len && depth > 0L) {
      ch <- substr(text, pos, pos)
      if (ch == "{") {
        depth <- depth + 1L
      } else if (ch == "}") {
        depth <- depth - 1L
      }
      pos <- pos + 1L
    }

    if (depth != 0L) {
      return(NULL)
    }

    return(list(start = value_start, end = pos - 2L))
  }

  pos <- value_start
  previous <- ""

  while (pos <= text_len) {
    ch <- substr(text, pos, pos)
    if (ch == "\"" && previous != "\\") {
      break
    }
    previous <- ch
    pos <- pos + 1L
  }

  if (pos > text_len) {
    return(NULL)
  }

  list(start = value_start, end = pos - 1L)
}

get_line_number <- function(text, position) {
  if (position <= 1L) {
    return(1L)
  }

  prefix <- substr(text, 1, position - 1L)
  newlines <- gregexpr("\n", prefix, fixed = TRUE)[[1]]
  if (newlines[1] == -1) {
    return(1L)
  }

  length(newlines) + 1L
}

get_language_setting <- function(yaml_data, type) {
  if (type == "fsar") return(FALSE)

  output_name <- paste0("csasdown::", type, "_docx")
  if (!is.null(yaml_data$output[[output_name]]$french)) {
    return(isTRUE(yaml_data$output[[output_name]]$french))
  }

  return(FALSE)
}

report_missing_fields <- function(missing, type, index_fn) {
  skeleton_path <- system.file(
    "rmarkdown", "templates", type, "skeleton", "skeleton.Rmd",
    package = "csasdown"
  )

  cli::cli_abort(c(
    "Missing required YAML fields in {.file {index_fn}}:",
    "x" = "The following fields are missing: {.field {missing}}",
    "i" = "Document type: {type}",
    "i" = "See {.file {skeleton_path}} for a complete example."
  ))
}
