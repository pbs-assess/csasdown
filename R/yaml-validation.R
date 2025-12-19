# Optional fields that should not be validated
OPTIONAL_FIELDS <- c("knit", "bibliography", "title")

#' Parse skeleton YAML and extract required fields
#'
#' @param type Document type (resdoc, fsar, sr, techreport)
#' @return Character vector of required field names
#' @keywords internal
get_skeleton_fields <- function(type) {
  skeleton_path <- system.file(
    "rmarkdown", "templates", type, "skeleton", "skeleton.Rmd",
    package = "csasdown2"
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
#' Dynamically parses skeleton.Rmd to determine required fields.
#'
#' @param index_fn Path to the index R Markdown file. Default: "index.Rmd"
#' @param type Document type ("resdoc", "fsar", "sr", "techreport").
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
#' Required fields vary by document type:
#'
#' - **resdoc**: Requires `author`, `year`, `output`, and both English and
#'   French title, address, and region fields
#' - **fsar**: Requires report metadata (title, year, number, etc.) and
#'   context information
#' - **sr**: Requires `year`, `email`, `output`, and both English and French
#'   title and region fields (French SR not yet supported)
#' - **techreport**: Requires `author`, `year`, `report_number`, `abstract`,
#'   `output`, and both English and French metadata fields
#'
#' @examples
#' \dontrun{
#' # Validate before rendering
#' check_yaml("index.Rmd", type = "resdoc", verbose = TRUE)
#'
#' # Auto-detect document type
#' check_yaml()
#' }
#'
#' @keywords internal
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

get_language_setting <- function(yaml_data, type) {
  if (type == "fsar") return(FALSE)

  output_name <- paste0("csasdown2::", type, "_docx")
  if (!is.null(yaml_data$output[[output_name]]$french)) {
    return(isTRUE(yaml_data$output[[output_name]]$french))
  }

  return(FALSE)
}

report_missing_fields <- function(missing, type, index_fn) {
  skeleton_path <- system.file(
    "rmarkdown", "templates", type, "skeleton", "skeleton.Rmd",
    package = "csasdown2"
  )

  cli::cli_abort(c(
    "Missing required YAML fields in {.file {index_fn}}:",
    "x" = "The following fields are missing: {.field {missing}}",
    "i" = "Document type: {type}",
    "i" = "See {.file {skeleton_path}} for a complete example."
  ))
}
