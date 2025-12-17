parse_author_field <- function(author_string) {
  if (is.null(author_string) || nchar(trimws(author_string)) == 0) {
    stop("author field is required")
  }

  normalized <- normalize_author_string(author_string)
  conj <- normalized$conjunction

  if (is.null(conj)) {
    english_author <- author_string
    french_author <- author_string
  } else if (conj == "and") {
    english_author <- author_string
    french_author <- swap_conjunction(author_string, "and", "et")
  } else {
    french_author <- author_string
    english_author <- swap_conjunction(author_string, "et", "and")
  }

  english_author_list <- generate_author_citation_list(normalized$authors, "and")
  french_author_list <- generate_author_citation_list(normalized$authors, "et")

  list(
    english_author = english_author,
    french_author = french_author,
    english_author_list = english_author_list,
    french_author_list = french_author_list
  )
}

normalize_author_string <- function(author_string) {
  author_string <- trimws(author_string)

  conj <- detect_conjunction(author_string)

  if (is.null(conj)) {
    return(list(authors = list(author_string), conjunction = NULL))
  }

  if (conj == "and") {
    pattern <- c(", and\\s+", ",\\s*\n\\s*and\\s+", "\n\\s*and\\s+")
  } else {
    pattern <- c(", et\\s+", ",\\s*\n\\s*et\\s+", "\n\\s*et\\s+")
  }

  for (p in pattern) {
    if (grepl(p, author_string, perl = TRUE)) {
      parts <- strsplit(author_string, p, perl = TRUE)[[1]]
      if (length(parts) == 2) {
        before_conj <- parts[1]
        last_author <- parts[2]

        authors_before <- strsplit(before_conj, ",\\s*\n\\s*|,\\s+", perl = TRUE)[[1]]
        authors_before <- trimws(authors_before)
        authors_before <- authors_before[nchar(authors_before) > 0]

        last_author <- trimws(last_author)

        all_authors <- c(authors_before, last_author)
        return(list(authors = as.list(all_authors), conjunction = conj))
      }
    }
  }

  return(list(authors = list(author_string), conjunction = conj))
}

detect_conjunction <- function(author_string) {
  if (grepl(", and\\s+|,?\\s*\n\\s*and\\s+", author_string, perl = TRUE)) {
    return("and")
  } else if (grepl(", et\\s+|,?\\s*\n\\s*et\\s+", author_string, perl = TRUE)) {
    return("et")
  } else {
    return(NULL)
  }
}

swap_conjunction <- function(author_string, from_conj, to_conj) {
  if (from_conj == "and") {
    patterns <- c(", and\\s+", ",\\s*\n\\s*and\\s+", "\n\\s*and\\s+")
    replacements <- c(paste0(", ", to_conj, " "), paste0(",\n", to_conj, " "), paste0("\n", to_conj, " "))
  } else {
    patterns <- c(", et\\s+", ",\\s*\n\\s*et\\s+", "\n\\s*et\\s+")
    replacements <- c(paste0(", ", to_conj, " "), paste0(",\n", to_conj, " "), paste0("\n", to_conj, " "))
  }

  result <- author_string
  for (i in seq_along(patterns)) {
    result <- gsub(patterns[i], replacements[i], result, perl = TRUE)
  }
  result
}

generate_author_citation_list <- function(authors, conjunction) {
  citations <- character(length(authors))
  for (i in seq_along(authors)) {
    parsed <- parse_single_author_name(authors[[i]])
    citations[i] <- convert_to_citation_format(parsed)
  }

  n_authors <- length(citations)
  if (n_authors == 1) {
    return(citations[1])
  } else if (n_authors == 2) {
    return(paste(citations[1], conjunction, citations[2]))
  } else {
    first_part <- paste(citations[1:(n_authors - 1)], collapse = ", ")
    return(paste0(first_part, ", ", conjunction, " ", citations[n_authors]))
  }
}

parse_single_author_name <- function(name_string) {
  name_clean <- gsub("^\\s*(and|et)\\s+", "", name_string)
  name_clean <- gsub(",\\s*$", "", name_clean)
  name_clean <- gsub("\\^[0-9,]+\\^", "", name_clean)
  name_clean <- trimws(name_clean)

  parts <- strsplit(name_clean, "\\s+")[[1]]

  if (length(parts) < 2) {
    stop(paste("Could not parse author name:", name_string))
  }

  has_middle <- length(parts) >= 3 && grepl("^[A-Z]\\.$", parts[2])

  if (has_middle) {
    first <- parts[1]
    middle <- parts[2]
    last <- paste(parts[3:length(parts)], collapse = " ")
  } else {
    first <- parts[1]
    middle <- NA
    last <- paste(parts[2:length(parts)], collapse = " ")
  }

  list(first = first, middle = middle, last = last)
}

convert_to_citation_format <- function(parsed_name) {
  first_initial <- paste0(substr(parsed_name$first, 1, 1), ".")

  if (!is.na(parsed_name$middle)) {
    paste0(parsed_name$last, ", ", first_initial, parsed_name$middle)
  } else {
    paste0(parsed_name$last, ", ", first_initial)
  }
}
