test_that("resdoc abstract preprocessing and engine capture abstract markdown", {
  old_wd <- getwd()
  tmp <- file.path(tempdir(), "resdoc_abstract_engine")
  unlink(tmp, recursive = TRUE, force = TRUE)
  dir.create(tmp, recursive = TRUE)
  setwd(tmp)
  on.exit(setwd(old_wd), add = TRUE)

  writeLines(c(
    "book_filename: \"resdoc\"",
    "rmd_files:",
    "  - index.Rmd",
    "  - 01-content.Rmd"
  ), "_bookdown.yml")

  writeLines(c(
    "---",
    "title: test",
    "output: html_document",
    "---"
  ), "index.Rmd")

  original_content <- c(
    "# ABSTRACT",
    "",
    "Inline abstract value: `r abstract_value`.",
    "",
    "# INTRODUCTION",
    "",
    "Main content paragraph."
  )
  writeLines(original_content, "01-content.Rmd")

  state <- preprocess_resdoc_abstract("_bookdown.yml")
  on.exit({
    if (file.exists(state$source_file)) {
      writeLines(state$source_content, state$source_file, useBytes = TRUE)
    }
  }, add = TRUE)

  modified_content <- readLines("01-content.Rmd", warn = FALSE)
  expect_false(any(grepl("^# ABSTRACT$", modified_content)))
  expect_true(any(grepl("^```\\{abstract\\}$", modified_content)))
  expect_true(any(grepl("Inline abstract value: `r abstract_value`.", modified_content, fixed = TRUE)))

  assign("abstract_value", 2, envir = knitr::knit_global())
  old_engine <- set_resdoc_abstract_engine(state$abstract_file)
  on.exit({
    restore_resdoc_abstract_engine(old_engine)
    if (exists("abstract_value", envir = knitr::knit_global(), inherits = FALSE)) {
      rm("abstract_value", envir = knitr::knit_global())
    }
  }, add = TRUE)

  rendered <- knitr::knit("01-content.Rmd", quiet = TRUE)
  rendered_lines <- readLines(rendered, warn = FALSE)

  expect_false(any(grepl("Inline abstract value", rendered_lines, fixed = TRUE)))
  expect_true(any(grepl("Main content paragraph", rendered_lines, fixed = TRUE)))

  expect_true(file.exists(state$abstract_file))
  abstract_lines <- readLines(state$abstract_file, warn = FALSE)
  expect_true(any(grepl("Inline abstract value: 2\\.", abstract_lines)))
})
