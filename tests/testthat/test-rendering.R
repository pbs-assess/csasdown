toggle_french <- function(index_file = "index.Rmd") {
  index_content <- readLines(index_file)
  index_content <- gsub("french: false", "french: true", index_content, fixed = TRUE)
  writeLines(index_content, index_file)
}

set_numeric_year <- function(index_file = "index.Rmd") {
  index_content <- readLines(index_file)
  index_content <- gsub(
    pattern = '^(\\s*year:\\s*)"([0-9]{4})"(\\s*)$',
    replacement = "\\1\\2\\3",
    x = index_content,
    perl = TRUE
  )
  writeLines(index_content, index_file)
}

test_that("sr builds", {
  wd <- getwd()
  testing_path <- file.path(tempdir(), "sr")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  draft("sr", create_dir = FALSE, edit = FALSE)
  render()
  if (FALSE) {
    system("open _book/sr.docx")
  }
  expect_true(file.exists("_book/sr.docx"))
  setwd(wd)
})

test_that("FSAR builds", {
  wd <- getwd()
  testing_path <- file.path(tempdir(), "fsar")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  draft("fsar", create_dir = FALSE, edit = FALSE)
  render()
  if (FALSE) {
    system("open _book/fsar.docx")
  }
  expect_true(file.exists("_book/fsar.docx"))
  setwd(wd)
})

test_that("resdoc builds", {
  wd <- getwd()
  testing_path <- file.path(tempdir(), "resdoc")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft("resdoc", create_dir = FALSE, edit = FALSE))
  render()
  if (FALSE) {
    system("open _book/resdoc.docx")
  }
  expect_true(file.exists("_book/resdoc.docx"))
  setwd(wd)
})

test_that("techreport builds", {
  wd <- getwd()
  testing_path <- file.path(tempdir(), "techreport")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  draft("techreport", create_dir = FALSE, edit = FALSE)
  render()
  if (FALSE) {
    system("open _book/techreport.docx")
  }
  expect_true(file.exists("_book/techreport.docx"))
  setwd(wd)
})

test_that("techreport builds in French", {
  wd <- getwd()
  testing_path <- file.path(tempdir(), "techreport_french")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  draft("techreport", create_dir = FALSE, edit = FALSE)
  toggle_french()
  render()
  if (FALSE) {
    system("open _book/techreport.docx")
  }
  expect_true(file.exists("_book/techreport.docx"))
  setwd(wd)
})

test_that("manureport builds", {
  wd <- getwd()
  testing_path <- file.path(tempdir(), "manureport")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  draft("manureport", create_dir = FALSE, edit = FALSE)
  render()
  if (FALSE) {
    system("open _book/manureport.docx")
  }
  expect_true(file.exists("_book/manureport.docx"))
  setwd(wd)
})

test_that("manureport builds in French", {
  wd <- getwd()
  testing_path <- file.path(tempdir(), "manureport_french")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  draft("manureport", create_dir = FALSE, edit = FALSE)
  toggle_french()
  render()
  if (FALSE) {
    system("open _book/manureport.docx")
  }
  expect_true(file.exists("_book/manureport.docx"))
  setwd(wd)
})

test_that("datareport builds", {
  wd <- getwd()
  testing_path <- file.path(tempdir(), "datareport")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  draft("datareport", create_dir = FALSE, edit = FALSE)
  render()
  if (FALSE) {
    system("open _book/datareport.docx")
  }
  expect_true(file.exists("_book/datareport.docx"))
  setwd(wd)
})

test_that("datareport builds in French", {
  wd <- getwd()
  testing_path <- file.path(tempdir(), "datareport_french")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  draft("datareport", create_dir = FALSE, edit = FALSE)
  toggle_french()
  render()
  if (FALSE) {
    system("open _book/datareport.docx")
  }
  expect_true(file.exists("_book/datareport.docx"))
  setwd(wd)
})

test_that("FSAR builds in French", {
  wd <- getwd()
  testing_path <- file.path(tempdir(), "fsar_french")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  draft("fsar", create_dir = FALSE, edit = FALSE)
  toggle_french()
  render()
  if (FALSE) {
    system("open _book/fsar.docx")
  }
  expect_true(file.exists("_book/fsar.docx"))
  setwd(wd)
})

test_that("resdoc builds in French", {
  wd <- getwd()
  testing_path <- file.path(tempdir(), "resdoc_french")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  draft("resdoc", create_dir = FALSE, edit = FALSE)
  toggle_french()
  render()
  if (FALSE) {
    system("open _book/resdoc.docx")
  }
  expect_true(file.exists("_book/resdoc.docx"))
  setwd(wd)
})

test_that("resdoc builds with numeric year", {
  wd <- getwd()
  testing_path <- file.path(tempdir(), "resdoc_numeric_year")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  draft("resdoc", create_dir = FALSE, edit = FALSE)
  set_numeric_year()
  render()
  expect_true(file.exists("_book/resdoc.docx"))
  setwd(wd)
})
