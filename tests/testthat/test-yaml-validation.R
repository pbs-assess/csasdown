test_that("check_yaml detects missing common fields in resdoc", {
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "index.Rmd")

  yaml_content <- '---
english_title: "Test Title"
english_address: "Test Address"
english_region: "Test Region"
output:
  csasdown::resdoc_docx:
    french: false
---
'
  writeLines(yaml_content, temp_file)

  expect_error(
    check_yaml(index_fn = temp_file, type = "resdoc"),
    "Missing required YAML fields"
  )
  expect_error(
    check_yaml(index_fn = temp_file, type = "resdoc"),
    "author"
  )
  expect_error(
    check_yaml(index_fn = temp_file, type = "resdoc"),
    "year"
  )

  unlink(temp_file)
})

test_that("check_yaml detects missing English-specific fields", {
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "index.Rmd")

  yaml_content <- '---
author: "Test Author"
year: "2025"
output:
  csasdown::resdoc_docx:
    french: false
---
'
  writeLines(yaml_content, temp_file)

  expect_error(
    check_yaml(index_fn = temp_file, type = "resdoc"),
    "Missing required YAML fields"
  )
  expect_error(
    check_yaml(index_fn = temp_file, type = "resdoc"),
    "english_title"
  )

  unlink(temp_file)
})

test_that("check_yaml detects missing French-specific fields", {
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "index.Rmd")

  yaml_content <- '---
author: "Test Author"
year: "2025"
output:
  csasdown::resdoc_docx:
    french: true
---
'
  writeLines(yaml_content, temp_file)

  expect_error(
    check_yaml(index_fn = temp_file, type = "resdoc"),
    "Missing required YAML fields"
  )
  expect_error(
    check_yaml(index_fn = temp_file, type = "resdoc"),
    "french_title"
  )

  unlink(temp_file)
})

test_that("check_yaml passes for complete resdoc YAML", {
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "index.Rmd")

  yaml_content <- '---
author: "Test Author"
year: "2025"
english_title: "Test Title"
french_title: "Titre de test"
english_address: "Test Address"
french_address: "Adresse de test"
english_region: "Test Region"
french_region: "Région de test"
output:
  csasdown::resdoc_docx:
    french: false
---
'
  writeLines(yaml_content, temp_file)

  expect_invisible(check_yaml(index_fn = temp_file, type = "resdoc"))
  expect_true(check_yaml(index_fn = temp_file, type = "resdoc"))

  unlink(temp_file)
})

test_that("check_yaml handles FSAR correctly", {
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "index.Rmd")

  yaml_content <- '---
english_title: "Test Report"
output:
  csasdown::fsar_docx
---
'
  writeLines(yaml_content, temp_file)

  expect_error(
    check_yaml(index_fn = temp_file, type = "fsar"),
    "Missing required YAML fields"
  )
  expect_error(
    check_yaml(index_fn = temp_file, type = "fsar"),
    "report_number"
  )

  unlink(temp_file)
})

test_that("check_yaml passes for complete FSAR YAML", {
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "index.Rmd")

  yaml_content <- '---
english_title: "Test Report"
french_title: "Titre français"
english_title_short: "Short Title"
french_title_short: "Short Title"
english_region: "Test Region"
french_region: "Région de test"
year: "2025"
release_month: "May"
meeting_date: "May 1, 2025"
report_number: "001"
email: "test@example.com"
english_csa_address: "Test Address"
french_csa_address: "Adresse de test"
context: "This is the context."
inuktitut_citation: "Inuktitut text"
output:
  csasdown::fsar_docx
---
'
  writeLines(yaml_content, temp_file)

  expect_invisible(check_yaml(index_fn = temp_file, type = "fsar"))
  expect_true(check_yaml(index_fn = temp_file, type = "fsar"))

  unlink(temp_file)
})

test_that("check_yaml rejects French SR", {
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "index.Rmd")

  yaml_content <- '---
year: "2025"
email: "test@example.com"
english_title: "English Title"
french_title: "Titre français"
english_title_short: "Short"
french_title_short: "Titre court"
english_region: "Region"
french_region: "Région"
english_csa_address: "Address"
french_csa_address: "Adresse"
english_month: "May"
french_month: "Mai"
output:
  csasdown::sr_docx:
    french: true
---
'
  writeLines(yaml_content, temp_file)

  expect_error(
    check_yaml(index_fn = temp_file, type = "sr"),
    "French Science Responses are not yet supported"
  )

  unlink(temp_file)
})

test_that("check_yaml auto-detects document type", {
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "index.Rmd")

  yaml_content <- '---
author: "Test Author"
year: "2025"
english_title: "Test Title"
french_title: "Titre de test"
english_address: "Test Address"
french_address: "Adresse de test"
english_region: "Test Region"
french_region: "Région de test"
output:
  csasdown::resdoc_docx:
    french: false
---
'
  writeLines(yaml_content, temp_file)

  expect_invisible(check_yaml(index_fn = temp_file, type = NULL))
  expect_true(check_yaml(index_fn = temp_file))

  unlink(temp_file)
})

test_that("check_yaml handles missing file", {
  expect_error(
    check_yaml(index_fn = "nonexistent_file.Rmd"),
    "does not exist"
  )
})

test_that("check_yaml handles unknown document type", {
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "index.Rmd")

  yaml_content <- '---
title: "Test"
output:
  html_document
---
'
  writeLines(yaml_content, temp_file)

  expect_error(
    check_yaml(index_fn = temp_file, type = "unknown_type"),
    "Skeleton file not found"
  )

  unlink(temp_file)
})

test_that("check_yaml verbose mode works", {
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "index.Rmd")

  yaml_content <- '---
author: "Test Author"
year: "2025"
english_title: "Test Title"
french_title: "Titre de test"
english_address: "Test Address"
french_address: "Adresse de test"
english_region: "Test Region"
french_region: "Région de test"
output:
  csasdown::resdoc_docx:
    french: false
---
'
  writeLines(yaml_content, temp_file)

  expect_message(
    check_yaml(index_fn = temp_file, type = "resdoc", verbose = TRUE),
    "YAML validation passed"
  )

  unlink(temp_file)
})

test_that("check_yaml handles techreport correctly", {
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "index.Rmd")

  yaml_content <- '---
author: "Test Author"
year: "2025"
report_number: "001"
output:
  csasdown::techreport_docx:
    french: false
---
'
  writeLines(yaml_content, temp_file)

  expect_error(
    check_yaml(index_fn = temp_file, type = "techreport"),
    "Missing required YAML fields"
  )
  expect_error(
    check_yaml(index_fn = temp_file, type = "techreport"),
    "abstract"
  )
  expect_error(
    check_yaml(index_fn = temp_file, type = "techreport"),
    "english_doi"
  )

  unlink(temp_file)
})

test_that("check_yaml passes for complete techreport YAML", {
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "index.Rmd")

  yaml_content <- '---
author: "Test Author"
year: "2025"
report_number: "001"
abstract: "This is the abstract."
english_title: "Test Title"
french_title: "Titre de test"
english_address: "Test Address"
french_address: "Adresse de test"
english_region: "Test Region"
french_region: "Région de test"
english_preamble_pages: "iv"
french_preamble_pages: "iv"
english_content_pages: "100"
french_content_pages: "100"
english_doi: "10.1234/test"
french_doi: "10.1234/test"
english_isbn: "978-0-12345-678-9"
french_isbn: "978-0-12345-678-9"
english_cat_no: "Cat123"
french_cat_no: "Cat123"
output:
  csasdown::techreport_docx:
    french: false
---
'
  writeLines(yaml_content, temp_file)

  expect_invisible(check_yaml(index_fn = temp_file, type = "techreport"))
  expect_true(check_yaml(index_fn = temp_file, type = "techreport"))

  unlink(temp_file)
})

test_that("check_yaml passes for complete SR YAML", {
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "index.Rmd")

  yaml_content <- '---
year: "2025"
email: "test@example.com"
english_title: "Test Title"
french_title: "Titre de test"
english_title_short: "Short Title"
french_title_short: "Titre court"
english_region: "Test Region"
french_region: "Région de test"
english_csa_address: "Test Address"
french_csa_address: "Adresse de test"
english_month: "May"
french_month: "Mai"
output:
  csasdown::sr_docx:
    french: false
---
'
  writeLines(yaml_content, temp_file)

  expect_invisible(check_yaml(index_fn = temp_file, type = "sr"))
  expect_true(check_yaml(index_fn = temp_file, type = "sr"))

  unlink(temp_file)
})

# Dynamic parsing tests
test_that("get_skeleton_fields parses skeleton correctly", {
  fields <- csasdown:::get_skeleton_fields("resdoc")

  # Should include required fields
  expect_true("author" %in% fields)
  expect_true("year" %in% fields)
  expect_true("english_title" %in% fields)
  expect_true("french_title" %in% fields)
  expect_true("output" %in% fields)

  # Should NOT include optional fields
  expect_false("knit" %in% fields)
  expect_false("bibliography" %in% fields)
})

test_that("get_skeleton_fields works for all document types", {
  types <- c("resdoc", "fsar", "sr", "techreport")

  for (type in types) {
    fields <- csasdown:::get_skeleton_fields(type)
    expect_true(length(fields) > 0)
    expect_true("output" %in% fields)
    expect_false("knit" %in% fields)
    expect_false("bibliography" %in% fields)
  }
})

