test_that("replace_bookmark_with_markdown replaces text", {
  skip_on_cran()

  template_path <- system.file(
    "tech-report-docx",
    "01-tech-report-cover-english.docx",
    package = "csasdown"
  )
  if (!file.exists(template_path)) skip("Template not found")

  doc <- officer::read_docx(template_path)
  result <- replace_bookmark_with_markdown(doc, "title", "Simple text")

  doc_summary <- officer::docx_summary(result)

  expect_true(any(grepl("Simple text", doc_summary$text)))
  expect_false(any(grepl("^Title$", doc_summary$text)))
})


test_that("replace_bookmark_with_markdown preserves markdown formatting", {
  skip_on_cran()

  template_path <- system.file(
    "tech-report-docx",
    "01-tech-report-cover-english.docx",
    package = "csasdown"
  )
  if (!file.exists(template_path)) skip("Template not found")

  doc <- officer::read_docx(template_path)

  result <- replace_bookmark_with_markdown(
    doc,
    "title",
    "Text *italic*^1^\\\nLine 2"
  )

  temp_doc <- tempfile(fileext = ".docx")
  print(result, target = temp_doc)

  temp_dir <- tempfile()
  dir.create(temp_dir)
  utils::unzip(temp_doc, exdir = temp_dir)

  xml <- paste(
    readLines(file.path(temp_dir, "word", "document.xml"), warn = FALSE),
    collapse = ""
  )

  expect_true(grepl("<w:i/>", xml))  # italic
  expect_true(grepl('vertAlign.*superscript', xml))  # superscript
  expect_true(grepl("<w:br", xml))  # line break

  unlink(c(temp_doc, temp_dir), recursive = TRUE)
})


test_that("replace_bookmark_with_markdown preserves template formatting", {
  skip_on_cran()

  template_path <- system.file(
    "tech-report-docx",
    "01-tech-report-cover-english.docx",
    package = "csasdown"
  )
  if (!file.exists(template_path)) skip("Template not found")

  doc <- officer::read_docx(template_path)
  result <- replace_bookmark_with_markdown(doc, "title", "Test")

  temp_doc <- tempfile(fileext = ".docx")
  print(result, target = temp_doc)

  temp_dir <- tempfile()
  dir.create(temp_dir)
  utils::unzip(temp_doc, exdir = temp_dir)

  xml <- paste(
    readLines(file.path(temp_dir, "word", "document.xml"), warn = FALSE),
    collapse = ""
  )

  # check that template styling still present
  expect_true(grepl("<w:b/>", xml))
  expect_true(grepl("Microsoft Sans Serif", xml))

  unlink(c(temp_doc, temp_dir), recursive = TRUE)
})


test_that("replace_bookmarks_with_markdown replaces multiple bookmarks", {
  skip_on_cran()

  template_path <- system.file(
    "tech-report-docx",
    "01-tech-report-cover-english.docx",
    package = "csasdown"
  )
  if (!file.exists(template_path)) skip("Template not found")

  doc <- officer::read_docx(template_path)

  result <- replace_bookmarks_with_markdown(
    doc,
    title = "New Title",
    authors = "Author One",
    address = "Line 1\\\nLine 2"
  )

  doc_summary <- officer::docx_summary(result)

  expect_true(any(grepl("New Title", doc_summary$text)))
  expect_true(any(grepl("Author One", doc_summary$text)))
})


test_that("replace_bookmarks_with_markdown validates input", {
  skip_on_cran()

  doc <- officer::read_docx()

  expect_error(
    replace_bookmarks_with_markdown(doc, title = "A", title = "B"),
    "Duplicate"
  )

  args <- list(doc = doc, "A")
  names(args) <- c("doc", "")

  expect_error(
    do.call(replace_bookmarks_with_markdown, args),
    "named"
  )
})


test_that("replace_bookmarks_with_markdown warns on missing bookmark", {
  skip_on_cran()

  doc <- officer::read_docx()

  expect_warning(
    replace_bookmarks_with_markdown(doc, not_real = "Test"),
    "not found"
  )
})
