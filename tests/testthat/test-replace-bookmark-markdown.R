test_that("replace_bookmark_with_markdown replaces plain text", {
  skip_on_cran()

  # Use the actual tech report template which has proper bookmark structure
  template_path <- system.file("tech-report-docx", "01-tech-report-cover-english.docx",
                               package = "csasdown2")

  if (!file.exists(template_path)) {
    skip("Tech report template not found")
  }

  doc <- officer::read_docx(template_path)

  # Replace the bookmark with plain text
  result <- replace_bookmark_with_markdown(doc, "title", "Simple text")

  # Check that the document contains the new text
  doc_summary <- officer::docx_summary(result)
  expect_true(any(grepl("Simple text", doc_summary$text)))

  # Check that original "Title" placeholder is gone
  expect_false(any(grepl("^Title$", doc_summary$text)))
})

test_that("replace_bookmark_with_markdown handles italic markdown", {
  skip_on_cran()

  # Use the actual tech report template which has proper bookmark structure
  template_path <- system.file("tech-report-docx", "01-tech-report-cover-english.docx",
                               package = "csasdown2")

  if (!file.exists(template_path)) {
    skip("Tech report template not found")
  }

  doc <- officer::read_docx(template_path)

  # Replace title bookmark with text containing italics
  result <- replace_bookmark_with_markdown(doc, "title", "Population of *Sebastes alutus*")

  # Save to temp file and extract XML to verify italic tags
  temp_doc <- tempfile(fileext = ".docx")
  print(result, target = temp_doc)

  # Extract and check XML
  temp_dir <- tempfile()
  dir.create(temp_dir)
  utils::unzip(temp_doc, exdir = temp_dir)

  doc_xml <- readLines(file.path(temp_dir, "word", "document.xml"), warn = FALSE)
  doc_xml_full <- paste(doc_xml, collapse = "")

  # Check that the text is present
  expect_true(grepl("Population of", doc_xml_full, fixed = TRUE))
  expect_true(grepl("Sebastes alutus", doc_xml_full, fixed = TRUE))

  # Check that italic tags are present for the species name
  # Find the "Sebastes alutus" text and check if it's in an italic run
  sebastes_section <- regmatches(doc_xml_full,
                                 regexpr("<w:r>.*?Sebastes alutus.*?</w:r>", doc_xml_full, perl = TRUE))
  expect_true(grepl("<w:i/>", sebastes_section))

  # Check that "Population of" is NOT italic
  pop_section <- regmatches(doc_xml_full,
                            regexpr("<w:r>.*?Population of.*?</w:r>", doc_xml_full, perl = TRUE))
  expect_false(grepl("<w:i/>", pop_section))

  # Cleanup
  unlink(c(temp_doc, temp_dir), recursive = TRUE)
})

test_that("replace_bookmark_with_markdown preserves original formatting", {
  skip_on_cran()

  template_path <- system.file("tech-report-docx", "01-tech-report-cover-english.docx",
                               package = "csasdown2")

  if (!file.exists(template_path)) {
    skip("Tech report template not found")
  }

  doc <- officer::read_docx(template_path)

  # Replace title with text
  result <- replace_bookmark_with_markdown(doc, "title", "Test *Title*")

  # Save and extract XML
  temp_doc <- tempfile(fileext = ".docx")
  print(result, target = temp_doc)

  temp_dir <- tempfile()
  dir.create(temp_dir)
  utils::unzip(temp_doc, exdir = temp_dir)

  doc_xml <- readLines(file.path(temp_dir, "word", "document.xml"), warn = FALSE)
  doc_xml_full <- paste(doc_xml, collapse = "")

  # Extract the run containing "Test" to check formatting
  test_section <- regmatches(doc_xml_full,
                             regexpr('<w:r>.*?<w:t[^>]*>Test</w:t>.*?</w:r>', doc_xml_full, perl = TRUE))

  # Check that original formatting is preserved (bold, size 36, Microsoft Sans Serif)
  expect_true(grepl("<w:b/>", test_section))
  expect_true(grepl('<w:sz w:val="36"/>', test_section, fixed = TRUE))
  expect_true(grepl("Microsoft Sans Serif", test_section, fixed = TRUE))

  # Check that Title has italic added while preserving other formatting
  title_section <- regmatches(doc_xml_full,
                              regexpr('<w:r>.*?<w:t[^>]*>Title</w:t>.*?</w:r>', doc_xml_full, perl = TRUE))
  expect_true(grepl("<w:i/>", title_section))
  expect_true(grepl("<w:b/>", title_section))
  expect_true(grepl('<w:sz w:val="36"/>', title_section, fixed = TRUE))

  # Cleanup
  unlink(c(temp_doc, temp_dir), recursive = TRUE)
})

test_that("replace_bookmark_with_markdown handles multiple italic sections", {
  skip_on_cran()

  template_path <- system.file("tech-report-docx", "01-tech-report-cover-english.docx",
                               package = "csasdown2")

  if (!file.exists(template_path)) {
    skip("Tech report template not found")
  }

  doc <- officer::read_docx(template_path)

  # Text with multiple italic sections
  result <- replace_bookmark_with_markdown(doc, "title",
                                          "Analysis of *Sebastes alutus* and *Sebastes ruberrimus*")

  temp_doc <- tempfile(fileext = ".docx")
  print(result, target = temp_doc)

  temp_dir <- tempfile()
  dir.create(temp_dir)
  utils::unzip(temp_doc, exdir = temp_dir)

  doc_xml <- readLines(file.path(temp_dir, "word", "document.xml"), warn = FALSE)
  doc_xml_full <- paste(doc_xml, collapse = "")

  # Check both species names are present
  expect_true(grepl("Sebastes alutus", doc_xml_full, fixed = TRUE))
  expect_true(grepl("Sebastes ruberrimus", doc_xml_full, fixed = TRUE))

  # Check both are italicized
  alutus_section <- regmatches(doc_xml_full,
                               regexpr("<w:r>.*?Sebastes alutus.*?</w:r>", doc_xml_full, perl = TRUE))
  expect_true(grepl("<w:i/>", alutus_section))

  ruberrimus_section <- regmatches(doc_xml_full,
                                   regexpr("<w:r>.*?Sebastes ruberrimus.*?</w:r>", doc_xml_full, perl = TRUE))
  expect_true(grepl("<w:i/>", ruberrimus_section))

  # Check that "Analysis of" and "and" are not italic
  analysis_section <- regmatches(doc_xml_full,
                                 regexpr("<w:r>.*?Analysis of.*?</w:r>", doc_xml_full, perl = TRUE))
  expect_false(grepl("<w:i/>", analysis_section))

  # Cleanup
  unlink(c(temp_doc, temp_dir), recursive = TRUE)
})

test_that("replace_bookmark_with_markdown handles text without italics", {
  skip_on_cran()

  template_path <- system.file("tech-report-docx", "01-tech-report-cover-english.docx",
                               package = "csasdown2")

  if (!file.exists(template_path)) {
    skip("Tech report template not found")
  }

  doc <- officer::read_docx(template_path)

  # Plain text without any markdown
  result <- replace_bookmark_with_markdown(doc, "title", "A Simple Title")

  temp_doc <- tempfile(fileext = ".docx")
  print(result, target = temp_doc)

  doc_summary <- officer::docx_summary(result)

  # Check text is present
  expect_true(any(grepl("A Simple Title", doc_summary$text)))

  # Check original "Title" placeholder is gone
  expect_false(any(grepl("^Title$", doc_summary$text)))

  unlink(temp_doc)
})

test_that("replace_bookmark_with_markdown returns officer rdocx object", {
  skip_on_cran()

  template_path <- system.file("tech-report-docx", "01-tech-report-cover-english.docx",
                               package = "csasdown2")

  if (!file.exists(template_path)) {
    skip("Tech report template not found")
  }

  doc <- officer::read_docx(template_path)
  result <- replace_bookmark_with_markdown(doc, "title", "Test")

  expect_s3_class(result, "rdocx")
})

test_that("replace_bookmark_with_markdown handles superscript markdown", {
  skip_on_cran()

  template_path <- system.file("tech-report-docx", "01-tech-report-cover-english.docx",
                               package = "csasdown2")

  if (!file.exists(template_path)) {
    skip("Tech report template not found")
  }

  doc <- officer::read_docx(template_path)

  # Replace with text containing superscript
  result <- replace_bookmark_with_markdown(doc, "title", "First. M. Last^1^ and Alex B. Smith^2^")

  # Save to temp file and extract XML to verify superscript tags
  temp_doc <- tempfile(fileext = ".docx")
  print(result, target = temp_doc)

  temp_dir <- tempfile()
  dir.create(temp_dir)
  utils::unzip(temp_doc, exdir = temp_dir)

  doc_xml <- readLines(file.path(temp_dir, "word", "document.xml"), warn = FALSE)
  doc_xml_full <- paste(doc_xml, collapse = "")

  # Check that the text is present
  expect_true(grepl("First. M. Last", doc_xml_full, fixed = TRUE))
  expect_true(grepl("Alex B. Smith", doc_xml_full, fixed = TRUE))

  # Check that superscript tags are present
  expect_true(grepl('w:vertAlign.*w:val="superscript"', doc_xml_full))

  # Check that the superscript numbers are in superscript runs
  superscript_1 <- grepl('vertAlign.*superscript.*>1<', doc_xml_full)
  superscript_2 <- grepl('vertAlign.*superscript.*>2<', doc_xml_full)
  expect_true(superscript_1 || grepl('>1<.*vertAlign.*superscript', doc_xml_full))
  expect_true(superscript_2 || grepl('>2<.*vertAlign.*superscript', doc_xml_full))

  # Cleanup
  unlink(c(temp_doc, temp_dir), recursive = TRUE)
})

test_that("replace_bookmark_with_markdown handles both italic and superscript", {
  skip_on_cran()

  template_path <- system.file("tech-report-docx", "01-tech-report-cover-english.docx",
                               package = "csasdown2")

  if (!file.exists(template_path)) {
    skip("Tech report template not found")
  }

  doc <- officer::read_docx(template_path)

  # Replace with text containing both italic and superscript
  result <- replace_bookmark_with_markdown(doc, "title", "Population of *Sebastes alutus*^1^ in BC")

  temp_doc <- tempfile(fileext = ".docx")
  print(result, target = temp_doc)

  temp_dir <- tempfile()
  dir.create(temp_dir)
  utils::unzip(temp_doc, exdir = temp_dir)

  doc_xml <- readLines(file.path(temp_dir, "word", "document.xml"), warn = FALSE)
  doc_xml_full <- paste(doc_xml, collapse = "")

  # Check both formatting types are present
  expect_true(grepl("<w:i/>", doc_xml_full))
  expect_true(grepl('w:vertAlign.*w:val="superscript"', doc_xml_full))

  # Check the species name is present and italicized
  expect_true(grepl("Sebastes alutus", doc_xml_full, fixed = TRUE))

  # Cleanup
  unlink(c(temp_doc, temp_dir), recursive = TRUE)
})

test_that("replace_bookmark_with_markdown handles empty italic markers", {
  skip_on_cran()

  template_path <- system.file("tech-report-docx", "01-tech-report-cover-english.docx",
                               package = "csasdown2")

  if (!file.exists(template_path)) {
    skip("Tech report template not found")
  }

  doc <- officer::read_docx(template_path)

  # Text with adjacent asterisks (should not create empty italic runs)
  result <- replace_bookmark_with_markdown(doc, "title", "Test**Title")

  temp_doc <- tempfile(fileext = ".docx")
  print(result, target = temp_doc)

  doc_summary <- officer::docx_summary(result)

  # Should contain the text (asterisks removed)
  expect_true(any(grepl("Test", doc_summary$text)))
  expect_true(any(grepl("Title", doc_summary$text)))

  unlink(temp_doc)
})

test_that("replace_bookmark_with_markdown handles line breaks", {
  skip_on_cran()

  template_path <- system.file("tech-report-docx", "01-tech-report-cover-english.docx",
                               package = "csasdown2")

  if (!file.exists(template_path)) {
    skip("Tech report template not found")
  }

  doc <- officer::read_docx(template_path)

  # Text with line breaks (backslash followed by newline, as from YAML with |)
  text_with_breaks <- "Line 1\\\nLine 2\\\nLine 3"

  result <- replace_bookmark_with_markdown(doc, "title", text_with_breaks)

  temp_doc <- tempfile(fileext = ".docx")
  print(result, target = temp_doc)

  # Extract XML to verify line break tags
  temp_dir <- tempfile()
  dir.create(temp_dir)
  utils::unzip(temp_doc, exdir = temp_dir)

  doc_xml <- readLines(file.path(temp_dir, "word", "document.xml"), warn = FALSE)
  doc_xml_full <- paste(doc_xml, collapse = "")

  # Should have line break tags
  expect_true(grepl("<w:br", doc_xml_full))

  # Should have the text content
  expect_true(grepl("Line 1", doc_xml_full))
  expect_true(grepl("Line 2", doc_xml_full))
  expect_true(grepl("Line 3", doc_xml_full))

  # Cleanup
  unlink(c(temp_doc, temp_dir), recursive = TRUE)
})

test_that("replace_bookmark_with_markdown handles complex formatting", {
  skip_on_cran()

  template_path <- system.file("tech-report-docx", "01-tech-report-cover-english.docx",
                               package = "csasdown2")

  if (!file.exists(template_path)) {
    skip("Tech report template not found")
  }

  doc <- officer::read_docx(template_path)

  # Complex text with superscript, italics, and line breaks
  # (simulating YAML with |)
  complex_text <- "^1^*Sebastes alutus*\\\nPacific Biological Station\\\n^2^Another place"

  result <- replace_bookmark_with_markdown(doc, "title", complex_text)

  temp_doc <- tempfile(fileext = ".docx")
  print(result, target = temp_doc)

  temp_dir <- tempfile()
  dir.create(temp_dir)
  utils::unzip(temp_doc, exdir = temp_dir)

  doc_xml <- readLines(file.path(temp_dir, "word", "document.xml"), warn = FALSE)
  doc_xml_full <- paste(doc_xml, collapse = "")

  # Should have all three formatting types
  expect_true(grepl("<w:i/>", doc_xml_full))
  expect_true(grepl('w:vertAlign.*w:val="superscript"', doc_xml_full))
  expect_true(grepl("<w:br", doc_xml_full))

  # Should have the text content
  expect_true(grepl("Sebastes alutus", doc_xml_full))
  expect_true(grepl("Pacific Biological Station", doc_xml_full))
  expect_true(grepl("Another place", doc_xml_full))

  # Cleanup
  unlink(c(temp_doc, temp_dir), recursive = TRUE)
})
