test_that("resdoc frontmatter replaces header region and year bookmarks", {
  front_file <- system.file("csas-docx", "resdoc-frontmatter-english2.docx", package = "csasdown")

  frontmatter <- officer::read_docx(front_file) |>
    officer::headers_replace_text_at_bkm("region", "Test Region") |>
    officer::headers_replace_text_at_bkm("year", "2030")

  out <- tempfile(fileext = ".docx")
  print(frontmatter, target = out)

  xml_dir <- tempfile()
  dir.create(xml_dir)
  utils::unzip(out, exdir = xml_dir)
  header_xml <- paste(readLines(file.path(xml_dir, "word", "header1.xml"), warn = FALSE), collapse = "")

  expect_match(header_xml, "Test Region", fixed = TRUE)
  expect_match(header_xml, "2030", fixed = TRUE)
  expect_false(grepl("Name of the region", header_xml, fixed = TRUE))

  unlink(c(out, xml_dir), recursive = TRUE, force = TRUE)
})
