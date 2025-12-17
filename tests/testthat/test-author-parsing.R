test_that("Single author parsed correctly", {
  author <- "First M. Last^1^"
  result <- parse_author_field(author)

  expect_equal(result$english_author, author)
  expect_equal(result$french_author, author)
  expect_equal(result$english_author_list, "Last, F.M.")
  expect_equal(result$french_author_list, "Last, F.M.")
})

test_that("Two authors with English conjunction (multi-line with commas)", {
  author <- "First M. Last^1^,\nand Alex B. Smith^2^"
  result <- parse_author_field(author)

  expect_equal(result$english_author, author)
  expect_equal(result$french_author, "First M. Last^1^,\net Alex B. Smith^2^")
  expect_equal(result$english_author_list, "Last, F.M. and Smith, A.B.")
  expect_equal(result$french_author_list, "Last, F.M. et Smith, A.B.")
})

test_that("Two authors with English conjunction (single-line)", {
  author <- "First M. Last^1^, and Alex B. Smith^2^"
  result <- parse_author_field(author)

  expect_equal(result$english_author, author)
  expect_equal(result$french_author, "First M. Last^1^, et Alex B. Smith^2^")
  expect_equal(result$english_author_list, "Last, F.M. and Smith, A.B.")
  expect_equal(result$french_author_list, "Last, F.M. et Smith, A.B.")
})

test_that("Two authors with French conjunction (multi-line with commas)", {
  author <- "First M. Last^1^,\net Alex B. Smith^2^"
  result <- parse_author_field(author)

  expect_equal(result$french_author, author)
  expect_equal(result$english_author, "First M. Last^1^,\nand Alex B. Smith^2^")
  expect_equal(result$english_author_list, "Last, F.M. and Smith, A.B.")
  expect_equal(result$french_author_list, "Last, F.M. et Smith, A.B.")
})

test_that("Two authors with French conjunction (single-line)", {
  author <- "First M. Last^1^, et Alex B. Smith^2^"
  result <- parse_author_field(author)

  expect_equal(result$french_author, author)
  expect_equal(result$english_author, "First M. Last^1^, and Alex B. Smith^2^")
  expect_equal(result$english_author_list, "Last, F.M. and Smith, A.B.")
  expect_equal(result$french_author_list, "Last, F.M. et Smith, A.B.")
})

test_that("Multiple authors with Oxford comma (English, multi-line)", {
  author <- "First M. Last^1^,\nAlex B. Smith^2^,\nSuzanne A. Tremblay^3^, and\nJean C. Deschoses^4^"
  result <- parse_author_field(author)

  expect_equal(result$english_author, author)
  expect_equal(result$english_author_list, "Last, F.M., Smith, A.B., Tremblay, S.A., and Deschoses, J.C.")
  expect_equal(result$french_author_list, "Last, F.M., Smith, A.B., Tremblay, S.A., et Deschoses, J.C.")
})

test_that("Multiple authors with Oxford comma (English, single-line)", {
  author <- "First M. Last^1^, Alex B. Smith^2^, Suzanne A. Tremblay^3^, and Jean C. Deschoses^4^"
  result <- parse_author_field(author)

  expect_equal(result$english_author, author)
  expect_equal(result$english_author_list, "Last, F.M., Smith, A.B., Tremblay, S.A., and Deschoses, J.C.")
  expect_equal(result$french_author_list, "Last, F.M., Smith, A.B., Tremblay, S.A., et Deschoses, J.C.")
})

test_that("Three authors with Oxford comma (multi-line)", {
  author <- "First M. Last^1^,\nAlex B. Smith^2^, and\nJean C. Doe^3^"
  result <- parse_author_field(author)

  expect_equal(result$english_author_list, "Last, F.M., Smith, A.B., and Doe, J.C.")
  expect_equal(result$french_author_list, "Last, F.M., Smith, A.B., et Doe, J.C.")
})

test_that("Three authors with Oxford comma (single-line)", {
  author <- "First M. Last^1^, Alex B. Smith^2^, and Jean C. Doe^3^"
  result <- parse_author_field(author)

  expect_equal(result$english_author_list, "Last, F.M., Smith, A.B., and Doe, J.C.")
  expect_equal(result$french_author_list, "Last, F.M., Smith, A.B., et Doe, J.C.")
})

test_that("Multiple affiliations preserved in display format", {
  author <- "First M. Last^1,2,3^, and Alex B. Smith^2^"
  result <- parse_author_field(author)

  expect_true(grepl("\\^1,2,3\\^", result$english_author))
  expect_false(grepl("\\^", result$english_author_list))
  expect_equal(result$english_author_list, "Last, F.M. and Smith, A.B.")
})

test_that("Name containing 'and' not confused with conjunction", {
  author <- "Anderson M. Smith^1^, and Beth C. Jones^2^"
  result <- parse_author_field(author)

  expect_equal(result$english_author_list, "Smith, A.M. and Jones, B.C.")
})

test_that("Hyphenated last name", {
  author <- "First M. Smith-Jones^1^"
  result <- parse_author_field(author)

  expect_equal(result$english_author_list, "Smith-Jones, F.M.")
})

test_that("Author without middle initial", {
  author <- "First Last^1^"
  result <- parse_author_field(author)

  expect_equal(result$english_author_list, "Last, F.")
})

test_that("detect_conjunction finds 'and' (multi-line)", {
  author <- "First M. Last^1^,\nand Alex B. Smith^2^"
  expect_equal(detect_conjunction(author), "and")
})

test_that("detect_conjunction finds 'and' (single-line)", {
  author <- "First M. Last^1^, and Alex B. Smith^2^"
  expect_equal(detect_conjunction(author), "and")
})

test_that("detect_conjunction finds 'et' (multi-line)", {
  author <- "First M. Last^1^,\net Alex B. Smith^2^"
  expect_equal(detect_conjunction(author), "et")
})

test_that("detect_conjunction finds 'et' (single-line)", {
  author <- "First M. Last^1^, et Alex B. Smith^2^"
  expect_equal(detect_conjunction(author), "et")
})

test_that("detect_conjunction returns NULL when no conjunction", {
  author <- "First M. Last^1^\nAlex B. Smith^2^"
  expect_null(detect_conjunction(author))
})

test_that("swap_conjunction replaces 'and' with 'et' (multi-line)", {
  author <- "First M. Last^1^,\nand Alex B. Smith^2^"
  result <- swap_conjunction(author, "and", "et")
  expect_equal(result, "First M. Last^1^,\net Alex B. Smith^2^")
})

test_that("swap_conjunction replaces 'and' with 'et' (single-line)", {
  author <- "First M. Last^1^, and Alex B. Smith^2^"
  result <- swap_conjunction(author, "and", "et")
  expect_equal(result, "First M. Last^1^, et Alex B. Smith^2^")
})

test_that("swap_conjunction replaces 'et' with 'and' (multi-line)", {
  author <- "First M. Last^1^,\net Alex B. Smith^2^"
  result <- swap_conjunction(author, "et", "and")
  expect_equal(result, "First M. Last^1^,\nand Alex B. Smith^2^")
})

test_that("swap_conjunction replaces 'et' with 'and' (single-line)", {
  author <- "First M. Last^1^, et Alex B. Smith^2^"
  result <- swap_conjunction(author, "et", "and")
  expect_equal(result, "First M. Last^1^, and Alex B. Smith^2^")
})

test_that("parse_single_author_name with middle initial", {
  result <- parse_single_author_name("First M. Last^1^")
  expect_equal(result$first, "First")
  expect_equal(result$middle, "M.")
  expect_equal(result$last, "Last")
})

test_that("parse_single_author_name without middle initial", {
  result <- parse_single_author_name("First Last^1^")
  expect_equal(result$first, "First")
  expect_true(is.na(result$middle))
  expect_equal(result$last, "Last")
})

test_that("parse_single_author_name strips conjunction", {
  result <- parse_single_author_name("and First M. Last^1^")
  expect_equal(result$first, "First")
  expect_equal(result$middle, "M.")
  expect_equal(result$last, "Last")
})

test_that("parse_single_author_name strips superscripts", {
  result <- parse_single_author_name("First M. Last^1,2,3^")
  expect_equal(result$last, "Last")
})

test_that("convert_to_citation_format with middle initial", {
  parsed <- list(first = "First", middle = "M.", last = "Last")
  expect_equal(convert_to_citation_format(parsed), "Last, F.M.")
})

test_that("convert_to_citation_format without middle initial", {
  parsed <- list(first = "First", middle = NA, last = "Last")
  expect_equal(convert_to_citation_format(parsed), "Last, F.")
})

test_that("generate_author_citation_list with 1 author", {
  authors <- list("First M. Last^1^")
  result <- generate_author_citation_list(authors, "and")
  expect_equal(result, "Last, F.M.")
})

test_that("generate_author_citation_list with 2 authors (no Oxford comma)", {
  authors <- list("First M. Last^1^", "Alex B. Smith^2^")
  result <- generate_author_citation_list(authors, "and")
  expect_equal(result, "Last, F.M. and Smith, A.B.")
})

test_that("generate_author_citation_list with 3+ authors (Oxford comma)", {
  authors <- list("First M. Last^1^", "Alex B. Smith^2^", "Jean C. Doe^3^")
  result <- generate_author_citation_list(authors, "and")
  expect_equal(result, "Last, F.M., Smith, A.B., and Doe, J.C.")
})
