test_that("csasdown:::is_rmd_list_line() works", {

  k <- csasdown:::is_rmd_list_line(NULL)
  expect_identical(k, NULL)
  # ---------------------------------------------------------------------------

  j <- c("1. Item 1", "2. Item 2", "Hello", NA)
  expect_error(csasdown:::is_rmd_list_line(j))
  # ---------------------------------------------------------------------------

  j <- "- X"
  k <- csasdown:::is_rmd_list_line(j)
  expect_identical(k , TRUE)
  # ---------------------------------------------------------------------------

  j <- "* X"
  k <- csasdown:::is_rmd_list_line(j)
  expect_identical(k , TRUE)
  # ---------------------------------------------------------------------------

  j <- "+ X"
  k <- csasdown:::is_rmd_list_line(j)
  expect_identical(k , TRUE)
  # ---------------------------------------------------------------------------

  j <- c("1. ne", "2. lkmec", "kjbc", "        a. rewkjn","                 i. 121")
  k <- csasdown:::is_rmd_list_line(j)
  expect_identical(k , c(TRUE, TRUE, FALSE, TRUE, TRUE))
  # ---------------------------------------------------------------------------

  j <- c("- ne", "+ lkmec", "1. kjbc", "        a. rewkjn","                 i. 121")
  k <- csasdown:::is_rmd_list_line(j)
  expect_identical(k , c(TRUE, TRUE, TRUE, TRUE, TRUE))
  # ---------------------------------------------------------------------------
})
