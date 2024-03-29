test_that("csasdown:::is_rmd_table_line() works", {

  k <- csasdown:::is_rmd_table_line(NULL)
  expect_identical(k, NULL)
  # ---------------------------------------------------------------------------

  expect_identical(csasdown:::is_rmd_table_line(""), "false")
  # ---------------------------------------------------------------------------

  expect_identical(csasdown:::is_rmd_table_line(c("", "")), "false")
  # ---------------------------------------------------------------------------

  j <- c("", "", NA)
  k <- csasdown:::is_rmd_table_line(j)
  expect_identical(k, "false")
  # ---------------------------------------------------------------------------

  j <- c("", "", "")
  k <- csasdown:::is_rmd_table_line(j)
  expect_identical(k, "false")
  # ---------------------------------------------------------------------------

  j <- c("-----", "ABCDE", "-----")
  k <- csasdown:::is_rmd_table_line(j)
  expect_identical(k, "false")
  # ---------------------------------------------------------------------------

  j <- c("ABCDE", "-----", "EFGHI")
  k <- csasdown:::is_rmd_table_line(j)
  expect_identical(k, "type2")
  # ---------------------------------------------------------------------------

  j <- list(c("-----", "ABCDE", "-----"),
            c("ABCDE", "-----", "EFGHI"))
  k <- csasdown:::is_rmd_table_line(j)
  expect_identical(k, c("false", "type2"))
  # ---------------------------------------------------------------------------

  j <- list(c("-----", "ABCDE", "-----"), NA)
  expect_error(csasdown:::is_rmd_table_line(j))
  # ---------------------------------------------------------------------------

  j <- list(NA, NA)
  expect_error(csasdown:::is_rmd_table_line(j))
  # ---------------------------------------------------------------------------

  j <- list(c("-----", "ABCDE", "-----"), NULL)
  expect_identical(csasdown:::is_rmd_table_line(j), c("false", "false"))
  # ---------------------------------------------------------------------------

  j <- list(NULL, NULL)
  expect_identical(csasdown:::is_rmd_table_line(j), c("false", "false"))

  # ---------------------------------------------------------------------------
  j <- c("-----", "ABCDE", "EFGHI")
  k <- csasdown:::is_rmd_table_line(j)
  expect_identical(k, "false")
  # ---------------------------------------------------------------------------
})
