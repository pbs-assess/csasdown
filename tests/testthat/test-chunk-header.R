test_that("chunk_header_contains() works", {
  x <- paste0("```{r chap03-para-1-fr, eval = fr(), hello_world, ",
              "results = 'asis', test = \"yes\", needs_trans = FALSE}")

  expect_null(csasdown:::chunk_header_contains(NULL, "hello"))
  expect_identical(csasdown:::chunk_header_contains(NA, "hello"), NA_character_)
  expect_identical(csasdown:::chunk_header_contains(x, NULL), FALSE)
  expect_identical(csasdown:::chunk_header_contains(x, NA), FALSE)

  expect_false(csasdown:::chunk_header_contains(x, ""))
  expect_false(csasdown:::chunk_header_contains(x, "     "))
  expect_false(csasdown:::chunk_header_contains(x, "chap03-para-1-fr"))
  expect_false(csasdown:::chunk_header_contains(x, "Eval"))
  expect_false(csasdown:::chunk_header_contains(x, "eval = "))

  expect_true(csasdown:::chunk_header_contains(x, "hello_world"))

  expect_true(csasdown:::chunk_header_contains(x, "eval"))
  expect_false(csasdown:::chunk_header_contains(x, "eval = fr"))
  expect_true(csasdown:::chunk_header_contains(x, "eval = fr()"))
  expect_true(csasdown:::chunk_header_contains(x, "eval=fr()"))
  expect_true(csasdown:::chunk_header_contains(x, "eval=     fr()"))
  expect_true(csasdown:::chunk_header_contains(x, "eval    =fr()"))

  expect_true(csasdown:::chunk_header_contains(x, "      needs_trans "))
  expect_false(csasdown:::chunk_header_contains(x, "needs_trans = TRUE"))
  expect_true(csasdown:::chunk_header_contains(x, "needs_trans = FALSE"))

  expect_true(csasdown:::chunk_header_contains(x, "results"))
  expect_true(csasdown:::chunk_header_contains(x, "results = 'asis'"))
  expect_true(csasdown:::chunk_header_contains(x, "results = \"asis\""))

  expect_true(csasdown:::chunk_header_contains(x, "test"))
  expect_true(csasdown:::chunk_header_contains(x, 'test'))
  expect_true(csasdown:::chunk_header_contains(x, 'test = "yes"'))
  expect_true(csasdown:::chunk_header_contains(x, "test = \"yes\""))
  expect_true(csasdown:::chunk_header_contains(x, "test = 'yes'"))


})

test_that("chunk_header_remove() works", {
  x <- paste0("```{r chap03-para-1-fr, eval = fr(), hello_world, ",
              "results = 'asis', needs_trans = FALSE}")

  expect_null(csasdown:::chunk_header_remove(NULL, "hello"))
  expect_identical(csasdown:::chunk_header_remove(NA, "hello"), NA_character_)
  expect_identical(csasdown:::chunk_header_remove(x, NULL), x)
  expect_identical(csasdown:::chunk_header_remove(x, NA), x)

  # ---------------------------------------------------------------------------
  expect_warning(actual <- csasdown:::chunk_header_remove(
    "``{r not, a, chunk, header}", "hello"),
    "is not a valid knitr chunk header")
  expect_identical(actual, "``{r not, a, chunk, header}")

  # ---------------------------------------------------------------------------
  i <- csasdown:::chunk_header_remove(x, "hello_world")
  j <- csasdown:::chunk_header_remove(i, "needs_trans")
  k <- csasdown:::chunk_header_remove(j, "results")
  l <- csasdown:::chunk_header_remove(k, "eval")
  expect_warning(actual <- csasdown:::chunk_header_remove(l,
                                                         "chap03-para-1-fr"),
                 "was not found in the knitr chunk")

  expect_identical(actual, l)

})

test_that("chunk_header_add() works", {
  x <- paste0("```{r chap03-para-1-en}")

  expect_null(csasdown:::chunk_header_add(NULL, "hello"))
  expect_identical(csasdown:::chunk_header_add(NA, "hello"), NA_character_)
  expect_identical(csasdown:::chunk_header_add(x, NULL), x)
  expect_identical(csasdown:::chunk_header_add(x, NA), x)

  # ---------------------------------------------------------------------------
  expect_warning(actual <- csasdown:::chunk_header_add(
    "``{r not, a, chunk, header}", "hello"),
    "is not a valid knitr chunk header")
  expect_identical(actual, "``{r not, a, chunk, header}")

  # ---------------------------------------------------------------------------
  i <- csasdown:::chunk_header_add(x, "hello_world")
  j <- csasdown:::chunk_header_add(i, "needs_trans = FALSE")
  k <- csasdown:::chunk_header_add(j, "results = 'asis'")
  l <- csasdown:::chunk_header_add(k, "eval = !fr()")
  expect_warning(
    actual <- csasdown:::chunk_header_add(l, "eval = fr()"),
                 "already defined in the knitr chunk header")
  expect_identical(actual, l)

  actual <- csasdown:::chunk_header_add(l, "eval = fr()", ovr = TRUE)
  expect_identical(actual, paste0("```{r chap03-para-1-en, hello_world, ",
                                  "needs_trans = FALSE, results = 'asis', ",
                                  "eval = fr()}"))

  actual <- csasdown:::chunk_header_add(l, "hello_world = FALSE", ovr = TRUE)
  expect_identical(actual, paste0("```{r chap03-para-1-en, needs_trans = FALSE, ",
                                  "results = 'asis', eval = !fr(), ",
                                  "hello_world = FALSE}"))

  actual <- csasdown:::chunk_header_add(l, 'results = "asis"', ovr = TRUE)
  expect_identical(actual, paste0("```{r chap03-para-1-en, hello_world, ",
                                  "needs_trans = FALSE, eval = !fr(), ",
                                  "results = \"asis\"}"))
})
