get_csl_arg <- function(output_format) {
  args <- output_format$pandoc$args
  csl_idx <- which(args == "--csl")
  if (length(csl_idx) == 0) {
    return(NULL)
  }

  args[csl_idx[length(csl_idx)] + 1]
}

test_that("docx formats default to English CSL", {
  expect_equal(get_csl_arg(resdoc_docx()), "csl/csas.csl")
  expect_equal(get_csl_arg(fsar_docx()), "csl/csas.csl")
  expect_equal(get_csl_arg(techreport_docx()), "csl/csas.csl")
  expect_equal(get_csl_arg(manureport_docx()), "csl/csas.csl")
  expect_equal(get_csl_arg(datareport_docx()), "csl/csas.csl")
})

test_that("docx formats use French CSL when french is TRUE", {
  expect_equal(get_csl_arg(resdoc_docx(french = TRUE)), "csl/csas-french.csl")
  expect_equal(get_csl_arg(fsar_docx(french = TRUE)), "csl/csas-french.csl")
  expect_equal(get_csl_arg(techreport_docx(french = TRUE)), "csl/csas-french.csl")
  expect_equal(get_csl_arg(manureport_docx(french = TRUE)), "csl/csas-french.csl")
  expect_equal(get_csl_arg(datareport_docx(french = TRUE)), "csl/csas-french.csl")
})

test_that("user CSL overrides package default", {
  output <- resdoc_docx(french = TRUE, pandoc_args = c("--csl", "custom.csl"))
  expect_equal(get_csl_arg(output), "custom.csl")
})
