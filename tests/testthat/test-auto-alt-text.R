test_that("auto_alt_text numbers main document figures", {
  output <- resdoc_docx()
  hook <- output$knitr$opts_hooks$auto_alt_text

  first <- hook(list(
    auto_alt_text = TRUE,
    fig.cap = "First figure."
  ))
  second <- hook(list(
    auto_alt_text = TRUE,
    fig.cap = "Second figure."
  ))

  expect_equal(first$fig.alt, "Figure 1")
  expect_equal(second$fig.alt, "Figure 2")
})

test_that("auto_alt_text preserves explicit fig.alt", {
  output <- resdoc_docx()

  options <- output$knitr$opts_hooks$auto_alt_text(list(
    auto_alt_text = TRUE,
    fig.cap = "First figure.",
    fig.alt = "Custom figure alt text"
  ))

  expect_equal(options$fig.alt, "Custom figure alt text")
})

test_that("auto_alt_text numbers appendix figures", {
  output <- resdoc_docx()
  hook <- output$knitr$opts_hooks$auto_alt_text

  first <- hook(list(
    auto_alt_text = TRUE,
    fig.cap = "First appendix figure.",
    fig.cap.pre = "Figure A.",
    fig.autonum.start_at = 1
  ))
  second <- hook(list(
    auto_alt_text = TRUE,
    fig.cap = "Second appendix figure.",
    fig.cap.pre = "Figure A.",
    fig.autonum.start_at = 1
  ))

  expect_equal(first$fig.alt, "Figure A1")
  expect_equal(second$fig.alt, "Figure A2")
})

test_that("auto_alt_text handles multiple figures from one chunk", {
  output <- resdoc_docx()
  hook <- output$knitr$opts_hooks$auto_alt_text

  main <- hook(list(
    auto_alt_text = TRUE,
    fig.cap = c("First figure.", "Second figure.")
  ))
  appendix <- hook(list(
    auto_alt_text = TRUE,
    fig.cap = c("First appendix figure.", "Second appendix figure."),
    fig.cap.pre = "Figure B.",
    fig.autonum.start_at = 1
  ))

  expect_equal(main$fig.alt, c("Figure 1", "Figure 2"))
  expect_equal(appendix$fig.alt, c("Figure B1", "Figure B2"))
})

test_that("auto_alt_text skips chunks without figure captions", {
  output <- resdoc_docx()

  options <- output$knitr$opts_hooks$auto_alt_text(list(
    auto_alt_text = TRUE
  ))

  expect_null(options$fig.alt)
})
