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

test_that("auto_alt_text preserves matching explicit fig.alt", {
  output <- resdoc_docx()
  hook <- output$knitr$opts_hooks$auto_alt_text

  options <- hook(list(
    auto_alt_text = TRUE,
    fig.cap = "First figure.",
    fig.alt = "Figure 1"
  ))
  next_options <- hook(list(
    auto_alt_text = TRUE,
    fig.cap = "Second figure."
  ))

  expect_equal(options$fig.alt, "Figure 1")
  expect_equal(next_options$fig.alt, "Figure 2")
})

test_that("auto_alt_text warns once and replaces custom fig.alt", {
  output <- resdoc_docx()
  hook <- output$knitr$opts_hooks$auto_alt_text

  options <- expect_warning(
    hook(list(
      auto_alt_text = TRUE,
      fig.cap = "First figure.",
      fig.alt = "Custom figure alt text"
    )),
    "csasdown automatically sets figure alt text"
  )
  next_custom <- expect_warning(
    hook(list(
      auto_alt_text = TRUE,
      fig.cap = "Second figure.",
      fig.alt = "Another custom alt text"
    )),
    NA
  )
  next_options <- hook(list(
    auto_alt_text = TRUE,
    fig.cap = "Third figure."
  ))

  expect_equal(options$fig.alt, "Figure 1")
  expect_equal(next_custom$fig.alt, "Figure 2")
  expect_equal(next_options$fig.alt, "Figure 3")
})

test_that("auto_alt_text replaces custom appendix fig.alt", {
  output <- resdoc_docx()
  hook <- output$knitr$opts_hooks$auto_alt_text

  options <- expect_warning(
    hook(list(
      auto_alt_text = TRUE,
      fig.cap = "First appendix figure.",
      fig.cap.pre = "Figure A.",
      fig.autonum.start_at = 1,
      fig.alt = "Custom appendix alt text"
    )),
    "csasdown automatically sets figure alt text"
  )
  next_options <- hook(list(
    auto_alt_text = TRUE,
    fig.cap = "Second appendix figure.",
    fig.cap.pre = "Figure A.",
    fig.autonum.start_at = 1
  ))

  expect_equal(options$fig.alt, "Figure A1")
  expect_equal(next_options$fig.alt, "Figure A2")
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
