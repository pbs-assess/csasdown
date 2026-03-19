test_that("auto_asp overrides inherited default fig.asp for external images", {
  old_fig_asp <- knitr::opts_chunk$get("fig.asp")
  on.exit(knitr::opts_chunk$set(fig.asp = old_fig_asp), add = TRUE)
  knitr::opts_chunk$set(fig.asp = 1 / 1.618)

  img <- tempfile(fileext = ".png")
  grDevices::png(img, width = 400, height = 200)
  graphics::plot.new()
  grDevices::dev.off()

  output <- resdoc_docx()
  options <- output$knitr$opts_hooks$auto_asp(list(
    auto_asp = TRUE,
    fig.asp = knitr::opts_chunk$get("fig.asp"),
    code = sprintf('knitr::include_graphics("%s")', img)
  ))

  expect_equal(options$fig.asp, 0.5)
})

test_that("auto_asp preserves explicit fig.asp overrides", {
  old_fig_asp <- knitr::opts_chunk$get("fig.asp")
  on.exit(knitr::opts_chunk$set(fig.asp = old_fig_asp), add = TRUE)
  knitr::opts_chunk$set(fig.asp = 1 / 1.618)

  img <- tempfile(fileext = ".png")
  grDevices::png(img, width = 400, height = 200)
  graphics::plot.new()
  grDevices::dev.off()

  output <- resdoc_docx()
  options <- output$knitr$opts_hooks$auto_asp(list(
    auto_asp = TRUE,
    fig.asp = 2,
    code = sprintf('knitr::include_graphics("%s")', img)
  ))

  expect_equal(options$fig.asp, 2)
})
