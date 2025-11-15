test_that("FSAR builds", {
  skip_on_ci()
  wd <- getwd()
  testing_path <- file.path(tempdir(), "fsar")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft("fsar", create_dir = FALSE, edit = FALSE))
  suppressWarnings(render_sar())
  if (FALSE) {
    system("open _book/fsar.docx")
  }
  expect_true(file.exists("_book/fsar.docx"))
  setwd(wd)
})

test_that("resdoc builds", {
  wd <- getwd()
  testing_path <- file.path(tempdir(), "resdoc")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft("resdoc", create_dir = FALSE, edit = FALSE))
  suppressWarnings(csasdown2::render())
  if (FALSE) {
    system("open _book/resdoc.docx")
  }
  expect_true(file.exists("_book/resdoc.docx"))
  setwd(wd)
})
