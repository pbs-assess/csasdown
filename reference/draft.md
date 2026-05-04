# Create a new CSAS document based on a template

Create a draft of an R Markdown CSAS document.

## Usage

``` r
draft(
  type = c("resdoc", "fsar", "sr", "techreport"),
  directory = ".",
  edit = FALSE,
  create_rstudio_project = TRUE,
  ...
)
```

## Arguments

- type:

  The type of document to draft. Must be one of `resdoc`, `fsar`, `sr`,
  or `techreport`.

- directory:

  The directory to place the draft document files. Current directory by
  default

- edit:

  `TRUE` to edit the template immediately.

- create_rstudio_project:

  `TRUE` to create an RStudio project file.

- ...:

  Other arguments to pass to
  [`rmarkdown::draft()`](https://pkgs.rstudio.com/rmarkdown/reference/draft.html).

## Value

A new project folder with template files for creating a csasdown report.

## Details

This is a light wrapper around
[`rmarkdown::draft()`](https://pkgs.rstudio.com/rmarkdown/reference/draft.html).
Consult that function for further details.

## See also

[`render()`](https://pbs-assess.github.io/csasdown/reference/render.md)

## Examples

``` r
# \donttest{
# create a temporary example folder:
wd <- getwd()
example_path <- file.path(tempdir(), "csasdown-example")
dir.create(example_path)
setwd(example_path)

# create a draft template:
csasdown::draft("resdoc")
#> ✔ Drafting a new resdoc project
#> ✔ Created .gitignore file
#> ✔ Created .here file
#> ✔ Created RStudio project file: csasdown-example.Rproj

# return to original working directory after running example:
setwd(wd)

# clean up:
unlink(example_path, recursive = TRUE, force = TRUE)
# }
```
