# Validate YAML fields for CSAS documents

Dynamically parses `skeleton.Rmd` to determine required fields.

## Usage

``` r
check_yaml(index_fn = "index.Rmd", type = NULL, verbose = FALSE)
```

## Arguments

- index_fn:

  Path to the index R Markdown file. Default: "index.Rmd"

- type:

  Document type ("resdoc", "fsar", "sr", "techreport"). If NULL
  (default), auto-detects from YAML output field.

- verbose:

  Print informative message on success? Default: FALSE

## Value

Invisibly returns TRUE if validation passes, aborts with informative
error message if validation fails.

## Details

Required fields are dynamically determined by parsing the skeleton.Rmd
files for each document type. This ensures the skeletons remain the
single source of truth for required YAML fields.

All fields in the skeleton are required, including both English and
French variants, as they are used in citations and references regardless
of the document language setting.

## Examples

``` r
# \donttest{
# Validate before rendering
wd <- getwd()
example_path <- file.path(tempdir(), "csasdown-example")
dir.create(example_path)
setwd(example_path)
csasdown::draft("resdoc")
#> ✔ Drafting a new resdoc project
#> ✔ Created .gitignore file
#> ✔ Created .here file
#> ✔ Created RStudio project file: csasdown-example.Rproj
check_yaml("index.Rmd", verbose = TRUE)
#> YAML validation passed for resdoc document.
setwd(wd)
unlink(example_path, recursive = TRUE, force = TRUE)
# }
```
