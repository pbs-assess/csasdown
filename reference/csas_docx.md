# Create `.docx` CSAS-formatted documents

This is a function called within the YAML of the `index.Rmd` file to
specify the creation of a `.docx` version of a CSAS report.

## Usage

``` r
fsar_docx(...)

resdoc_docx(...)

sr_docx(...)

techreport_docx(...)
```

## Arguments

- ...:

  Other arguments to
  [`officedown::rdocx_document()`](https://davidgohel.github.io/officedown/reference/rdocx_document.html)

## Value

A `.docx` file

## See also

[`render()`](https://pbs-assess.github.io/csasdown/reference/render.md),
[`draft()`](https://pbs-assess.github.io/csasdown/reference/draft.md)

## Examples

``` r
# These functions would normally only be specified within the YAML section of
# index.Rmd

x <- resdoc_docx()
names(x)
#>  [1] "knitr"                   "pandoc"                 
#>  [3] "keep_md"                 "clean_supporting"       
#>  [5] "df_print"                "pre_knit"               
#>  [7] "post_knit"               "pre_processor"          
#>  [9] "intermediates_generator" "post_processor"         
#> [11] "file_scope"              "on_exit"                
#> [13] "bookdown_output_format" 
```
