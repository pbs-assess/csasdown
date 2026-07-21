# Render a CSAS report

This is the main rendering function for reports.

## Usage

``` r
render(config_file = "_bookdown.yml", verbose = FALSE, ...)
```

## Arguments

- config_file:

  YAML configuration file.

- verbose:

  Verbose?

- ...:

  Arguments to pass to
  [`bookdown::render_book()`](https://pkgs.rstudio.com/bookdown/reference/render_book.html).

## Value

A rendered `.docx` report.

## Details

`render()` automatically detects document type from the YAML in
`index.Rmd` and renders appropriately.

`render()` can be called from the command line as `csasdown::render()`
or via clicking the RStudio Knit button assuming the following YAML
argument is set in `index.Rmd`:

    knit: (function(input, ...) {csasdown::render()})

## Examples

``` r
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

# render:
csasdown::render()
#> ✔ Detected document type: resdoc
#> ✔ YAML validation passed
#> Rendering document with bookdown...
#> 
#> 
#> processing file: resdoc.Rmd
#> 1/31                          
#> 2/31 [setup]                  
#> 3/31                          
#> 4/31 [load-libraries-csasdown]
#> 5/31                          
#> 6/31 [table-settings]         
#> 7/31                          
#> 8/31 [load-libraries-user]    
#> 9/31                          
#> 10/31 [unnamed-chunk-1]        
#> 11/31                          
#> 12/31 [example-table]          
#> 13/31                          
#> 14/31 [example-fig]            
#> 15/31                          
#> 16/31 [example-fig2]           
#> 17/31                          
#> 18/31 [unnamed-chunk-2]        
#> 19/31                          
#> 20/31 [example-fig3]           
#> 21/31                          
#> 22/31 [fig-biology-example]    
#> 23/31                          
#> 24/31 [tab-biology-summary]    
#> 25/31                          
#> 26/31 [fig-biology-histogram]  
#> 27/31                          
#> 28/31 [fig-model-diagnostic]   
#> 29/31                          
#> 30/31 [tab-model-parameters]   
#> 31/31                          
#> output file: resdoc.knit.md
#> /opt/hostedtoolcache/pandoc/3.8.3/x64/pandoc +RTS -K512m -RTS resdoc.knit.md --to docx --from markdown+autolink_bare_uris+tex_math_single_backslash --output resdoc.docx --lua-filter /home/runner/work/_temp/Library/bookdown/rmarkdown/lua/custom-environment.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --metadata-file /tmp/RtmpY11UCA/file1bac6ade7428 --syntax-highlighting tango --reference-doc /home/runner/work/_temp/Library/csasdown/csas-docx/resdoc-content-2026.docx --lua-filter /home/runner/work/_temp/Library/csasdown/rmarkdown/lua/loose-lists.lua --syntax-highlighting=none --metadata link-citations=true --csl csl/csas.csl '--metadata=title:' --extract-media resdoc_files --citeproc 
#> 
#> Output created: resdoc.docx
#> ✔ Bookdown rendering complete
#> ✔ Moved output to _book/resdoc.docx
#> ✔ Render complete!
#> ✔ You've got this down to a science 🔬

# return to original working directory after running example:
setwd(wd)

# clean up:
unlink(example_path, recursive = TRUE, force = TRUE)
```
