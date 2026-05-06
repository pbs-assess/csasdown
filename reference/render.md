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
#> 1/29                          
#> 2/29 [setup]                  
#> 3/29                          
#> 4/29 [load-libraries-csasdown]
#> 5/29                          
#> 6/29 [table-settings]         
#> 7/29                          
#> 8/29 [load-libraries-user]    
#> 9/29                          
#> 10/29 [example-table]          
#> 11/29                          
#> 12/29 [example-fig]            
#> 13/29                          
#> 14/29 [example-fig2]           
#> 15/29                          
#> 16/29 [unnamed-chunk-1]        
#> 17/29                          
#> 18/29 [example-fig3]           
#> 19/29                          
#> 20/29 [fig-biology-example]    
#> 21/29                          
#> 22/29 [tab-biology-summary]    
#> 23/29                          
#> 24/29 [fig-biology-histogram]  
#> 25/29                          
#> 26/29 [fig-model-diagnostic]   
#> 27/29                          
#> 28/29 [tab-model-parameters]   
#> 29/29                          
#> output file: resdoc.knit.md
#> /opt/hostedtoolcache/pandoc/3.8.3/x64/pandoc +RTS -K512m -RTS resdoc.knit.md --to docx --from markdown+autolink_bare_uris+tex_math_single_backslash --output resdoc.docx --lua-filter /home/runner/work/_temp/Library/bookdown/rmarkdown/lua/custom-environment.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --metadata-file /tmp/RtmpZ6zbJz/file1c3ddf498ba --syntax-highlighting tango --reference-doc /home/runner/work/_temp/Library/csasdown/csas-docx/resdoc-content-2026.docx --lua-filter /home/runner/work/_temp/Library/csasdown/rmarkdown/lua/loose-lists.lua --syntax-highlighting=none --metadata link-citations=true --csl csl/csas.csl '--metadata=title:' '--metadata=abstract:' --extract-media resdoc_files --citeproc 
#> 
#> Output created: resdoc.docx
#> ✔ Bookdown rendering complete
#> ✔ Moved output to _book/resdoc.docx
#> ✔ Render complete!
#> ✔ You've done it again — smooth rendering all the way 🌊

# return to original working directory after running example:
setwd(wd)

# clean up:
unlink(example_path, recursive = TRUE, force = TRUE)
```
