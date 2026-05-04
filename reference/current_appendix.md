# Get the current appendix letter

Returns the current appendix letter. Useful for including in appendix
headings to ensure the heading matches the automatic numbering.

## Usage

``` r
current_appendix()
```

## Value

Character. The current appendix letter, or "A" if none has been set.

## Examples

``` r
# \donttest{
new_appendix()
#> [1] "B"
current_appendix()  # Returns "A"
#> [1] "B"

# Use in heading: # APPENDIX `r current_appendix()`. TITLE HERE
# }
```
