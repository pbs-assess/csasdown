# Start a new appendix with auto-incremented letter

Automatically increments to the next appendix letter (A → B → C, etc.)
and configures figure and table numbering for that appendix. This is the
primary function users should call at the start of each appendix.

## Usage

``` r
new_appendix()
```

## Value

Character. The appendix letter that was set.

## Examples

``` r
# \donttest{
# At the start of your first appendix
new_appendix()  # Sets to "A"
#> [1] "C"

# At the start of your second appendix
new_appendix()  # Sets to "B"
#> [1] "D"

# Use in heading: # APPENDIX `r new_appendix()`. BIOLOGICAL DATA
# }
```
