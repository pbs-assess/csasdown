# Set the current appendix letter

Sets the current appendix context for automatic figure and table
numbering. This function stores the appendix letter and automatically
configures both figure and table numbering. For most users,
[`new_appendix()`](https://pbs-assess.github.io/csasdown/reference/new_appendix.md)
is preferred as it auto-increments. Use this function when you need to
explicitly set a specific appendix letter.

## Usage

``` r
set_appendix(letter)
```

## Arguments

- letter:

  Character. The appendix letter (e.g., "A", "B", "C")

## Value

Invisible NULL

## Examples

``` r
# \donttest{
# Explicitly set to Appendix A
set_appendix("A")

# Explicitly set to Appendix B
set_appendix("B")
# }
```
