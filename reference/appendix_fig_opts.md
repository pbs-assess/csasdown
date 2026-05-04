# Set appendix figure chunk options

Internal function to set chunk options for appendix figures with
automatic numbering (e.g., A.1, A.2). This is called automatically by
[`set_appendix()`](https://pbs-assess.github.io/csasdown/reference/set_appendix.md)
and
[`new_appendix()`](https://pbs-assess.github.io/csasdown/reference/new_appendix.md).
Users should not need to call this directly.

## Usage

``` r
appendix_fig_opts(appendix_letter = NULL)
```

## Arguments

- appendix_letter:

  Character. The appendix letter (e.g., "A", "B"). If NULL, uses the
  value set by
  [`set_appendix()`](https://pbs-assess.github.io/csasdown/reference/set_appendix.md)

## Value

Invisible NULL (sets chunk options as a side effect)
