# Create appendix table caption

Helper function to create a properly formatted table caption for
appendices. This is useful when using
[`flextable::set_caption()`](https://davidgohel.github.io/flextable/reference/set_caption.html)
to ensure consistent formatting with automatic numbering.

## Usage

``` r
appendix_table_caption(caption, appendix_letter = NULL)
```

## Arguments

- caption:

  Character. The table caption text

- appendix_letter:

  Character. The appendix letter (e.g., "A", "B"). If NULL, uses the
  value set by
  [`set_appendix()`](https://pbs-assess.github.io/csasdown/reference/set_appendix.md)
  or
  [`new_appendix()`](https://pbs-assess.github.io/csasdown/reference/new_appendix.md)

## Value

Character. A formatted caption string

## Examples

``` r
library(flextable)
new_appendix()
#> [1] "A"
df <- data.frame(a = 1:3)
flextable(df) |>
  set_caption(appendix_table_caption("Summary statistics"))


.cl-139f4238{}.cl-13972a1c{font-family:'DejaVu Sans';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-139a73ac{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-139aac64{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-139aac6e{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-139aac78{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}

Summary statistics
a
```
