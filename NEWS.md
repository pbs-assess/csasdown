# csasdown2 0.0.0.9005

* Simplified to only need an `authors` YAML field. Translation and the author
  list parsing are done internally.
  
* Added vertical space between author addresses.

* Fixed pandoc compatibility by detecting version and using `--syntax-highlighting=none`
  for pandoc >= 3.8 or `--highlight-style=none` for older versions.