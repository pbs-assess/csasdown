# csasdown development version

* Fix rendering of FSARs (was missing a .docx file). #18

* Allow numeric years etc. in YAML. #5

* Eliminate warning on render of tech report. #10

* Fix styles in French figure captions. #8

* Add Science Response template. English only at first.

* Simplified to only need an `authors` YAML field. Translation and the author
  list parsing are done internally.
  
* Added vertical space between author addresses.

* Fixed pandoc compatibility by detecting version and using `--syntax-highlighting=none`
  for pandoc >= 3.8 or `--no-highlight` for older versions.