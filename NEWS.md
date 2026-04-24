# csasdown development version

## csasdown 0.0.0.90023

* Add a standalone test that exercises resdoc abstract preprocessing and the
  custom `{abstract}` knitr engine, including inline-R evaluation capture to
  `tmp-abstract.md` while excluding abstract body content from rendered output.

## csasdown 0.0.0.90022

* Rework resdoc abstract handling to capture and evaluate the abstract from a
  temporary `{abstract}` knitr engine sidecar file, insert it into frontmatter
  via bookmark markdown replacement, and avoid extracting/removing abstract
  content from rendered Word output.

## csasdown 0.0.0.90021

* Optimize bookmark markdown replacement by vectorizing conversions through a 
  single `rmarkdown::pandoc_convert()` call and reducing repeated docx zip/unzip 
  work during multi-bookmark updates.

## csasdown 0.0.0.90020

* Fix resdoc frontmatter region and year substitution by replacing the
  cover-page header bookmarks instead of trying to edit the document body.

## csasdown 0.0.0.90019

* Add auto-detection of aspect ratio for external png/jpeg figures. #25

## csasdown 0.0.0.90018

* Speed up Res Doc rendering by switching the frontmatter/content
  merge from `officer::body_import_docx()` to `officer::body_add_docx()`.
  Note that this does not preserve the Roman numeral page numbers in the
  frontmatter. For now, this will have to be manually fixed after the fact.
  This switch speeds up rendering time of large documents by ~10 fold. #22

## csasdown 0.0.0.90017

* Add example of how to include an externally created .png figure with the 
  correct aspect ratio. See the Res Doc draft example template.
  <https://github.com/pbs-assess/csasdown/blob/main/inst/rmarkdown/templates/resdoc/skeleton/01-introduction.Rmd>

## csasdown 0.0.0.90016

* Speed up `fix_table_cell_styles_xml()` on documents with many tables
  by optimizing regular expression searches on table cell XML. #22

## csasdown 0.0.0.90015

* Enhance documentation.

* Detect raw `<`/`>` in `.bib` `doi` fields before rendering and raise a clear
  error pointing to file/line and the `&lt;`/`&gt;` fix. #19

* Display positive affirmations on successful rendering. :)

* Use `csl/csas-french.csl` automatically when `french: true` is set in docx
  output options. #21

* Fix missing spacing between `Body Text` paragraphs in Tech Reports. #20

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
