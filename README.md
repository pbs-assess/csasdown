# csasdown2 <img src="man/figures/logo.png" align="right" height="138" alt="csasdown" />

> Reproducible CSAS Reports with .docx output

<!-- badges: start -->
[![R-CMD-check](https://github.com/seananderson/csasdown2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/seananderson/csasdown2/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

> ⚠️ **Experimental Package**: This package is under active development.

csasdown2 is a rewrite of [csasdown](https://github.com/pbs-assess/csasdown) that focuses on generating .docx output compliant with [CSAS](https://www.dfo-mpo.gc.ca/csas-sccs/index-eng.htm) (Canadian Science Advisory Secretariat) formatting requirements, bilingual-language support, and accessibility standards.

## ✨ Features

- 📄 **Two document types**: Research Documents and Science Advisory Reports (FSAR/SAR)
- 🎨 **CSAS-compliant formatting**: Pre-configured Word templates with official styles
- 🔄 **Reproducible workflow**: Integrate R code, analysis, and narrative in one place
- 📚 **Multi-file support**: Organize large documents across multiple R Markdown files
- 🌍 **Bilingual support**: English and French language configurations
- ♿ **Accessibility**: Built with accessibility standards in mind
- 📊 **Automatic numbering**: Figures, tables, and cross-references handled automatically
- 📖 **Bibliography management**: Integrated citation and reference formatting

## 📦 Installation

Install the development version from GitHub:

``` r
# Using pak (recommended)
pak::pak("pbs-assess/csasdown2")

# Or using remotes
remotes::install_github("pbs-assess/csasdown2")
```

## 🚀 Quick Start

### Create a new Research Document

Create a new Research Document in the current working directory:

``` r
csasdown2::draft("resdoc")
```

Or specify an existing custom directory:

``` r
csasdown2::draft("resdoc", directory = "my-research-doc")
```

### Create a Science Advisory Report

``` r
csasdown2::draft("fsar")
```

### Render your document

Open `index.Rmd` and click the "Knit" button in RStudio, or run:

``` r
# For Research Documents
csasdown2::render()

# For Science Advisory Reports
csasdown2::render_sar()
```

Your compiled .docx file will appear in the `_book/` directory.

## 📁 Project Structure

After running `draft()`, your project will contain:

### Core Files

- **`index.Rmd`** 📝
  Main file containing YAML metadata (title, authors, dates, report numbers) and the first content section. This is where you configure document-wide settings.

- **`_bookdown.yml`** ⚙️
  Configuration file specifying the order of R Markdown files to merge, output filename, and other bookdown settings.

- **`01-introduction.Rmd`, `02-methods.Rmd`, etc.** 📄
  Chapter files for your document. Add, remove, or rename these as needed, updating `_bookdown.yml` to match.

- **`99-references.Rmd`** 📚
  Placeholder for the bibliography section (automatically populated from your .bib file).

### Supporting Directories

- **`figs/`** 🖼️
  Store external figures and images here. Reference them in your .Rmd files with relative paths.

- **`data/`** 💾
  Store data files (.csv, .rds, etc.) used in your analysis.

- **`csl/`** 🎓
  Citation Style Language files for formatting your bibliography.

- **`bib/`** 📖
  BibTeX files containing your references (e.g., `refs.bib`).

### Output

- **`_book/`** 📦
  Generated directory containing your compiled .docx file and intermediate files.

## 📝 Document Types

- Research documents in English and French
- Fisheries Science Advisory Reports in English and French
- Science Responses and Technical Reports to come

## 🛠️ Customization

### Modify YAML metadata

Edit the YAML header in `index.Rmd` to customize:
- Title
- Author names and affiliations
- Report numbers and dates
- Language settings
- Abstract text

### Add chapters

1. Create a new `.Rmd` file (e.g., `03-results.Rmd`)
2. Add it to `_bookdown.yml` in the desired order. For example:
```yaml
rmd_files:
  - "index.Rmd"
  - "01-introduction.Rmd"
  - "02-methods.Rmd"
  - "03-results.Rmd"
  - "99-references.Rmd"
```

### Figures

Use R code chunks with captions:

````markdown
```{r my-figure, fig.cap="My caption."}
plot(year, abundance)
```
````

Reference with `Figure \@ref(fig:my-figure)`.

Include an existing figure:

````markdown
```{r my-other-figure, fig.cap="My other caption."}
knitr::include_graphics("figs/myfigure.png", dpi = NA)
```
````

### Tables

Create a table:

````markdown
```{r my-table}
flextable::flextable(data) |> 
    set_caption(caption = "My caption.")
```
````

Reference with `Table \@ref(tab:my-table)`.

### Math

Create an equation:

```markdown
\begin{equation}
  1 + 1 = 2
  (\#eq:example-eq)
\end{equation}
```

Reference with `Equation \@ref(eq:example-eq)`.

Inline math:

```markdown
The symbol $\beta$ represents...
```

### References

Include citations:

```markdown
A statement [e.g., @Smith2024; @Johnson2025]. @Johnson2025 said this.
```

Turns into:

> A statement (e.g., Smith et al. 2024, Johnson et al. 2025). Johnson et al. (2025) said this.

With entries in the bibliography at the end.

### Appendices

Start an appendix:

```markdown
# APPENDIX `r new_appendix()`. ADDITIONAL ANALYSES {#app:additional}
```

Reference with `Appendix \@ref(app:additional)`

## 🔧 Under the Hood

csasdown2 uses a multi-stage rendering pipeline:

1. **bookdown** merges multiple R Markdown files
2. **officedown** converts to Word with CSAS styles
3. **officer** post-processes the document (injects frontmatter, replaces bookmarks, assembles final document)

## 🤝 Contributing

Bug reports and feature requests are welcome on the [GitHub issue tracker](https://github.com/pbs-assess/csasdown2/issues).

## 📄 License

This package is licensed under the MIT License.

## 🙏 Acknowledgments

csasdown2 builds on the original [csasdown](https://github.com/pbs-assess/csasdown) package and the excellent [bookdown](https://bookdown.org/), [officedown](https://davidgohel.github.io/officedown/), and [officer](https://davidgohel.github.io/officer/) packages.
