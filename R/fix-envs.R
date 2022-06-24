#' Post-process LaTeX code
#'
#' @description
#' A post-processor to modify LaTeX code after the document has been knit
#'
#' @keywords internal
#' @param x The LaTeX code as a vector of character strings, one for each line
#' @param include_abstract Logical. If `TRUE` include an abstract page
#' @param prepub Logical. If `TRUE` the document if pre-publication. Currently
#' only implemented for SR documents
#' @param highlight The code highlight type. Must be one of "pygments", "tango",
#' "espresso", "zenburn", "kate", "monochrome", "breezedark", "haddock"
#' @param include_section_nums Logical. If `TRUE`, Add section numbers to the
#' document. If `FALSE` the sections will not be numbered
#' @param fix_ref_section_name Logical. If `TRUE`, the REFERENCES section
#' will be renamed to French version
#'
#' @return The modified LaTeX code as a vector of character strings, one for
#' each line
fix_envs <- function(x,
                     include_abstract = TRUE,
                     prepub = FALSE,
                     highlight = "tango",
                     include_section_nums = TRUE,
                     fix_ref_section_name = FALSE) {

  # fix equations:
  x <- gsub("^\\\\\\[$", "\\\\begin{equation}", x)
  x <- gsub("^\\\\\\]$", "\\\\end{equation}", x)
  x <- gsub("^\\\\\\]\\.$", "\\\\end{equation}.", x)
  x <- gsub("^\\\\\\],$", "\\\\end{equation},", x)

  # Get region line
  region_line <- grep(pattern = "% Region", x) + 1
  # If region is specified
  if (length(region_line) > 0) {
    # Get region name and contact info
    pat <- "\\\\rdRegion\\}\\{(.*?)\\}+$"
    region_vec <- regmatches(
      x = x[region_line],
      m = regexec(
        pattern = pat,
        text = x[region_line]
      )
    )[[1]]
    region <- region_supplied <- region_vec[2]
    region_def_ind <- grep(pat, x)
    if(fr()){
      # If the author supplied an English region name for a French doc, convert it
      eng_match <- grep(region_supplied, region_info$Region)
      if(length(eng_match)){
        region <- region_info[eng_match, ]$RegionFr
        if(length(region_def_ind)){
          x[region_def_ind] <- gsub(region_supplied, region, x[region_def_ind])
        }
      }
    }else{
      # If the author supplied an French region name for a English doc, convert it
      fr_match <- grep(region_supplied, region_info$RegionFr)
      if(length(fr_match)){
        region <- region_info[fr_match, ]$Region
        if(length(region_def_ind)){
          x[region_def_ind] <- gsub(region_supplied, region, x[region_def_ind])
        }
      }
    }
    contact_info <- get_contact_info(region = region)
    # Insert contact info
    x <- sub(
      pattern = "AddressPlaceholder", replacement = contact_info$address,
      x = x
    )
    x <- sub(
      pattern = "EmailPlaceholder",
      replacement = paste0(
        "\\\\link\\{mailto:", contact_info$email,
        "\\}\\{", contact_info$email, "\\}"
      ), x = x
    )
  } # End if region exists (SRs)
  ## Change csas-style to use the sty file found in csasdown repo
  g <- grep("csas-style", x)

  ## Find beginning and end of the abstract text if it is not a Science Response document
  if (include_abstract) {
    abs_beg <- grep("begin_abstract_csasdown", x)
    abs_end <- grep("end_abstract_csasdown", x)
    if (length(abs_beg) == 0L || length(abs_end) == 0L) {
      alert("`% begin_abstract_csasdown` or `% end_abstract_csasdown`` not found ",
            "in `templates/csas.tex`")
    } else {
      abs_vec <- x[seq(abs_beg + 1, abs_end - 1)]
      abs_vec <- abs_vec[abs_vec != ""]
      abstract <- paste(abs_vec, collapse = " \\vspace{1.5mm} \\break ")
      first_part <- x[seq_len(abs_beg - 1)]
      second_part <- x[seq(abs_end + 1, length(x))]
      x <- c(first_part, abstract, second_part)
    }
  }
  beg_reg <- "^\\s*\\\\begin\\{.*\\}"
  end_reg <- "^\\s*\\\\end\\{.*\\}"
  i3 <- if (length(i1 <- grep(beg_reg, x))) (i1 - 1)[grepl("^\\s*$", x[i1 - 1])]

  i3 <- c(
    i3,
    if (length(i2 <- grep(end_reg, x))) (i2 + 1)[grepl("^\\s*$", x[i2 + 1])]
  )
  if (length(i3)) x <- x[-i3]

  g <- grep("\\\\Appendices$", x)
  if (identical(length(g), 0L)) {
    appendix_line <- length(x) - 1 # no appendix
  } else {
    appendix_line <- min(g)
  }

  for (i in seq(1, appendix_line)) {
    x[i] <- gsub("\\\\subsection\\{", "\\\\subsubsection\\{", x[i])
    x[i] <- gsub("\\\\section\\{", "\\\\subsection\\{", x[i])
    x[i] <- gsub("\\\\chapter\\{", "\\\\section\\{", x[i])
  }

  for (i in seq(appendix_line + 1, length(x))) {
    x[i] <- gsub("\\\\section\\{", "\\\\appsection\\{", x[i])
    if (!fr()) {
      x[i] <- gsub(
        "\\\\chapter\\{",
        "\\\\starredchapter\\{APPENDIX~\\\\thechapter. ", x[i]
      )
    } else {
      x[i] <- gsub(
        "\\\\chapter\\{",
        "\\\\starredchapter\\{ANNEXE~\\\\thechapter. ", x[i]
      )
    }
  }
  x <- inject_refstepcounters(x)

  # Need to remove hypertarget for references to appendices to work:
  # rs_line <- grep("\\\\refstepcounter", x)
  # FIXME: make more robust
  rs_line <- grep("\\\\hypertarget\\{app:", x)
  x[rs_line + 0] <- gsub("hypertarget", "label", x[rs_line + 0])
  x[rs_line + 0] <- gsub("\\{%", "", x[rs_line + 0])
  x[rs_line + 1] <- gsub("\\}$", "", x[rs_line + 1])
  x[rs_line + 1] <- gsub("\\}.*\\}$", "}", x[rs_line + 1])

  x <- gsub("^.*\\\\tightlist$", "", x)

  # \eqref needs to be \ref so the equation references don't have () around them
  # https://tex.stackexchange.com/a/107425
  x <- gsub("\\\\eqref\\{", "\\\\ref\\{", x)

  # Non-breaking spaces:
  x <- gsub(" \\\\ref\\{", "~\\\\ref\\{", x)
  x <- gsub(" :", "~:", x) # French

  # ----------------------------------------------------------------------
  # Add tooltips so that figures have alternative text for read-out-loud
  figlabel_lines <- x[grep("\\\\label\\{fig:", x)]
  fig_labels <- gsub(
    "\\\\caption\\{(.*?)\\}\\\\label\\{fig:(.*?)\\}",
    "\\2", figlabel_lines
  )
  all_include_graphics <- grep("(\\\\includegraphics\\[(.*?)\\]\\{(.*?)\\})", x)

  # is this a true figure with a caption in Pandoc style?
  all_include_graphics <-
    all_include_graphics[grep("\\\\centering", x[all_include_graphics])]

  if (identical(length(fig_labels), length(all_include_graphics))) {
    for (i in seq_along(all_include_graphics)) {
      x[all_include_graphics[i]] <-
        gsub(
          "(\\\\includegraphics\\[(.*?)\\]\\{(.*?)\\})",
          paste0("\\\\pdftooltip{\\1}{", "Figure \\\\ref{fig:", fig_labels[i], "}}"),
          x[all_include_graphics[i]]
        )
    }
  } else {
    # nocov start
    alert("The number of detected figure captions did not match the number of ",
          "detected figures. Reverting to unnumbered alternative text figures.")
    x <- gsub(
      "(\\\\includegraphics\\[(.*?)\\]\\{(.*?)\\})",
      "\\\\pdftooltip{\\1}{Figure}", x
    )
    # nocov end
  }
  # ----------------------------------------------------------------------

  regexs <- c(
    "^\\\\CHAPTER\\*\\{R\\p{L}F\\p{L}RENCES", # English or French
    "^\\\\SECTION{SOURCES DE RENSEIGNEMENTS}",
    "^\\\\SECTION{SOURCES OF INFORMATION}"
  )
  .matches <- lapply(regexs, function(.x) grep(.x, toupper(x), perl = TRUE) + 1)
  references_insertion_line <- unlist(.matches)

  x[references_insertion_line - 1] <- sub("chapter", "section", x[references_insertion_line - 1])
  x[references_insertion_line] <- sub("chapter", "section", x[references_insertion_line])

  # Move the bibliography to before the appendices:
  if (length(references_insertion_line) > 0) {
    references_begin <- grep("^\\\\hypertarget\\{refs\\}\\{\\}$", x)
    if (length(references_begin) > 0) {
      references_end <- length(x) - 1
      x <- c(
        x[seq(1, references_insertion_line - 1)],
        # "\\phantomsection",
        x[references_insertion_line],
        "% This manually sets the header for this unnumbered chapter.",
        # "\\markboth{References}{References}",
        "\\noindent",
        "\\vspace{-2em}",
        "\\setlength{\\parindent}{-0.2in}",
        "\\setlength{\\leftskip}{0.2in}",
        "\\setlength{\\parskip}{8pt}",
        "",
        x[seq(references_begin, references_end)],
        "\\setlength{\\parindent}{0in} \\setlength{\\leftskip}{0in} \\setlength{\\parskip}{4pt}",
        x[seq(references_insertion_line + 1, references_begin - 1)],
        x[length(x)]
      )
      # Modify References from starred chapter to regular chapter so that it is numbered
      starred_references_line <- grep("\\\\section\\*\\{REFERENCES\\}\\\\label\\{references\\}\\}", x)
      if(length(starred_references_line)){
        x[starred_references_line] <- gsub("\\*", "", x[starred_references_line])
        # Remove the add contents line which was used to add the unnumbered section before
        add_toc_contents_line <- grep("\\\\addcontentsline\\{toc\\}\\{section\\}\\{REFERENCES\\}", x)
        x[add_toc_contents_line] <- ""
      }
      # Modify References section name here
      if(fix_ref_section_name){
        ref_ind <- grep("\\{REFERENCES", x)
        if(!length(ref_ind)){
          # nocov start
          bail("REFERENCES section header not found in the document. Make sure you ",
               "haven't commented out that section in _bookdown.yml or changed the header name")
          # nocov end
        }
        x[ref_ind] <- gsub("REFERENCES", ifelse(fr(),
                                                "R\u00c9F\u00c9RENCES CIT\u00c9ES",
                                                "REFERENCES CITED"), x[ref_ind])
      }

    } else {
      alert("Did not find the beginning of the LaTeX bibliography.") # nocov
    }
  }

  # Tech Report Appendices:
  x <- gsub(
    "\\% begin csasdown appendix",
    paste0(
      "\\begin{appendices}\n",
      "\\\\counterwithin{figure}{section}\n",
      "\\\\counterwithin{table}{section}\n",
      "\\\\counterwithin{equation}{section}"
    ),
    x
  )
  x <- gsub("\\% end csasdown appendix", "\\end{appendices}", x)

  label_app <- grep("^\\\\label\\{app:", x)
  for (i in seq_along(label_app)) {
    if (grepl("^\\\\section\\{", x[label_app[i] + 1])) {
      x[seq(label_app[i], label_app[i] + 1)] <- x[seq(label_app[i] + 1, label_app[i])]
    }
  }

  # Implement "Approved pre-publication" version (science response)
  if (prepub) {
    # Text to add
    addText <- ifelse(fr(), " -- PR\u00C9-PUBLICATION APPROUV\u00C9E}",
                      " -- APPROVED PRE-PUBLICATION}"
    )
    # 1. Modify header first page (report number)
    rn_loc_1 <- grep(pattern = "\\% Report number", x = x) + 1
    rn_loc_2 <- grep(pattern = "\\% End of report number", x = x) - 1
    if (rn_loc_1 != rn_loc_2) {
      bail("Can't find report number (report_number)")
    }
    rn_text <- x[rn_loc_1]
    rn_text_clean <- gsub(pattern = "\\}+$", replacement = "", x = rn_text)
    rn_text_new <- paste0(rn_text_clean, "}", addText)
    x[rn_loc_1] <- rn_text_new
    # 2. Modify short title
    st_loc_1 <- grep(pattern = "\\% Title short", x = x) + 1
    st_loc_2 <- grep(pattern = "\\% End of title short", x = x) - 1
    if (st_loc_1 != st_loc_2) bail("Can't find short title (title_short)")
    st_text <- x[st_loc_1]
    st_text_clean <- gsub(pattern = "\\}+$", replacement = "", x = st_text)
    st_text_new <- paste0(st_text_clean, addText)
    x[st_loc_1] <- st_text_new
    # 3. Modify citation (2 things)
    if (fr()) {
      # Edit french citation
      cite_head_fr <- grep(
        pattern = "La pr\\\\\'\\{e\\}sente publication doit \\\\\\^\\{e\\}tre cit\\\\\'\\{e\\}e comme suit~:",
        x = x
      )
      if (length(cite_head_fr) == 0) bail("Can't find French citation header")
      x[cite_head_fr] <- "Cite comme ceci (jusqu'\u00E0 la publication)~:"
      cite_loc_fr <- grep(
        pattern = "\\\\citeFr\\{\\\\rdWorkDoneYear\\{\\}/\\\\rdNumber\\{\\}\\}", x = x
      )
      if (length(cite_loc_fr) == 0) bail("Can't find French citation")
      x[cite_loc_fr] <- "\\citeFr{Sous presse}"
      # Nuke english citation
      cite_head_eng <- grep(
        pattern = "\\\\emph\\{Also available in English:\\}",
        x = x
      )
      if (length(cite_head_eng) == 0) bail("Can't find English citation header")
      x[cite_head_eng] <- ""
      cite_loc_eng <- grep(
        pattern = "\\\\citeEng\\{\\\\rdWorkDoneYear\\{\\}/\\\\rdNumber\\{\\}\\}", x = x
      )
      if (length(cite_loc_eng) == 0) bail("Can't find English citation")
      x[cite_loc_eng] <- ""
    } else {
      # Edit english citation
      cite_head_eng <- grep(
        pattern = "Correct Citation for this Publication:",
        x = x
      )
      if (length(cite_head_eng) == 0) bail("Can't find English citation header")
      x[cite_head_eng] <- "Correct citation (until published):"
      cite_loc_eng <- grep(
        pattern = "\\\\citeEng\\{\\\\rdWorkDoneYear\\{\\}/\\\\rdNumber\\{\\}\\}", x = x
      )
      if (length(cite_loc_eng) == 0) bail("Can't find English citation")
      x[cite_loc_eng] <- "\\citeEng{In press}"
      # Nuke french citation
      cite_head_fr <- grep(
        pattern = "\\\\emph\\{Aussi disponible en fran\\\\c\\{c\\}ais~:\\}",
        x = x
      )
      if (length(cite_head_fr) == 0) bail("Can't find French citation header")
      x[cite_head_fr] <- ""
      cite_loc_fr <- grep(
        pattern = "\\\\citeFr\\{\\\\rdWorkDoneYear\\{\\}/\\\\rdNumber\\{\\}\\}", x = x
      )
      if (length(cite_loc_fr) == 0) bail("Can't find French citation")
      x[cite_loc_fr] <- ""
    } # End modify citations
  } # End if prepub

  # Fix Res. Doc. 2013/092: -> Res. Doc. 2013/092.
  x <- gsub("Res\\. Doc\\. ([0-9]{4}/[0-9]{2,}):", "Res. Doc. \\1.", x)
  x <- gsub("MPO\\. Doc\\. de rech ([0-9]{4}/[0-9]{2,}):", "MPO. Doc. de rech \\1.", x)

  # Pandoc now turns DOIs into href in references but must be \link{} to have underline:
  x <- gsub("\\\\href\\{", "\\\\link\\{", x)

  # Fix Pandoc/LaTeX bug as of 2021-04-07 where
  # \leavevmode\vadjust pre{\hypertarget{ref-edwards2013}{}}%
  # gets created instead of
  # \leavevmode{\hypertarget{ref-edwards2013}{}}%
  # and creates error
  # ! You can't use `\vadjust' in vertical mode.
  #    \leavevmode\vadjust
  # pre{\hypertarget{ref-edwards2013}{}}%
  x <- gsub("\\\\vadjust pre", "", x)

  # Enable reference linking to subsections of appendices
  x <- add_appendix_subsection_refs(x)

  if(!include_section_nums){
    document_start_ind <- grep("^\\\\documentclass", x)
    pre_start <- x[1:document_start_ind]
    post_start <- x[(document_start_ind + 1):length(x)]
    inp_lines <- c("\\makeatletter",
                   "\\def\\@seccntformat#1{",
                   "  \\expandafter\\ifx\\csname c@#1\\endcsname\\c@section\\else",
                   "  \\expandafter\\ifx\\csname c@#1\\endcsname\\c@subsection\\else",
                   "  \\expandafter\\ifx\\csname c@#1\\endcsname\\c@subsubsection\\else",
                   "  \\csname the#1\\endcsname\\quad",
                   "  \\fi\\fi\\fi}",
                   "\\makeatother")
    x <- c(pre_start, inp_lines, post_start)
  }

  # Add the latex chunk for code highlighting
  theme_ind <- grep("^% Add theme here$", x)
  if(length(theme_ind)){
    themes <- c("pygments", "tango", "espresso", "zenburn", "kate", "monochrome", "breezedark", "haddock")
    pre_theme <- x[1:(theme_ind - 1)]
    post_theme <- x[(theme_ind + 1):length(x)]
    if(highlight %in% themes){
      theme_latex <- readLines(system.file("themes", paste0(highlight, ".latex"), package = "csasdown"))
    }else{
      theme_latex <- readLines(here(highlight))
    }
    x <- c(pre_theme, theme_latex, post_theme)
  }

  x
}