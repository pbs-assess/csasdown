#' Parse a string to ensure correct parentheses matching and find the end
#' of a [cat()] call
#'
#' @description
#' Simple character parser with stack implementation used to match parentheses
#' in the input string and to find the final closing paren for the [cat()] call
#' which must start off the string
#'
#' @details
#' A vector of strings is passed in starting with 'cat(' but is is unknown
#' where this call ends, i.e. where is the matching ')' for this call.
#' This function is a simple parser to parse the vector text one character
#' at a time, matching parentheses of arbitrary complexity as it goes,
#' trying to find the matching ')' to end the 'cat('. Ad a side effect,
#' the paren matching algorithm sill detect incorrectly nested parentheses
#' and throw an error.
#'
#' Either single or double quotes can be used to surround the text passed
#' to [cat()] in the string.
#'
#' @keywords internal
#'
#' @param str_vec A vector of strings which starts with a 'cat('
#' @param ret_inds Logical. If `TRUE`, return the indices of the input
#' vector instead of the actual values. If `FALSE`, return the values
#' (default behaviour)
#' @param verbose Show details about what was matched and pushed to or
#' popped off the stack
#'
#' @return The text that is actually inside the 'cat(' call (as a vector
#' of strings, same as what was passed in). The 'cat(' and ')' are not
#' included in the return text
#'
#' @importFrom crayon cyan red
parse_cat_text <- function(str_vec, ret_inds = FALSE, verbose = FALSE){


  if(!length(grep("^cat\\(.*", str_vec[1]))){
    bail("The first line of ", csas_color("str_vec"), " must start with ",
         csas_color("cat()"))
  }
  str_vec[1] <- gsub("^cat\\(", "", trimws(str_vec[1]))
  if(length(grep("^\"", trimws(str_vec[1])))){
    # `cat()` text is surrounded by double quotes
    quote_type <- "\""
  }else if(length(grep("^'", trimws(str_vec[1])))){
    # `cat()` text is surrounded by single quotes
    quote_type <- "'"
  }else{
    bail("Quote type not allowed for ", csas_color("cat()"), " text. ",
         "You must use either ",
         "single or double quotes. You used ", substr(trimws(str_vec[1]), 1, 1))
  }

  # Parse (possibly nested) parens in the text
  pstack <- NULL
  # Push the `cat(` '(' to the stack initially
  pstack <- stk_push(pstack, "(")
  if(verbose){
    notify("Pushed '(' from ", csas_color("cat()"), " to stack. ",
           "Stack size is now ", stk_size(pstack), "\n")
  }
  matched <- FALSE
  prev_char <- NULL
  for(.y in seq_along(str_vec)){
    for(char_pos in seq_len(nchar(trimws(str_vec[.y])))){
      char <- substr(trimws(str_vec[.y]), char_pos, char_pos)
      if(char == "("){
        pstack <- stk_push(pstack, "(")
        if(verbose){
          notify("Pushed '(' to stack on line ", .y, ", char ", char_pos, "\n",
                 "stack size is now ", stk_size(pstack), "\n",
                 cyan(substr(trimws(str_vec[.y]), 1, char_pos - 1)),
                 red(substr(trimws(str_vec[.y]), char_pos, char_pos)),
                 cyan(substr(trimws(str_vec[.y]), char_pos + 1 , nchar(trimws(str_vec[.y])))),
                 "\n")
        }
      }
      if(char == ")"){
        # Close parens are never pushed to the stack, only matched with a '('
        # on top of the stack
        tmp <- stk_pop(pstack)
        pstack <- tmp$stack
        popval <- tmp$val
        # nocov start
        if(is.null(popval)){
          bail("No preceeding '(' to match ')' found on line  ", .y, ", char ",
               char_pos, "\n",
               cyan(substr(trimws(str_vec[.y]), 1, char_pos - 1)),
               red(substr(trimws(str_vec[.y]), char_pos, char_pos)),
               cyan(substr(trimws(str_vec[.y]), char_pos + 1 , nchar(trimws(str_vec[.y])))),
               "\n")
        }
        # nocov end
        if(prev_char == quote_type){
          # Matched the end ')' of cat()
          if(stk_size(pstack)){
            bail("Extra '(' matches this ')' on line  ", .y, ", char ", char_pos, "\n",
                 cyan(substr(trimws(str_vec[.y]), 1, char_pos - 1)),
                 red(substr(trimws(str_vec[.y]), char_pos, char_pos)),
                 cyan(substr(trimws(str_vec[.y]), char_pos + 1 , nchar(trimws(str_vec[.y])))),
                 "\n")
          }
          if(verbose){
            notify("Matched closing ')' for `cat()` on line ", .y, ", char ", char_pos, "\n",
                   "stack size is now ", stk_size(pstack), "\n",
                   cyan(substr(trimws(str_vec[.y]), 1, char_pos - 1)),
                   red(substr(trimws(str_vec[.y]), char_pos, char_pos)),
                   cyan(substr(trimws(str_vec[.y]), char_pos + 1 , nchar(trimws(str_vec[.y])))),
                   "\n")
          }
          if(char_pos != nchar(trimws(str_vec[.y]))){
            bail("Extra characters follow the ')' that closes `cat()`.\n",
                 "Closing ')' for ", csas_color("cat()"), " must be at the ",
                 "end of the line \n",
                 "with nothing following it (check for trailing whitespace):\n\n",
                 cyan(substr(trimws(str_vec[.y]), 1, char_pos - 1)),
                 red(substr(trimws(str_vec[.y]), char_pos, char_pos)),
                 cyan(substr(trimws(str_vec[.y]), char_pos + 1 , nchar(str_vec[.y]))),
                 "\n")
          }
          # Remove ')' that closes `cat()`
          ws <- gsub("^(\\s*).*", "\\1", str_vec[.y]) # leading whitespace
          str_vec[.y] <- paste0(ws, substr(trimws(str_vec[.y]), 1, char_pos - 1))
          # This is the total return chunk
          out_chunk <- str_vec[1:.y]
          if(ret_inds){
            return(1:.y)
          }
          matched <- TRUE
          break
        }
        if(!stk_size(pstack)){
          bail("No matching '(' for this ')' on line  ", .y, ", char ", char_pos, "\n",
               cyan(substr(trimws(str_vec[.y]), 1, char_pos - 1)),
               red(substr(trimws(str_vec[.y]), char_pos, char_pos)),
               cyan(substr(trimws(str_vec[.y]), char_pos + 1 , nchar(str_vec[.y]))),
               "\n")
        }
        if(verbose){
          notify("Matched ')' on line ", .y, ", char ", char_pos, "\n",
                 "stack size is now ", stk_size(pstack), "\n",
                 cyan(substr(trimws(str_vec[.y]), 1, char_pos - 1)),
                 red(substr(trimws(str_vec[.y]), char_pos, char_pos)),
                 cyan(substr(trimws(str_vec[.y]), char_pos + 1 , nchar(str_vec[.y]))),
                 "\n")
        }
      }
      prev_char <- char
    }
    if(matched){
      break
    }
  }
  out_chunk
}
