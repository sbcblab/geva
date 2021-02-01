
##########################
# TEXT PRINTING HELPER FUNCTIONS
# -----------------------
# 
# Helper methods to print object information
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al

#' @include stringhelpers.R
#' @include vectorhelpers.R
#' @include callhelpers.R
NULL

# Writes a formatted text (same as sprintf) followed by a new line
catline <- function(fmt, ...)
{
  if (!is.character(fmt)) fmt = as.character(fmt)
  lin = sprintf(fmt, ...)
  cat(lin)
  cat("\n")
  invisible(lin)
}

# Formats a vector with limits, appending an ellipsis when the limit is exceeded
fmt.limit <- function(arr, limit=6, mode=c('headtail', 'head', 'tail'), ellipsis=' ... (+%d)', empty.text='<none>')
{
  fmt = ""
  mode = match.arg(mode[1], call.default.arg(mode))
  if (is.na(limit) || limit <= 0) limit = getOption('max.print', 6)
  if (is.null(arr) || length(arr) == 0) return(empty.text)
  if (is.vector(arr))
  {
    fmt = strjoin(do.call(mode, list(x=arr, n=limit)), ', ')
    if (is.character(ellipsis) && nchar(ellipsis) != 0 && length(arr) > limit)
    {
      if (grepl('\\%d', ellipsis))
      {
        nextra = as.integer(length(arr) - limit)
        ellipsis = sprintf(ellipsis, nextra)
      }
      fmt = fmt + ellipsis
    }
  } else
  {
    fmt = deparse(as.character(arr))
  }
  fmt
}

# Checks if there's a 'verbose' variable defined as TRUE in parent function
is.verbose <- function(prevfns=0L)
{
  prevfns = prevfns + 1L
  for (i in head(seq.int(sys.parent(), to = sys.parents()[1]), n = prevfns))
  {
    mcall = as.list(match.call(sys.function(i), sys.call(i))[-1])
    mcall = mcall[setdiff(names(mcall), formalArgs(sys.function(i)))]
    if (is.logical(mcall[['verbose']]))
      return(any(mcall[['verbose']], na.rm = TRUE))
    if ("verbose" %in% ls(sys.frame(i)) && is.logical(sys.frame(i)$verbose))
      return(any(sys.frame(i)$verbose, na.rm = TRUE))
  }
  defvars = ls(envir = parent.frame(prevfns))
  return(!('verbose' %in% defvars) || all(eval.parent(quote(verbose), prevfns) == TRUE) == TRUE)
}



# Performs a print if verbose is activated. Arguments can be formatted as in sprintf
vprint <- function(x, ..., new.line=TRUE)
{
  isverb = tryCatch(is.verbose(2L), error = function(e) FALSE)
  if (isverb)
  {
    if (is.character(x)) cat(sprintf(x, ...)) else print(x)
    if (new.line) cat('\n')
  }
  invisible(isverb)
}
