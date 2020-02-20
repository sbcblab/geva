
##########################
# TEXT PRINTING HELPER FUNCTIONS
# -----------------------
# 
# Helper methods to print object information
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

#' @include stringhelpers.R
#' @include vectorhelpers.R
#' @include callhelpers.R

# Writes a formatted text (same as sprintf) followed by a new line
catline <- function(fmt, ...)
{
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