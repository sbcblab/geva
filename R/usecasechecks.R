

##########################
# USE CASE CHECK FUNCTIONS
# -----------------------
# 
# Functions to deal with specific use cases, usually stopping or warning the user when something occurred
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

#' @include callhelpers.R

# ---------------------
# GEVAInput
check.suitable.factors <- function(fs)
{
  if (length(levels(fs)) == 1)
  {
    warning("'factors' has only one level. All analyses using factors will be ignored")
    return(invisible(F))
  }
  invisible(T)
}

# ---------------------
# TypedList
check.typed.list.class <- function(tlist, elemclass)
{
  if (!(extends(elem.class(tlist), elemclass) && all(sapply(tlist, inherits, elemclass))))
  {
    lsnm = call.objname(tlist, 1L)
    stop("elements in '%s' must be %s", lsnm, elemclass)
  }
  invisible(T)
}

# ---------------------
# GEVAQuantiles
check.quantiles.count <- function(qs, qv)
{
  if (qs < 3L || qv < 2L) stop("number of quantiles must be >= 3 for summary and >= 2 for variation")
  invisible(T)
}
