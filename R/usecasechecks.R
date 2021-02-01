

##########################
# USE CASE CHECK FUNCTIONS
# -----------------------
# 
# Functions to deal with specific use cases, usually stopping or warning the user when something occurred
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al

#' @include callhelpers.R
NULL

# ---------------------
# GEVAInput
check.suitable.factors <- function(fs, warn=TRUE, msg=TRUE)
{
  if (length(levels(fs)) == 1)
  {
    if (warn) warning("'factors' has only one level. All analyses using factors will be ignored")
    if (msg) catline("Factoring analysis ignored (two or more levels required)")
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

# ---------------------
# TypedList
check.typed.list.class <- function(tlist, elemclass)
{
  if (!(extends(elem.class(tlist), elemclass) && all(vapply(tlist, inherits, FALSE, what=elemclass))))
  {
    lsnm = call.objname(tlist, 1L)
    stop("elements in '%s' must be %s", lsnm, elemclass)
  }
  invisible(TRUE)
}

# ---------------------
# GEVAQuantiles
check.quantiles.count <- function(qs, qv)
{
  if (qs < 3L || qv < 2L) stop("number of quantiles must be >= 3 for summary and >= 2 for variation")
  invisible(TRUE)
}

# ---------------------
# WANOVA's
check.factors.are.specific <- function(fs, warn=TRUE, msg=TRUE)
{
  lvls = levels(fs)
  is.spec = length(lvls) >= 2L
  if (!is.spec)
  {
    if (warn) warning("'factors' have less than 2 levels. Specific factoring analysis will not be performed")
    if (msg) catline("Factor-specific analysis ignored (less than 2 levels in factors)")
  }
  is.spec
}
