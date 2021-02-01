
##########################
# MATRIX HELPER FUNCTIONS
# -----------------------
# 
# Helper methods to manipulate matrices and data.frames
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al

#' @include callhelpers.R
#' @include vectorhelpers.R
#' @include funhelpers.R
NULL

# Multi-row apply, where a function is applied to each row of a matrix. If more matrices are used as arguments
mrapply <- function(FUN, ...)
{
  FUN = match.fun(FUN)
  vignore = call.dots.asis(...)
  nargs = ...length()
  if (nargs == 0L) stop("at least one two-dimensional object must be used as argument")
  fargnms = formalArgs(FUN)
  hasdots = '...' %in% fargnms
  fargs = as.list(formals(FUN)[!vapply(formals(FUN), function(e) is.name(e) || is.call(e), FALSE)])
  largs = list(...)
  argnms = call.dots.argnames(...)
  if (is.null(argnms)) argnms = rep('', nargs)
  vdim = NULL
  uanms = setdiff(fargnms, c('...', '', argnms))
  largs.unamed = largs[nchar(argnms) == 0]
  j = 1L
  lcalls = list()
  fenv = new.env(parent=environment(FUN))
  for (i in seq_len(nargs))
  {
    arg = largs[[i]]
    argnm = argnms[i]
    if (nchar(argnm) == 0L)
    {
      if (j > length(uanms)) next
      argnm = uanms[j]
      j = j + 1L
    }
    if (hasdots || argnm %in% fargnms)
    {
      fenv[[argnm]] = arg
      if (argnm %in% vignore)
      {
        lcalls[[argnm]] = argnm
        next
      }
      if (is.matrix(arg))
      {
        argdim = dim(arg)
        if (is.null(vdim)) vdim = argdim
        if (!any(vdim != argdim))
        {
          lcalls[[argnm]] = sprintf("%s[.inds]", argnm)
        }
      }
      else if (!is.null(vdim) && is.vector(arg) && length(arg) == vdim[1])
      {
        lcalls[[argnm]] = sprintf("%s[.inds[1]]", argnm)
      }
      else lcalls[[argnm]] = argnm
    }
  }
  if (is.null(vdim)) stop("none of the provided arguments in a two-dimensional object")
  minds = matrix(seq_len(prod(vdim)), nrow=vdim[1], ncol=vdim[2])
  fenv$.fn = FUN
  fn = eval(parse(text=sprintf("function(.inds) .fn(%s)", paste(names(lcalls), lcalls, sep = '=', collapse = ', ' ) )), envir = fenv)
  vres = apply(minds, 1, fn)
  vres
}

# Applies a function that returns a logical vector, then applies the 'any' function to each vector margin
# By default, this applies the any to a function for each row (MARGIN=1)
any.apply <- function(X, FUN, MARGIN=1, ...)
{
  msel = apply(X, MARGIN, FUN, ...)
  if (length(MARGIN) == 1L && MARGIN == 1) msel = t(msel)
  vsel = apply(msel, MARGIN, any)
  vsel
}

# Applies a function that returns a logical vector, then applies the 'all' function to each vector margin
# By default, this applies the any to a function for each row (MARGIN=1)
all.apply <- function(X, FUN, MARGIN=1, ...)
{
  msel = apply(X, MARGIN, FUN, ...)
  if (length(MARGIN) == 1L && MARGIN == 1) msel = t(msel)
  vsel = apply(msel, MARGIN, all)
  vsel
}
