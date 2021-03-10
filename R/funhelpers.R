
##########################
# FUNCTION HELPER METHODS
# -----------------------
# 
# Helper methods for function manipulation
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al

#' @include callhelpers.R
#' @include stringhelpers.R
NULL

# Gets the variables at the left, right, or both sides of a formula
vars.formula <- function(formula, side=c('both', 'left', 'right'), unique=TRUE)
{
  if (!is(formula, 'formula')) stop("must provide a formula as argument")
  side = match.arg(side)
  twolen = length(formula) <= 2L
  vars = switch(side,
                both = all.vars(formula, unique=unique),
                left = if (twolen) character(0) else all.vars(formula[[2L]], unique=unique),
                right = all.vars(formula[[ifelse(twolen, 2L, 3L)]], unique=unique)
                )
  vars
}

# Inserts the arguments in dots inside a function environment
insert.function.optargs <- function(fn, ..., .selector=NULL)
{
  if (!is.function(fn)) stop("must provide a function as argument")
  if (...length() == 0L) return(fn)
  argnms = call.dots.argnames(...)
  if (length(argnms) == 0L) argnms = sprintf("..%s", 1L:...length())
  if (is.null(.selector)) .selector = 1L:...length()
  if (is.character(.selector)) .selector = .selector %in% argnms
  if (is.logical(.selector)) .selector = which(.selector)
  if (!is.integer(.selector)) .selector = as.integer(.selector)
  .selector = .selector[.selector >= 1 & .selector <= ...length()]
  if (length(.selector) == 0) return(fn)
  fenv = environment(fn)
  for (opti in .selector)
  {
    argnm = argnms[[opti]]
    if (nchar(argnm) == 0L) argnm = sprintf("..%s", opti)
    fenv[[argnm]] = ...elt(opti)
  }
  return(fn)
}

# Converts a formula to a function based on the given variables. Argument names in dots will be removed in final function
formula2function <- function(formula, ...)
{
  if (!is(formula, 'formula')) stop("must provide a formula as argument")
  nms = call.dots.namesorargs(...)
  dvars = vars.formula(formula, 'right')
  vardef = dvars %in% nms
  undefvars = dvars[!vardef]
  fbody = deparse(formula[[length(formula)]])
  fn = eval(parse(text=sprintf('function(%s){%s}', strjoin(unique(c(undefvars, '...')), ', '), fbody)), envir = new.env())
  if (any(vardef))
  {
    argnms = call.dots.argnames(...)
    dvinds = which(argnms %in% dvars)
    fenv = environment(fn)
    insert.function.optargs(fn, ..., .selector = dvinds)
  }
  fn
}

# Converts a function to a call using the specified arguments
function2expression <- function(fn, ..., args.list=NULL)
{
  fname = deparse(substitute(fn))
  forms = formals(fn)
  forms$... = NULL
  fargnms = names(forms)
  forms = lapply(forms, deparse)
  args.list = lapply(lapply(args.list, function(a) if (is.expression(a) || is.call(a)) as.character(a) else deparse(a)), as.character)
  dotsls = lapply(as.list(substitute(list(...)))[-1L], deparse)
  vargs = unlist(list.merge(dotsls, args.list, forms))
  vargs = vargs[nchar(vargs) != 0L]
  if (length(vargs) == 0L) return(parse(text=sprintf("%s()", fname)))
  argnms = if(is.named(vargs)) names(vargs) else rep("", length(vargs))
  argorder = order(match(argnms, fargnms, nomatch = length(vargs) + 1L))
  argnms = argnms[argorder]
  vargs = vargs[argorder]
  argnms = vapply(argnms, function(nm) if(nchar(nm) == 0L) "" else sprintf("%s = ", nm), "")
  fncall = parse(text=sprintf("%s(%s)", fname, paste0(paste0(argnms, vargs), collapse=", ") ))
  fncall
}
