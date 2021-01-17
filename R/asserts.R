
##########################
# ASSERTION FUNCTIONS
# -----------------------
# 
# Assertion methods to check arguments inside functions
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al

#' @include callhelpers.R
#' @include printhelpers.R
#' @include stringhelpers.R
NULL

# Base function to assert that the result of calling functions matches the input arguments
# The 'posscalls' argument must be a list like: list(functionname = "Error message objname %s: expected %s, got %s")
assert.callres <- function(obj, posscalls, prevframes=2, pastefn=base::deparse, ...)
{
  objname = call.objname(obj, prevframes)
  argls = list(...)
  argnms = names(argls)
  for (callnm in intersect(argnms, names(posscalls)))
  {
    callres = do.call(callnm, list(obj))
    callinput = argls[[callnm]]
    if (all.equal(callinput, callres)[1] != TRUE)
    {
      stop(sprintf(posscalls[[callnm]],
                   objname,
                   pastefn(callinput),
                   pastefn(callres)))
    }
  }
  invisible(T)
}

# Asserts that 'arr' has the correct dimensions specified by (<function name> = <expected result>) in '...'
# Valid arguments are nrow, ncol, dim, and length
assert.dim <- function(arr, ...)
{
  posscalls = list(
    nrow = "Invalid number of rows in '%s': expected %s, got %s",
    ncol = "Invalid number of columns in '%s': expected %s, got %s",
    dim = "Invalid dimensions in '%s': expected %s, got %s",
    length = "Invalid length in '%s': expected %s, got %s"
  )
  assert.callres(arr, posscalls, ...)
  invisible(T)
}

# Asserts that 'arr' has the same character sequence as the result of a call applied to it
# Valid arguments are names, rownames, colnames
assert.names.equal <- function(arr, ...)
{
  posscalls = list(
    names = "Mismatching names in '%s': expected [%s], got [%s]",
    rownames = "Mismatching row.names in '%s': expected [%s], got [%s]",
    colnames = "Mismatching col.names in '%s': expected [%s], got [%s]",
    levels = "Mismatching levels in '%s': expected [%s], got [%s]"
  )
  assert.callres(arr, posscalls, pastefn = fmt.limit, ...)
  invisible(T)
}

# Asserts that the length of a list or array is not zero. This function accepts multiple arguments
assert.notempty <- function(..., .posmsg = "'%s' must contain at least one element")
{
  arglen = ...length()
  if (arglen != 0)
  {
    argnms = call.dots.argnames(...)
    for (i in 1:arglen)
    {
      objname = argnms[i]
      if (length(...elt(i)) == 0) stop(sprintf(.posmsg, objname))
    }
  }
  invisible(T)
}

# Asserts that all elements in a collection (needle) are included in another collection (arr)
assert.included <- function(needle, arr, elemnames = "elements")
{
  nmneedle = call.objname(needle, 1)
  nmarr = call.objname(arr, 1)
  misselems = !(needle %in% arr)
  if (any(misselems)) stop(sprintf("Some %s in '%s' are missing in '%s': [%s]", elemnames, nmneedle, nmarr, fmt.limit(needle[misselems])))
  invisible(T)
}

# Asserts that the object belongs, inherits or is related to an class
assert.class <- function(object, ...)
{
  objname = call.objname(object, 1)
  argls = list(...)
  posscalls = list(
    class = "'%s' must be %s",
    is = "'%s' must be a %s",
    inherits = "'%s' must inherit the %s class",
    typeof = "'%s' must be a %s object"
  )
  callfns = list(
    class = function(obj, cl) any(class(obj) %in% cl, na.rm = TRUE),
    is = function(obj, cl) any(sapply(cl, is, object=obj)),
    typeof = function(obj, cl) typeof(obj) %in% cl
  )
  for (fnm in intersect(names(argls), names(posscalls)))
  {
    reqclass = argls[[fnm]]
    if (length(reqclass) == 1L && reqclass == 'ANY') next
    fn = if (fnm %in% names(callfns)) callfns[[fnm]] else fnm
    if (!(do.call(fn, list(object, reqclass))))
    {
      stop(sprintf(posscalls[[fnm]], objname, strconjunct(reqclass, conj = " or ")))
    }
  }
  invisible(T)
}

# Asserts that the argument of a funtion
assert.choices <- function(arg, accept.multiple=FALSE, accept.null=FALSE, accept.na=FALSE)
{
  argnm = call.objname(arg, 1)
  choices = eval(call.default.arg(arg, 1))
  if (!accept.null && is.null(arg)) stop(sprintf("'%s' cannot be NULL", argnm))
  if (!accept.na && is.na(arg)) stop(sprintf("'%s' cannot be NA", argnm))
  if (!is.null(arg) && !is.na(arg))
  {
    if (accept.multiple && length(arg) > 1)
    {
      margs = !(arg %in% choices)
      if (any(margs)) stop(sprintf("invalid arguments in '%s': (%s).\nValid choices are: %s", argnm, strjoin(arg[margs], ', '), strjoin(choices, ', ')) )
    } else if (!all(arg %in% choices)) stop(sprintf("Invalid argument (%s) in '%s'.\nValid choices are: %s", arg, argnm, strjoin(choices, ', ')), call. = F)
  } else return(arg)
  if (!accept.multiple) arg = arg[1]
  arg
}

# Asserts that the argument satisfy some operator (like <, >, <=, >=, etc) based on another parameter
assert.operator <- function(arg, ..., .argname = NA)
{
  objname = if (is.na(.argname)) call.objname(arg, 1) else .argname
  argls = list(...)
  posscalls = list(
    `>` = "greater than %s",
    `<` = "less than %s",
    `>=` = "greater or equal than %s",
    `<=` = "less or equal than %s",
    `==` = "equal to %s",
    `!=` = "different than %s"
  )
  argnms = intersect(names(argls), names(posscalls))
  pretxt = if (length(arg) > 1) "All elements in '%s'" else "'%s'"
  for (fnm in argnms)
  {
    reqop = argls[[fnm]]
    mism = !(do.call(fnm, list(arg, reqop)))
    if (any(mism))
    {
      stop(sprintf(pretxt + ' must be ' + posscalls[[fnm]], objname, reqop), call. = F)
    }
  }
  invisible(T)
}
