
##########################
# ASSERTION FUNCTIONS
# -----------------------
# 
# Assertion methods to check arguments inside functions
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

#' @include callhelpers.R
#' @include printhelpers.R


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
    colnames = "Mismatching col.names in '%s': expected [%s], got [%s]"
  )
  assert.callres(arr, posscalls, pastefn = fmt.limit, ...)
  invisible(T)
}

# Asserts that the length of a list or array is not zero. This function accepts multiple arguments
assert.notempty <- function(...)
{
  arglen = ...length()
  if (arglen != 0)
  {
    argnms = call.dots.argnames(...)
    for (i in 1:arglen)
    {
      objname = argnms[i]
      if (length(...elt(i)) == 0) stop(sprintf("'%s' must contain at least one element", objname))
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
    class = "'%s' must belong the %s class",
    is = "'%s' must be a '%s'",
    inherits = "'%s' must inherit the %s class"
  )
  for (fnm in intersect(names(argls), names(posscalls)))
  {
    reqclass = argls[[fnm]]
    if (reqclass == 'ANY') next
    if (!(do.call(fnm, reqclass)[1]))
    {
      stop(sprintf(posscalls[[fnm]], objname, reqclass))
    }
  }
  invisible(T)
}
