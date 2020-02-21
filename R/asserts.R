
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
    if (all.equal(callinput, callres) != TRUE)
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
