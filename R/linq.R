
##########################
# LINQ-STYLE HELPER FUNCTIONS
# -----------------------
# 
# Helper methods to manipulate collections in a C#'s linq-style
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

#' @include funhelpers.R
#' @include generics.R

# Gets the first element that satisfies a function, or a default value if no such item was found
# The default value is lazily evaluated
first.default <- function(x, fn, ..., default=NULL)
{
  if (missing(fn)) stop("must specify 'fn' as a function with one parameter or a formula")
  if (length(x) == 0) return(eval.parent(default))
  if (is.function(fn)) insert.function.optargs(fn, ...)
  else if (is(fn, 'formula')) fn = formula2function(fn, ...)
  for (elem in x)
  {
    sresp = fn(elem)
    if (!(is.logical(sresp) || is.null(sresp))) stop("test function must return a logical value")
    if (any(sresp, na.rm = T)) return(elem)
  }
  eval.parent(default, n = 2L)
}

first.formula <- function(formula, ..., default=NULL)
{
  if (length(formula) < 3L) stop("the provided formula needs a expression body in both sides from '~' operator")
  x = eval.parent(formula[[2]])
  fn = formula2function(formula, ...)
  defexpr = substitute(default)
  eval.parent(first.default(x, fn, default=defexpr))
}

# Gets all elements in a collection that satisfy a function or formula
where.default <- function(x, fn, ...)
{
  if (missing(fn)) stop("must specify 'fn' as a function with one parameter or a formula")
  if (length(x) == 0) return(x[NULL])
  if (is.function(fn)) insert.function.optargs(fn, ...)
  else if (is(fn, 'formula')) fn = formula2function(fn, ...)
  vsel = rep(F, length(x))
  i = 0L
  for (elem in x)
  {
    i = i + 1L
    sresp = fn(elem)
    if (!(is.logical(sresp) || is.null(sresp))) stop("test function must return a logical value")
    vsel[i] = any(sresp, na.rm = T)
  }
  x[vsel]
}

where.formula <- function(formula, ...)
{
  if (length(formula) < 3L) stop("the provided formula needs a expression body in both sides from '~' operator")
  x = eval.parent(formula[[2]])
  fn = formula2function(formula, ...)
  where.default(x, fn)
}

# Gets the distinct values in a collection based on the result values from a function
distinct.default <- function(x, fn, ..., incomparables=FALSE)
{
  if (missing(fn)) fn = function(e) e
  if (length(x) == 0) return(x[NULL])
  if (is.function(fn)) insert.function.optargs(fn, ...)
  else if (is(fn, 'formula')) fn = formula2function(fn, ...)
  else stop("'fn' parameter must be a formula or a function with one parameter")
  vresp = as.vector(sapply(x, fn))
  lresp = x[!duplicated(vresp)]
  lresp
}

distinct.formula <- function(formula, ..., incomparables=FALSE)
{
  if (length(formula) < 3L) stop("the provided formula needs a expression body in both sides from '~' operator")
  x = eval.parent(formula[[2]])
  fn = formula2function(formula, ...)
  distinct.default(x, fn, incomparables=incomparables)
}

# Gets the ordered index from a vector by value occurrences. Values not included in the list are placed after
orderby.occur <- function(x, occurs, descending=FALSE)
{
  ind.order = rep(0L, length(x))
  i = 1L
  j = if(descending) 1L else -1L
  for (occur in occurs)
  {
    ind.order[match(occur, x)] = i * j
    i = i + 1L
  }
  return(order(ind.order, decreasing = descending))
}
sortby.occur <- function(x, occurs, descending=FALSE) x[orderby.occur(x, occurs, descending)]
