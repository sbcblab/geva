
##########################
# VECTOR HELPER FUNCTIONS
# -----------------------
# 
# Helper methods to manipulate vectors
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

# Gets the initial and final indexes within a limit
indexes.headtail <- function(length, n=6L, start=1L, ...)
{
  length = as.integer(length)
  if (is.na(length)) stop("Invalid 'length' argument")
  if (length <= 0) return(integer(0))
  if (n < length)
  {
    halflimit = n / 2
    lowerlimit = as.integer(ceiling(halflimit))
    lowerinds = 1:lowerlimit
    upperlimit = as.integer(floor(halflimit))
    if (upperlimit == 0L) return(lowerinds)
    upperinds = (length - upperlimit + 1):length
    inds = c(lowerinds, upperinds)
  } else {
    inds = 1:length
  }
  inds = inds + (start - 1)
  inds
}

# Methods to get both head and tail from arrays and data.frames
headtail <- function(x, ...) UseMethod('headtail')

headtail.default <- function(x, n = 6L, ...)
{
  stopifnot(length(n) == 1L)
  inds = indexes.headtail(length(x), n, ...)
  x[inds]
}

headtail.matrix <- function(x, n = 6L, ...)
{
  stopifnot(length(n) == 1L)
  inds = indexes.headtail(nrow(x), n, ...)
  x[inds, , drop = FALSE]
}

headtail.data.frame <- function(x, n = 6L, ...) headtail.matrix(x, n, ...)

# Checks if a vector is a named vector
is.named <- function(x) !is.null(names(x))

# Gets the first element that satisfies a function, or a default value if no such item was found
first <- function(x, fn, default=NULL)
{
  if (missing(fn) && is(x, 'formula'))
  {
    fvars = all.vars(x)[1:2]
    if (is.na(fvars[2])) stop("must specify at least one variable for collection and one for element selector")
    fbody = deparse(x[[3]])
    x = eval.parent(x[[2]])
    fn = as.formula(sprintf('%s ~ %s', fvars[2], fbody))
  }
  if (length(x) == 0) return(default)
  if (is(fn, 'formula'))
  {
    fvar = all.vars(fn)[1]
    fbody = deparse(fn[[3]])
    fn = eval(parse(text=sprintf('function(%s){%s}', fvar, fbody)))
  }
  for (elem in x)
  {
    if (any(fn(elem))) return(elem)
  }
  default
}

# Merges multiple lists, assuring that the final list will contain unique names
list.merge <- function(...)
{
  if (...length() == 0L) return(list())
  argls = list(...)
  fls = list()
  for (arg in argls)
  {
    arglen = length(arg)
    if (arglen == 0) next
    argnms = names(arg)
    if (!is.named(arg))
    {
      argnms = tail(make.unique(c(names(fls), rep('X', arglen)), sep = ''), n = arglen)
    }
    validinds = which(!(argnms %in% names(fls)))
    for (vind in validinds)
    {
      vnm = argnms[vind]
      fls[[vnm]] = arg[[vind]]
    }
  }
  fls
}
