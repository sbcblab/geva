
##########################
# VECTOR HELPER FUNCTIONS
# -----------------------
# 
# Helper methods to manipulate vectors
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al


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
    lowerinds = seq_len(lowerlimit)
    upperlimit = as.integer(floor(halflimit))
    if (upperlimit == 0L) return(lowerinds)
    upperinds = seq.int((length - upperlimit + 1), length)
    inds = c(lowerinds, upperinds)
  } else {
    inds = seq_len(length)
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

# Flatten a list putting the underlying lists to the same recursive level
list.flatten <- function(x, overwrite.dups=TRUE, ignore.class=character(0), recursive=TRUE)
{
  n = length(x)
  if (n == 0L) return(x)
  assert.class(x, typeof='list')
  nms = names(x)
  hasnms = !is.null(nms)
  sel.lists = vapply(x, is.list, FALSE)
  if (!any(sel.lists)) return(x)
  sel.ignored = vapply(x, function(it) any(vapply(ignore.class, is, FALSE, object=it)), FALSE)
  if (all(sel.ignored)) return(x)
  resls = list()
  lsit.inds = which(sel.lists & !sel.ignored)
  j = 0L
  for (i in 1L:n)
  {
    j = j + 1L
    if (!(i %in% lsit.inds))
    {
      resls[[(if (hasnms) nms[i] else j)]] = x[[i]]
      next
    }
    subls = x[[i]]
    if (recursive)
      subls = list.flatten(subls, overwrite.dups, ignore.class, recursive)
    ns = length(subls)
    if (ns == 0L)
    {
      j = j - 1L
      next
    }
    subls.nms = names(subls)
    if (ns == 1L && is.null(subls.nms))
    {
      resls[[(if (hasnms) nms[i] else j)]] = x[[1L]]
      next
    }
    if (is.null(subls.nms))
      subls.nms = seq(j, length.out = ns)
    else if (!overwrite.dups)
      subls.nms = setdiff(make.unique(c(names(x), subls.nms)), names(x))
    nold = length(resls)
    resls[subls.nms] = subls
    j = j + (length(resls) - nold) - 1L
  }
  resls
}



# Applies the tapply function using a function that returns a vector of the same length of its arguments. The return vector has the same order and size from the X argument
gtapply <- function(X, INDEX, FUN, ...)
{
  len = length(X)
  if (len == 0L) return(X)
  vinds = unlist(tapply(1L:len, INDEX, function(i) i, simplify = FALSE), use.names = FALSE)
  vorder = order(vinds)
  vres = unlist(tapply(X, INDEX, FUN, ..., simplify = FALSE), use.names=FALSE)
  vres = vres[vorder]
  vres
}


# Keeps all values in a vector or matrix within a minimum and maximum value
clamp <- function(x, min.value=NA_real_, max.value=NA_real_, ...)
{
  assert.dim(min.value, length=1L)
  assert.dim(max.value, length=1L)
  has.min = !is.na(min.value) && is.numeric(min.value)
  has.max = !is.na(max.value) && is.numeric(max.value)
  if (!has.min && !has.max)
    return(x)
  if (has.min && has.max && min.value > max.value)
  {
    tmp = max.value
    max.value = min.value
    min.value = tmp
  }
  if (has.min)
  {
    x[x < min.value] = min.value
  }
  if (has.max)
  {
    x[x > max.value] = max.value
  }
  x
}

# Gets a matrix containing the k neighbors of the numeric vector x
k.neighbors <- function(x, k)
{
  assert.class(x, is='vector')
  assert.class(x, is='numeric')
  k = as.integer(k)
  n = length(x)
  if (k >= length(x))
    k = n - 1L
  if (is.integer(x) && is.null(names(x)))
    names(x) = as.character(x)
  if (k <= 0) return(matrix(numeric(0), nrow=n, ncol=0, dimnames = list(names(x), NULL)))
  koffsets = as.integer(ceiling(seq_len(k) / 2) * ifelse(seq_len(k) %% 2 == 1, 1, -1))
  mneigh = matrix(NA_real_, nrow=n, ncol=k, dimnames = list(names(x), as.character(koffsets)))
  ind.order = order(x)
  vx = x[ind.order]
  kh.ceil = ceiling(k / 2)
  kh.floor = floor(k / 2)
  for (ki in seq_len(k))
  {
    ind.offs = seq_len(n) + koffsets[ki]
    ind.offs = ind.offs +
      ifelse(ind.offs <= 0, k + 1L, 0)
    ind.offs = ind.offs +
      (k + 1L) * ifelse(ind.offs > n, -1, 0)
    mneigh[ind.order,ki] = vx[ind.offs]
  }
  mneigh
}

# Replaces all NA values with a replacement value. If the replacement is NULL, removes all NA's
na.replace <- function(x, replace=NULL)
{
  if (!anyNA(x)) return(x)
  if (is.null(replace)) return(x[!is.na(x),drop=FALSE])
  x[is.na(x)] = replace
  x
}
