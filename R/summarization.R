
##########################
# SUMMARIZATION METHODS
# -----------------------
# 
# Functions to summarize the processed data for GEVA
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

#' @include c_GEVAInput.R
#' @include c_GEVASummary.R
#' @include asserts.R

#' GEVA summarization
NULL

#' Returns a vector with the supported methods of summarization
#' @export
#' @rdname geva.summarize
options.summary <- c('mean', 'median')

#' Returns a vector with the supported methods of summarization
#' @export
#' @rdname geva.summarize
options.variation <- c('sd', 'var', 'mad')

# Calculates the weighted summary of a matrix
rows.weighted.summary <- function(mv, mw, summary.method = options.summary)
{
  if (missing(mw)) mw = mv^0
  summary.method = assert.choices(summary.method)
  if (ncol(mv) == 1L) return(mv)
  vv = as.numeric(mv)
  vw = as.numeric(mw)
  matinds = as.indexes(mv)
  summf = get.summary.method(summary.method)
  na.rm = anyNA(vv) || anyNA(vw)
  vres = apply(matinds, 1, function(vinds) summf(vv, vw, idxs = vinds, na.rm = na.rm))
  vres
}

# Calculates the weighted variation of a matrix
rows.weighted.variation <- function(mv, mw, centers, variation.method = options.variation)
{
  if (missing(mw)) mw = mv^0
  variation.method = assert.choices(variation.method)
  assert.dim(centers, length=nrow(mv))
  if (ncol(mv) == 1L) return(mv*0)
  vv = as.numeric(mv)
  vw = as.numeric(mw)
  matinds = as.indexes(mv)
  varf = get.variation.method(variation.method)
  na.rm = anyNA(vv) || anyNA(vw)
  vres = apply(matinds, 1, function(vinds) varf(vv, vw, idxs = vinds, center=centers[vinds[1]], na.rm = na.rm))
  vres
}

# Weighted variance implementation
weighted.var <- function(x, w = NULL, idxs = NULL, na.rm = FALSE, center = NULL, ...)
{
  n = length(x)
  if (n == 0L) return(0)
  if (is.null(w))
  {
    w = rep(1, times = n)
  }
  else if (length(w) != n)
  {
    stop ("The number of elements in arguments 'w' and 'x' does not match: ", length(w), " != ", n)
  }
  if (!is.null(idxs))
  {
    x = x[idxs]
    w = w[idxs]
  }
  if (na.rm)
  {
    keep = !(is.na(x) | is.na(w))
    if (any(!keep))
    {
      x = x[keep]
      w = w[keep]
    }
  }
  wsum = sum(w)
  if (wsum == 0)
  {
    w = rep(1, times = n)
    wsum = n
  }
  if (is.null(center))
  {
    center = sum(w * x) / wsum
  }
  sqdx = (x - center)^2
  wvar = sum(w * sqdx) / wsum
  wvar
}

# Weighted standard-deviation implementation
weighted.sd <- function(x, w = NULL, idxs = NULL, na.rm = FALSE, center = NULL, ...)
{
  wvar = weighted.var(x, w, idxs, na.rm, center, ...)
  wsd = sqrt(wvar)
  wsd
}

#' Summarizes the GEVAInput
#' @export
#' @rdname geva.summarize
geva.summarize <- function(ginput, summary.method = options.summary, variation.method = options.variation, ...)
{
  assert.class(ginput, is='GEVAInput')
  summary.method = assert.choices(summary.method)
  variation.method = assert.choices(variation.method)
  if (ncol(ginput) < 2L)
    stop(sprintf("ginput must contain at least two value columns (has %s)", ncol(ginput)))
  if (ncol(ginput) <= 3L && variation.method == 'mad')
  {
    variation.method = setdiff(options.variation, 'mad')[1]
    warning(sprintf("'mad' option for variation method requires at least 4 columns.\nUsing '%s' instead", variation.method))
  }
  mv = inputvalues(ginput)
  mw = inputweights(ginput)
  vsumm = rows.weighted.summary(mv, mw, summary.method)
  vvar = rows.weighted.variation(mv, mw, vsumm, variation.method)
  dfsv = data.frame(S=vsumm, V=vvar)
  svmets = svattr(summary.method, variation.method)
  infols = list(summary.method=summary.method, variation.method=variation.method)
  vprint("Input summarized")
  new('GEVASummary', sv=dfsv, inputdata=ginput, sv.method=svmets, info=infols)
}

# Gets the function for summarization (central point calculation)
get.summary.method.character <- function(method = options.summary)
{
  method = assert.choices(method)
  fn = switch(method,
              median = matrixStats::weightedMedian,
              mean = matrixStats::weightedMean,
              matrixStats::weightedMean
              )
  fn
}

# Gets the function for variation (deviation amount calculation)
get.variation.method.character <- function(method = options.variation)
{
  method = assert.choices(method)
  fn = switch(method,
              mad = matrixStats::weightedMad,
              sd = weighted.sd,
              var = weighted.var,
              weighted.sd
  )
  fn
}
