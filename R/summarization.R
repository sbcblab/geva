
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
options.summary <- c('median', 'mean')

#' Returns a vector with the supported methods of summarization
#' @export
options.variation <- c('mad', 'sd', 'var')

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

#' Summarizes the GEVAInput
#' @export
geva.summarize <- function(gevainput, summary.method = options.summary, variation.method = options.variation)
{
  assert.class(gevainput, is='GEVAInput')
  summary.method = assert.choices(summary.method)
  variation.method = assert.choices(variation.method)
  if (ncol(ginput) <= 3L && variation.method == 'mad')
  {
    variation.method = setdiff(options.variation, 'mad')[1]
    warning(sprintf("'mad' option for variation method requires at least 4 columns.\nUsing '%s' instead", variation.method))
  }
  mv = inputvalues(gevainput)
  mw = inputweights(gevainput)
  vsumm = rows.weighted.summary(mv, mw, summary.method)
  vvar = rows.weighted.variation(mv, mw, vsumm, variation.method)
  #summf = get.summary.method(summary.method)
  #varf = get.variation.method(variation.method)
  #matinds = as.indexes(gevainput)
  #vv = as.numeric(inputvalues(gevainput))
  #vw = as.numeric(inputweights(gevainput))
  #na.rm = anyNA(vv)
  #vsumm = apply(matinds, 1, function(vinds) summf(vv, vw, idxs = vinds, na.rm = na.rm))
  #vvar = apply(matinds, 1, function(vinds) varf(vv, vw, idxs = vinds, center=vsumm[vinds[1]], na.rm = na.rm))
  dfsv = data.frame(S=vsumm, V=vvar)
  svmets = svattr(summary.method, variation.method)
  infols = list(summary.method=summary.method, variation.method=variation.method)
  vprint("Input summarized")
  new('GEVASummary', sv=dfsv, inputdata=gevainput, sv.method=svmets, info=infols)
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
              sd = matrixStats::weightedSd,
              var = matrixStats::weightedVar,
              matrixStats::weightedVar
  )
  fn
}
