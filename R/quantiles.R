
##########################
# QUANTILES METHODS
# -----------------------
# 
# Functions to calculate quantiles from summarized data
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

#' @include c_GEVASummary.R
#' @include c_GEVAQuantilesAdjusted.R
#' @include asserts.R
#' @include vectorhelpers.R

# Calculates the quantiles of a SVTable 
geva.quantiles <- function(sv, nq.s = 3L, nq.v = 2L, initial.thresholds=c(S=NA_real_, V=NA_real_), ...)
{
  assert.names.equal(sv, colnames=c('S', 'V'))
  assert.dim(initial.thresholds, length=2L)
  assert.operator(nq.s, `>=` = 3L)
  assert.operator(nq.v, `>=` = 2L)
  vsm = sv[,'S']
  vvr = sv[,'V']
  s.even = nq.s %% 2L == 0L
  v.even = nq.v %% 2L == 0L
  if (!is.named(initial.thresholds)) names(initial.thresholds) = c('S', 'V')
  thrs = initial.thresholds[['S']]
  thrv = initial.thresholds[['V']]
  maxs = max(abs(vsm))
  maxv = max(vvr)
  if (is.na(thrs)) thrs = maxs * 2 / nq.s
  if (is.na(thrv)) thrv = maxs / nq.v
  assert.operator(thrs, `<` = maxs, .argname = "'S' initial threshold")
  assert.operator(thrv, `<` = maxv, .argname = "'V' initial threshold")
  vthr = if (s.even) c(-thrs, thrs) else c(-thrs, 0, thrs)
  halfnq.s = floor(nq.s / 2L)
  vbounds.s = seq(-maxs, -thrs, length.out = halfnq.s)
  vbounds.s = sort(if (s.even) c(vbounds.s, -vbounds.s) else c(vbounds.s, 0, -vbounds.s))
  # TODO: Finish quantiles function
  
}


