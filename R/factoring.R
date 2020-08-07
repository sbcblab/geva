
##########################
# FACTORING METHODS
# -----------------------
# 
# Functions to perform factoring (such as weighted ANOVA) considering GEVA inputs and quantiles
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

#' @include asserts.R
#' @include usecasechecks.R
#' @include summarization.R

# TODO: Methods for anova(v ~ f, weights=w)
# TODO: Combine anova for input values and quantiles. For quantiles, weights are their score
# 

# Creates a model.matrix for a row to be used in weighted fit based on specific factors
row.model.matrix <- function(factors)
{
  if (is.logical(factors)) factors = factor(ifelse(factors, '1', '2'))
  factors = as.factor(factors)
  x = model.matrix(~ factors)
  x
}

# Creates a weighted fit (if weights are present) to a numeric row based on specific factors
row.lm.wfit <- function(r, factors, w=NULL, modelmat=NULL)
{
  if (is.null(modelmat))
  {
    assert.dim(factors, length=length(r))
    modelmat = row.model.matrix(factors)
  }
  z = if (is.null(w))
  {
    lm.fit(modelmat, r, offset = NULL, singular.ok = T)
  }
  else
  {
    lm.wfit(modelmat, r, w, offset = NULL, singular.ok = T)
  }
  z
}

# Calculates the weighted ANOVA of a row based on factors
row.wanova = function(r, factors, w=NULL, modelmat=NULL)
{
  rlm = row.lm.wfit(r, factors, w, modelmat)
  ssr = sum(rlm$residuals^2)
  mss = sum(rlm$fitted.values^2)
  dfr = rlm$df.residual
  p = rlm$rank
  if (p > 0L)
  {
    p1 = 1L:p
    comp = rlm$effects[p1]
    asgn = rlm$assign[stats:::qr.lm(rlm)$pivot][p1]
    ss = c(vapply(split(comp^2, asgn), sum, 1), ssr)
    df = c(lengths(split(asgn, asgn)), dfr)
  }
  else
  {
    ss = ssr
    df = dfr
  }
  ms = ss/df
  f = ms/(ssr/dfr)
  pval = stats::pf(f, df, dfr, lower.tail = FALSE)[[2]]
  pval
}


# Calculates the factor-dependent p-values from a GEVAInput or GEVASummary object
# Returns a vector of p-values representing factor dependency
factoring.dep.fisher <- function(gobject, factors=NULL, idxs=NULL, ...)
{
  if (is.null(factors)) factors = factors(gobject)
  factors = as.factor(factors)
  nr = nrow(gobject)
  if (is.null(idxs)) idxs = 1L:nr
  else if (is.logical(idxs)) idxs = which(idxs)
  mv = inputvalues(gobject)[idxs,,drop=F]
  mw = inputweights(gobject)[idxs,,drop=F]
  assert.dim(factors, length=ncol(mv))
  
  vpvals = rep(NA_real_, nr)
  modmat = row.model.matrix(factors)
  vpvals[idxs] = sapply(idxs, function(i) row.wanova(mv[i,], factors, mw[i,], modmat))
  vpvals
}

# Calculates the factor-specific p-values from a GEVAInput or GEVASummary object
# Returns a matrix of p-values representing factor specificity
factoring.spec.levene <- function(gobject, factors=NULL, idxs=NULL, summary.method=NA_character_, variation.method=NA_character_, ...)
{
  if (is.null(factors)) factors = factors(gobject)
  factors = as.factor(factors)
  ugroup = levels(factors)
  if (!check.factors.are.specific(factors, warn=FALSE)) stop("'factors' must contain at least 3 levels for factor-specific analysis")
  ngroups = length(ugroup)
  nr = nrow(gobject)
  if (is.null(idxs)) idxs = 1L:nr
  else if (is.logical(idxs)) idxs = which(idxs)
  mv = inputvalues(gobject)[idxs,,drop=F]
  mw = inputweights(gobject)[idxs,,drop=F]
  assert.dim(factors, length=ncol(mv))
  if (is.na(summary.method))
  {
    if (inherits(gobject, 'GEVASummary')) summary.method = sv.method(gobject)$S
    else summary.method = options.summary[1]
  }
  if (is.na(variation.method))
  {
    if (inherits(gobject, 'GEVASummary')) variation.method = sv.method(gobject)$V
    else variation.method = options.variation[1]
  }
  mpvals = matrix(NA_real_, nrow=nr, ncol=ngroups, dimnames = list(rownames(mv), ugroup))
  meds = matrix(NA_real_, nrow=nrow(mv), ncol=2L, dimnames=list(rownames(mv)[idxs], c('1', '2')))
  mvars = matrix(NA_real_, nrow=nrow(mv), ncol=2L, dimnames=list(rownames(mv)[idxs], c('1', '2')))
  for (gi in 1L:ngroups)
  {
    gsel = factors == ugroup[gi]
    meds[,1L] = rows.weighted.summary(mv[,gsel,drop=F], mw[,gsel,drop=F], summary.method)
    meds[,2L] = rows.weighted.summary(mv[,!gsel,drop=F], mw[,!gsel,drop=F], summary.method)
    mvars[,1L] = rows.weighted.variation(mv[,gsel,drop=F], mw[,gsel,drop=F], centers = meds[,1L], variation.method)
    mvars[,2L] = rows.weighted.variation(mv[,!gsel,drop=F], mw[,!gsel,drop=F], centers = meds[,2L], variation.method)
    sel.invars = mvars[,1L] > mvars[,2L]
    mresp = abs(mv - meds[,ifelse(gsel, 1L, 2L)])
    modelmat = row.model.matrix(gsel)
    vpvals = apply(mresp, 1, row.wanova, factors = gsel, modelmat = modelmat)
    # p-values become NA if the variation in current group is greater than the other groups
    # This adjustment prevents selecting specific factors with greater variation
    vpvals[sel.invars] = NA_real_
    mpvals[idxs,gi] = vpvals
  }
  mpvals
}


