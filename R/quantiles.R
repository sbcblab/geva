
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

# Calculates the quantile indexes from the number of quantiles (nq)
calc.quantile.indexes <- function(nq, has.negative=TRUE)
{
  nq = as.integer(abs(nq))
  if (!has.negative) return(1L:nq)
  if (nq == 0L) return(integer(0))
  if (nq == 1L) return(if (has.negative) 0L else 1L)
  iseven = nq %% 2L == 0L
  amaxind = as.integer(floor(nq / 2))
  qinds = (-amaxind):amaxind
  if (iseven) qinds = qinds[-ceiling(length(qinds) / 2)]
  qinds
}

# Calculates the middle value within each pair of numeric values in a vector
calc.inbetweens <- function(x)
{
  if (length(x) <= 1L) return(x)
  return((x[-1L]+x[-length(x)]) / 2)
}

# Calculates the quantile thresholds from values (x) and number of quantiles (nq)
calc.quantile.thresholds <- function(x, nq, init.threshold=NA_real_, has.negative=TRUE)
{
  nq = as.integer(nq)
  if (nq == 0L) return(numeric(0))
  maxval = max(abs(x))
  if (nq == 1L)
  {
    if (has.negative) return(c(-maxval, maxval))
    return(c(0, maxval))
  }
  iseven = nq %% 2L == 0L
  if (is.na(init.threshold))
  {
    init.threshold = maxval / nq
    if (has.negative && iseven) init.threshold = init.threshold * 2
  }
  if (init.threshold > maxval) init.threshold = maxval
  if (has.negative)
  {
    qinds = calc.quantile.indexes(nq, has.negative)
    nqplus = max(qinds)
    if (!iseven) nqplus = nqplus + 1L
    midthres = if (iseven) 0 else NULL
    qthres = seq(from=init.threshold, to=maxval, length.out = nqplus)
    qthres = c((qthres[length(qthres):1L] * -1), midthres, qthres)
    
  } else {
    qthres = c(0, seq(from=init.threshold, to=maxval, length.out = nq))
  }
  attr(qthres, 'init.threshold') = init.threshold
  qthres
}

# Calculates the quantile centroids from values (x) and number of quantiles (nq)
calc.quantile.centroids <- function(x, nq, init.threshold=NA_real_)
{
  nq = as.integer(nq)
  if (nq == 0L) return(numeric(0))
  has.negative = any(x < 0)
  qinds = calc.quantile.indexes(nq, has.negative)
  qthres = calc.quantile.thresholds(x, nq, init.threshold, has.negative)
  if (nq == 1L)
  {
    qcents = if (has.negative) 0.0 else max(qthres) / 2
  } else {
    qcents = calc.inbetweens(qthres)
  }
  qcents = setNames(qcents, as.character(qinds))
  attr(qcents, 'indexes') = qinds
  attr(qcents, 'sizes') = setNames(qthres[-1] - qthres[-length(qthres)], as.character(qinds))
  attr(qcents, 'thresholds') = qthres
  attr(qcents, 'init.threshold') = attr(qthres, 'init.threshold')
  qcents
}

# Calculates the nearest quantiles from the given centroids
calc.quantile.nearest <- function(x, qcents)
{
  qnms = factor(names(qcents), levels=names(qcents))
  qsizes = attr(qcents, 'sizes')
  mdists = t(sapply(x, function(xi) abs((xi - qcents) / qsizes)))
  vmindists = apply(mdists, 1, which.min)
  qnearests = qnms[vmindists]
  qoffs = x - qcents[vmindists]
  attr(qnearests, 'offsets') = qoffs
  qdists = abs(qoffs)
  qscores = 1 - (qdists * 2 / qsizes[vmindists])
  seloverscores = x >= max(qcents) | x <= min(qcents)
  if (any(seloverscores)) qscores[seloverscores] = 1
  attr(qnearests, 'score') = qscores
  qnearests
}

# Generates the quantile colors
generate.quantile.colors <- function(svinds)
{
  sinds = svinds$S
  vinds = svinds$V
  
  szerosel = abs(sinds) == min(abs(sinds))
  
  h = ifelse(sinds > 0, 0, 2 / 3)
  s = 1 / vinds
  v = ifelse(szerosel, 0, 1)
  if (any(szerosel))
  {
    s[szerosel] = 0
    v[szerosel] = 1 - 1 / vinds[szerosel]
  }
  if (any(!szerosel))
  {
    v[!szerosel] = v[!szerosel] / (max(abs(sinds)) / abs(sinds)[!szerosel])
  }
  clrs = setNames(hsv(h, s, v), rownames(svinds))
  clrs
}

# Calculates the quantiles of a SVTable 
#' @export
geva.quantiles <- function(sv, nq.s = 3L, nq.v = 2L, initial.thresholds=c(S=NA_real_, V=NA_real_), ...)
{
  assert.names.equal(sv, colnames=c('S', 'V'))
  assert.dim(initial.thresholds, length=2L)
  nq.s = as.integer(nq.s)
  nq.v = as.integer(nq.v)
  assert.operator(nq.s, `>=` = 3L)
  assert.operator(nq.v, `>=` = 2L)
  vsm = sv[,'S']
  vvr = sv[,'V']
  if (!is.named(initial.thresholds)) names(initial.thresholds) = c('S', 'V')
  thrs = initial.thresholds[['S']]
  thrv = initial.thresholds[['V']]
  vsm.cents = calc.quantile.centroids(vsm, nq.s, thrs)
  vvr.cents = calc.quantile.centroids(vvr, nq.v, thrv)
  if (is.na(thrs)) thrs = attr(vsm.cents, 'init.threshold')
  if (is.na(thrv)) thrv = attr(vvr.cents, 'init.threshold')
  maxs = max(abs(vsm))
  maxv = max(vvr)
  assert.operator(thrs, `<` = maxs, .argname = "'S' initial threshold")
  assert.operator(thrv, `<` = maxv, .argname = "'V' initial threshold")
  
  vsm.qs = calc.quantile.nearest(vsm, vsm.cents)
  vvr.qs = calc.quantile.nearest(vvr, vvr.cents)
  svscores = svtable(attr(vsm.qs, 'score'), attr(vvr.qs, 'score'), rownames(sv))
  vsm.names = names(vsm.cents)
  vvr.names = names(vvr.cents)
  
  vsm.inds = attr(vsm.cents, 'indexes')
  vvr.inds = attr(vvr.cents, 'indexes')
  
  fsvlabel = function(s, v) sprintf("s%sv%s", s, v)
  mqinds = sapply(vsm.inds, function(si) sapply(vvr.inds, function(vi) fsvlabel(si, vi)))
  vlabels = as.vector(mqinds)
  svinds = svtable(S = as.vector(sapply(vsm.inds, rep, times=length(vvr.inds))),
                   V = rep(vvr.inds, length(vsm.inds)),
                   row.names = vlabels)
  
  dfinds = data.frame(S=as.factor(svinds$S), V=as.factor(svinds$V), row.names=rownames(svinds))
  svcents = svtable(vsm.cents[dfinds$S], vvr.cents[dfinds$V], rownames(svinds))
  qcount = svattr(nq.s, nq.v)
  qcutoff = svattr(as.numeric(thrs), as.numeric(thrv))
  qgroups = factor(mqinds[matrix(c(vvr.qs, vsm.qs), ncol=2)], levels = vlabels)
  
  # Score calculated by direct multiplication
  qscores = apply(svscores, 1, prod)
  qoffsets = svtable(attr(vsm.qs, 'offsets'), attr(vvr.qs, 'offsets'), rownames(sv))
  lthres = list(S=attr(vsm.cents, 'thresholds'), V=attr(vvr.cents, 'thresholds'))
  
  # Generating colors
  clrs = generate.quantile.colors(svinds)
  qinfo = list(thresholds = lthres, colors=clrs)
  
  new('GEVAQuantiles',
      grouping=qgroups,
      scores=qscores,
      centroids=svcents,
      offsets=qoffsets,
      info=qinfo,
      svscores=svscores,
      qindexes=svinds,
      qcount=qcount,
      qcutoff=qcutoff)
}



