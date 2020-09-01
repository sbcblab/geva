
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

# Gets the quantile's label from the given indexes
qindex2label <- function(si, vi) sprintf("s%sv%s", si, vi)

# Calculates the quantile indexes from the number of quantiles (nq)
calc.quantile.indexes <- function(nq, has.negative=TRUE)
{
  nq = as.integer(abs(nq))
  if (nq == 0L) return(integer(0))
  if (!has.negative) return(0L:(nq-1L))
  if (nq == 1L) return(0L) #return(if (has.negative) 0L else 1L)
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

# Gets the initial threshold from proportional area slide
qthreshold.proportional <- function(x, nq.split, maxval=max(abs(x)), ...)
{
  qthres = maxval * nq.split
  qthres
}

# Gets the initial threshold from the value containg the maximum standard-deviation from its k neighbor values
qthreshold.k.max.sd <- function(x, k=16, ...)
{
  mkneigh = k.neighbors(x, k)
  vsd = apply(mkneigh, 1, weighted.sd)
  ind.mid = which.max(vsd)
  qthres = vsd[ind.mid]
  qthres
}

# Gets the initial threshold from the value positioned near the range division
qthreshold.range.slice <- function(x, qslice=0.25, ...)
{
  xabs = abs(x)
  ind.slice = which.slice(xabs, qslice)
  qthres = abs(xabs[ind.slice])
  qthres
}

# Gets the initial threshold from proximal density by the standard deviation of k neighbors
qthreshold.density <- function(x, nq, qslice=0.25, k=16, nq.split=1/nq, ...)
{
  mkneigh = k.neighbors(x, k)
  vdens = apply(mkneigh, 1, weighted.sd)
  mkvars = k.neighbors(vdens, k)
  vdens = apply(mkvars, 1, mean)
  ind.mid.dense = which.slice(vdens, qslice * nq.split)
  qthres = abs(x[ind.mid.dense])
  qthres
}

# Gets the quantile's initial threshold (nearest to zero index)
calc.quantile.initial.threshold <- function(x, nq, qmethod, nq.split=1/nq, ...)
{
  if (qmethod == 'density')
  {
    qthres = qthreshold.density(x, nq, ...)
  }
  else if (qmethod == 'k.max.sd')
  {
    qthres = qthreshold.k.max.sd(x, ...)
  }
  else if (qmethod == 'range.slice')
  {
    qthres = qthreshold.range.slice(x, ...)
  }
  else
  {
    qthres = qthreshold.proportional(x, nq.split, ...)
  }
  qthres
}

# Calculates the quantile thresholds from values (x) and number of quantiles (nq)
calc.quantile.thresholds <- function(x, nq, qmethod, init.threshold=NA_real_, has.negative=TRUE, ...)
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
    # If it's not a custom threshold and the quantiles are even,
    # adapt them to the positive and negative values
    nq.split = if (has.negative && iseven) { 2 / nq } else { 1 / nq }
    init.threshold = calc.quantile.initial.threshold(x, nq, nq.split=nq.split, qmethod=qmethod, maxval=maxval, ...)
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
calc.quantile.centroids <- function(x, nq, qmethod, init.threshold=NA_real_)
{
  nq = as.integer(nq)
  if (nq == 0L) return(numeric(0))
  has.negative = any(x < 0, na.rm = T)
  qinds = calc.quantile.indexes(nq, has.negative)
  qthres = calc.quantile.thresholds(x, nq, qmethod, init.threshold, has.negative)
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
calc.quantile.nearest.vector <- function(x, qcents, qsizes=NULL)
{
  qnms = factor(names(qcents), levels=names(qcents))
  if (is.null(qsizes)) qsizes = attr(qcents, 'sizes')
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

# Calculates the nearest quantiles from the given points, centroids and sizes
calc.quantile.nearest.SVTable <- function(svx, svcents, svsizes)
{
  svx = as.SVTable(svx)
  svcents = as.SVTable(svcents)
  svsizes = as.SVTable(svsizes)
  qnms = rownames(svcents)
  assert.dim(svsizes, dim=dim(svcents))
  assert.names.equal(svcents, rownames=rownames(svsizes))
  gnms = rownames(svx)
  sels = !duplicated(qindexes(gq)$S)
  selv = !duplicated(qindexes(gq)$V)
  sinds = qindexes(gq)$S[sels]
  vinds = qindexes(gq)$V[selv]
  vscents = setNames(svcents$S[sels], sinds)
  vvcents = setNames(svcents$V[selv], vinds)
  vsasz = setNames(svsizes$S[sels], sinds)
  vvasz = setNames(svsizes$V[selv], vinds)
  
  vds = calc.quantile.nearest.vector(svx$S, vscents, vsasz)
  vdv = calc.quantile.nearest.vector(svx$V, vvcents, vvasz)
  
  qassocs = factor(qindex2label(vds, vdv), levels=qnms)
  names(qassocs) = gnms
  qassocs
}

# Gets a table of quantiles default classification table based on quantile indexes
get.quantiles.default.classification <- function(gquants)
{
  assert.class(gquants, inherits='GEVAQuantiles')
  qinds = qindexes(gq)
  qnms = rownames(qinds)
  
  qclasstable = data.frame(row.names=qnms)
  minqss = qnms[abs(qinds$S) == min(abs(qinds$S))]
  selbasal = qinds$V == min(qinds$V)
  qclasstable$basal = ifelse(selbasal, 1L, 0L)
  qclasstable$sparse = ifelse(!selbasal, 1L, 0L)
  qclasstable$consistent = ifelse(selbasal & !(qnms %in% minqss), 1L, 0L)
  qclasstable = as.matrix(qclasstable)
  attr(qclasstable, 'relevance') = setNames(1L:3L, colnames(qclasstable))
  qclasstable
}

# Generates the quantile colors
generate.quantile.colors <- function(svinds)
{
  sinds = svinds$S
  vinds = svinds$V + 1
  
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

#' Returns a vector with the supported methods of quantiles separation
#' @export
#' @rdname geva.quantiles
options.quantiles <- c('proportional', 'density', 'k.max.sd', 'range.slice', 'custom')


#' Calculates the quantiles of a SVTable 
#' 
#' @param sv SVTable
#' @param nq.s number of quantiles in S-axis
#' @param nq.v number of quantiles in V-axis
#' @param quantile.method method to detect the initial quantile thresholds. Ignored if initial.thresholds are set
#' @param initial.thresholds named numeric vector with the threshold used for the first quantile
#' @param comb.score.fn function to merge S and V scores into a single column. prod and mean are some examples
#' 
#' @export
#' @rdname geva.quantiles
geva.quantiles <- function(sv, nq.s = 3L, nq.v = 2L, quantile.method = options.quantiles,
                           initial.thresholds=c(S=NA_real_, V=NA_real_), comb.score.fn = prod, ...)
{
  assert.names.equal(sv, colnames=c('S', 'V'))
  assert.dim(initial.thresholds, length=2L)
  quantile.method = assert.choices(quantile.method)
  nq.s = as.integer(nq.s)
  nq.v = as.integer(nq.v)
  assert.operator(nq.s, `>=` = 3L)
  assert.operator(nq.v, `>=` = 2L)
  vsm = sv[,'S']
  vvr = sv[,'V']
  if (!is.named(initial.thresholds)) names(initial.thresholds) = c('S', 'V')
  thrs = initial.thresholds[['S']]
  thrv = initial.thresholds[['V']]
  if (anyNA(initial.thresholds))
  {
    if (quantile.method == 'custom')
      quantile.method = options.quantiles[1]
  }
  else
  {
    quantile.method = 'custom'
  }
  vsm.cents = calc.quantile.centroids(vsm, nq.s, quantile.method, thrs)
  vvr.cents = calc.quantile.centroids(vvr, nq.v, quantile.method, thrv)
  if (is.na(thrs)) thrs = attr(vsm.cents, 'init.threshold')
  if (is.na(thrv)) thrv = attr(vvr.cents, 'init.threshold')
  maxs = max(abs(vsm))
  maxv = max(vvr)
  assert.operator(thrs, `<` = maxs, .argname = "'S' initial threshold")
  assert.operator(thrv, `<` = maxv, .argname = "'V' initial threshold")
  
  vsm.qs = calc.quantile.nearest.vector(vsm, vsm.cents)
  vvr.qs = calc.quantile.nearest.vector(vvr, vvr.cents)
  svscores = svtable(attr(vsm.qs, 'score'), attr(vvr.qs, 'score'), rownames(sv))
  vsm.names = names(vsm.cents)
  vvr.names = names(vvr.cents)
  
  vsm.inds = attr(vsm.cents, 'indexes')
  vvr.inds = attr(vvr.cents, 'indexes')
  
  mqinds = sapply(vsm.inds, function(si) sapply(vvr.inds, function(vi) qindex2label(si, vi)))
  vlabels = as.vector(mqinds)
  svinds = svtable(S = as.vector(sapply(vsm.inds, rep, times=length(vvr.inds))),
                   V = rep(vvr.inds, length(vsm.inds)),
                   row.names = vlabels)
  
  
  dfinds = data.frame(S=as.factor(svinds$S), V=as.factor(svinds$V), row.names=rownames(svinds))
  svcents = svtable(vsm.cents[dfinds$S], vvr.cents[dfinds$V], rownames(svinds))
  qsizes = svtable(attr(vsm.cents, 'sizes')[dfinds$S], attr(vvr.cents, 'sizes')[dfinds$V], vlabels)
  qcount = svattr(nq.s, nq.v)
  qcutoff = svattr(as.numeric(thrs), as.numeric(thrv))
  qgroups = factor(mqinds[matrix(c(vvr.qs, vsm.qs), ncol=2)], levels = vlabels)
  
  # Score calculated by direct multiplication
  qscores = apply(svscores, 1, comb.score.fn)
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
      qareasizes=qsizes,
      qindexes=svinds,
      qcount=qcount,
      qcutoff=qcutoff,
      quantiles.method=quantile.method)
}



