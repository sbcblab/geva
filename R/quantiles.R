
##########################
# QUANTILES METHODS
# -----------------------
# 
# Functions to calculate quantiles from summarized data
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al

#' @include c_GEVASummary.R
#' @include c_GEVAQuantilesAdjusted.R
#' @include asserts.R
#' @include vectorhelpers.R
NULL

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
  qthres = switch (qmethod,
    density = qthreshold.density(x, nq, ...),
    k.max.sd = qthreshold.k.max.sd(x, ...),
    range.slice = qthreshold.range.slice(x, ...),
    proportional =,
    qthreshold.proportional(x, nq.split, ...)
  )
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
  has.negative = any(x < 0, na.rm = TRUE)
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
  mdists = t(vapply(x, function(xi) abs((xi - qcents) / qsizes), numeric(length(qcents))))
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
calc.quantile.nearest.SVTable <- function(svx, gq)
{
  svx = as.SVTable(svx)
  svcents = centroids(gq) #as.SVTable(svcents)
  svsizes = qareasizes(gq) #as.SVTable(svsizes)
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

# Calculates the nearest quantile index for Summary axis of one or more numeric values
calc.quantile.nearest.sindex <- function(vs, gquants, qsthresholds=NULL, qsindexes=NULL)
{
  if (length(vs) == 0L) return(numeric(0L))
  if (is.null(qsthresholds)) qsthresholds = sort(infolist(gquants)$thresholds$S)
  if (is.null(qsindexes)) qsindexes = sort(unique(qindexes(gquants)$S))
  vres = vs * NA_integer_
  mode(vres) = 'integer'
  for (j in 1L:length(vs))
  {
    vj = vs[j]
    for (i in 1L:length(qsindexes))
    {
      vres[j] = qsindexes[i]
      if (vj < qsthresholds[i+1L])
        break
    }
  }
  vres
}

# Gets a table of quantiles default classification table based on quantile indexes
get.quantiles.default.classification <- function(gquants)
{
  assert.class(gquants, inherits='GEVAQuantiles')
  qinds = qindexes(gquants)
  qnms = rownames(qinds)
  
  qclasstable = data.frame(row.names=qnms)
  minqss = qnms[abs(qinds$S) == min(abs(qinds$S))]
  selbasal = qinds$V == min(qinds$V)
  qclasstable$basal = ifelse(selbasal, 1L, 0L)
  qclasstable$sparse = ifelse(!selbasal, 1L, 0L)
  qclasstable$similar = ifelse(selbasal & !(qnms %in% minqss), 1L, 0L)
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

# Returns a vector with the supported methods of quantiles separation
#' @options [geva.quantiles]
#' @order 2
#' @seealso [geva.cluster]
options.quantiles <- c('range.slice', 'proportional', 'density', 'k.max.sd', 'custom')


#' @title GEVA Quantiles Detection
#' 
#' @description Calculates the quantiles of a [`SVTable-class`].
#' 
#' @param sv a [`SVTable-class`] object (usually [`GEVASummary-class`])
#' @param quantile.method `character`, method to detect the initial quantile thresholds. Ignored if `initial.thresholds` is specified with no `NA` elements
#' @param initial.thresholds named `numeric` vector with the threshold that delimits the initial quantile
#' @param nq.s `integer`, number of quantiles in S-axis (experimental, see `Note')
#' @param nq.v `integer`, number of quantiles in V-axis (experimental, see `Note')
#' @param comb.score.fn `function` applied to merge `S` and `V` score columns into a single column. The function must require only one argument of `numeric vector` type and return a single `numeric` value. Examples include `prod` or `mean`
#' @param ... additional arguments include:
#' \itemize{
#' \item{`qslice` : `numeric` (`0` to `1`), the axis fraction used by `"range.slice"` and `"density"` methods (see 'Details'). Default is `0.25`}
#' \item{`k` : `integer`, neighbor points used by `"density"` and `"k.max.sd"` methods (see 'Details'). Default is `16`}
#' \item{`verbose` : `logical`, whether to print the current progress. Default is `TRUE`}
#' }
#' 
#' @details
#' The `quantile.method` defines how the initial quantile (usually the one at the bottom center) is calculated. Each method has a specific way to estimate the first spatial delimiter, as described below: 
#' \describe{
#' \item{`"range.slice"` (default)}{Separation is set at the nearest point to a fraction of the spatial range. This fraction can be specified by the `qslice` optional argument (`numeric`, default is 0.25, or 25%);}
#' \item{`"density"`}{Separation is set at the point with the most proportional density by *k* neighbor points to its current spatial fraction. This method uses the optional arguments `qslice` (`numeric`, default is 0.25, or 25%) for the desired spatial fraction, and `k` (`numeric`, default is `16`) for the number of neighbor points;}
#' \item{`"k.max.sd"`}{Separation is set at the point with the greatest standard deviation of distance to its *k* neighbor points. The number of neighbor points can be specified by the `k` optional argument (`numeric`, default is `16`);}
#' \item{`"proportional"`}{Separation is set at the exact axis division so that all quantiles have the size;}
#' \item{`"custom"`}{Uses the values specified in the `initial.thresholds` argument.}
#' }
#' 
#' A custom initial separation point can be specified in the `initial.thresholds` as a `numeric` vector of two elements, where the first element refers to `S` axis and the second, to `V` axis. If one of the elements is `NA`, the initial quantile is calculated for that axis only. If both values are not `NA`, the quantile separation method is ignored and automatically set to `"custom"`.
#' 
#' The `nq.s` and `nq.v` arguments determine the number of quantiles for the `S` and `V` axes, respectively. These parameters can be used to increase the number of possible partitions in the SV space, but their applicability is currently being tested (see `Note').
#' 
#' The `comb.score.fn` is a function applied to the partial scores for each SV point to combine them into a single value. The result value is defined as the "quantile score" for a SV point. The function is applied iteratively to two-element `numeric` vectors.
#' 
#' @note
#' Customizing the number of quantiles by `nq.s` and `nq.v` is a **experimental feature** and the remaining analysis steps are mostly based on the default parameters for these arguments. Tests are being conducted to determine this feature's applicability for the next releases.
#' 
#' @examples 
#' ## Quantile detection from a randomly generated input 
#' 
#' # Preparing the data
#' ginput <- geva.ideal.example()      # Generates a random input example
#' gsummary <- geva.summarize(ginput)  # Summarizes with the default parameters
#' 
#' # Default usage
#' gquants <- geva.quantiles(gsummary) # Detects the quantiles
#' plot(gquants)                       # Plots the quantiles
#' 
#' # Custom initial delimiters
#' gquants <- geva.quantiles(gsummary,
#'                           initial.thresholds = c(S=1.00, V=0.5))
#' plot(gquants)                       # Plots the quantiles
#' 
#' # Quantile detection using densities
#' gquants <- geva.quantiles(gsummary, quantile.method = 'density')
#' plot(gquants)                       # Plots the quantiles
#' 
#' @export
#' @family geva.cluster
#' @rdname geva.quantiles
#' @order 1
geva.quantiles <- function(sv, quantile.method = options.quantiles,
                           initial.thresholds=c(S=NA_real_, V=NA_real_), nq.s = 3L, nq.v = 2L, comb.score.fn = prod, ...)
{
  if (is(sv, 'GEVAInput')) sv = geva.summarize(sv)
  assert.names.equal(sv, colnames=c('S', 'V'))
  assert.class(initial.thresholds, is='numeric')
  if (length(initial.thresholds) == 1L && is.named(initial.thresholds) &&
      any(names(initial.thresholds) %in% c('S', 'V')))
  {
    initial.thresholds = setNames(initial.thresholds[c('S', 'V')], c('S', 'V'))
  }
    
  assert.dim(initial.thresholds, length=2L)
  if (is.character(comb.score.fn))
  {
    comb.score.fn = eval(parse(text=comb.score.fn))
  }
  if (!is.function(comb.score.fn))
    stop("'comb.score.fn' must be a function or a valid function name with a single numeric vector as argument")
  quantile.method = match.arg(quantile.method)
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
  
  mqinds = vapply(vsm.inds, function(si) vapply(vvr.inds, function(vi) qindex2label(si, vi), ''), character(nq.v))
  vlabels = as.vector(mqinds)
  svinds = svtable(S = as.vector(vapply(vsm.inds, rep, integer(nq.v), times=length(vvr.inds))),
                   V = rep(vvr.inds, length(vsm.inds)),
                   row.names = vlabels)
  
  
  dfinds = data.frame(S=as.factor(svinds$S), V=as.factor(svinds$V), row.names=rownames(svinds))
  svcents = svtable(vsm.cents[dfinds$S], vvr.cents[dfinds$V], rownames(svinds))
  qsizes = svtable(attr(vsm.cents, 'sizes')[dfinds$S], attr(vvr.cents, 'sizes')[dfinds$V], vlabels)
  qcount = svattr(nq.s, nq.v)
  qcutoff = svattr(as.numeric(thrs), as.numeric(thrv))
  qgroups = factor(mqinds[matrix(c(as.integer(vvr.qs), as.integer(vsm.qs)), ncol=2)], levels = vlabels)
  
  # Score calculated by direct multiplication
  qscores = apply(svscores, 1, comb.score.fn)
  qoffsets = svtable(attr(vsm.qs, 'offsets'), attr(vvr.qs, 'offsets'), rownames(sv))
  lthres = list(S=attr(vsm.cents, 'thresholds'), V=attr(vvr.cents, 'thresholds'))
  
  # Generating colors
  clrs = generate.quantile.colors(svinds)
  qinfo = list(thresholds = lthres,
               colors=clrs,
               analysis.params=list(nq.s=nq.s,
                                    nq.v=nq.v,
                                    quantile.method=quantile.method,
                                    initial.thresholds=initial.thresholds,
                                    comb.score.fn=deparse(substitute(comb.score.fn)))
               )
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
      qmethod=quantile.method)
}



