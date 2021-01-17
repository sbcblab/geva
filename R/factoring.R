
##########################
# FACTORING METHODS
# -----------------------
# 
# Functions to perform factoring (such as weighted ANOVA) considering GEVA inputs and quantiles
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al

#' @include asserts.R
#' @include usecasechecks.R
#' @include summarization.R
#' @include quantiles.R
NULL

# Returns a vector with the supported methods of p-value adjustment for factors analysis
#' @options [geva.finalize]
#' @order 2
options.factoring.p.adjust <- c('partial.quantiles', stats::p.adjust.methods)

# Creates a model.matrix for a row to be used in weighted fit based on specific factors
row.model.matrix <- function(factors)
{
  if (is.logical(factors)) factors = factor(ifelse(factors, '1', '2'))
  factors = as.factor(factors)
  x = model.matrix(~ factors)
  x
}

# Creates a weighted fit (if weights are present) to a numeric row based on specific factors
row.lm.wfit <- function(r, factors, w=NULL, modelmat=NULL, ...)
{
  if (is.null(modelmat))
  {
    assert.dim(factors, length=length(r))
    modelmat = row.model.matrix(factors)
  }
  z = if (is.null(w))
  {
    lm.fit(modelmat, r, ...)
  }
  else
  {
    w[w < 1e-16] = 1e-16
    w = normalize.weights(w, by.min=TRUE)
    lm.wfit(modelmat, r, w, ...)
  }
  z
}

# Calculates the weighted ANOVA of a row based on factors
row.wanova <- function(r, factors, w=NULL, modelmat=NULL, ...)
{
  rlm = row.lm.wfit(r, factors, w, modelmat, ...)
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

# Calculates the silhouette between quantile S indexes and factors
row.factor.sindex.silhouette <- function(vsinds, factors)
{
  lvls = levels(factors)
  n = length(vsinds)
  if (length(lvls) == 1L) return(rep(1, n))
  sil = numeric(n)
  flist = tapply(vsinds, factors, list)
  fcounts = tapply(vsinds, factors, length)
  chfacts = as.character(factors)
  for (i in 1L:n)
  {
    f = levels(factors)[factors[i]]
    nf = fcounts[f]
    if (nf <= 1)
    {
      sil[i] = 0
      next
    }
    vf = flist[[f]]
    xf = vsinds[i]
    cfa = sum(abs(xf - vf)) / (nf - 1)
    cfb = Inf
    for (fj in lvls[lvls != f])
    {
      cfbj = mean(abs(flist[[fj]] - xf))
      if (cfbj < cfb)
        cfb = cfbj
    }
    if (cfa == cfb)
    {
      sil[i] = 0
      next
    }
    sil[i] = (cfb - cfa) / max(c(cfa, cfb))
  }
  sil
}

# Calculates a score for good relationship between S index and factors based on the silhouettes
# Returns a single numeric value from 0 (no similarity) to 1 (total similarity)
row.factor.sindex.similarity.score <- function(vsinds, factors)
{
  n = length(vsinds)
  if (n == 0L) return(0)
  if (all(vsinds %in% vsinds[1L], na.rm = TRUE)) return(0)
  vsil = row.factor.sindex.silhouette(vsinds, factors)
  sil = mean(tapply(vsil, factors, mean))
  sil
}

# Calculates the silhouette score between the factors and the predicted quantiles of the input values
# Returns a numeric vector with scores from -1 (negative similarity) to 0 (no similarity) to 1 (total similarity)
factoring.silhouette.partial.quantiles <- function(gobject, gquants=NULL, factors=NULL, ...)
{
  #assert.operator(penalty, `>=` = 0, `<=` = 1)
  if (is.null(factors)) factors = factors(gobject)
  if (is.null(gquants)) gquants = geva.quantiles(gobject)
  selcols = !is.na(factors)
  factors = factors[selcols]
  qsindexes = sort(unique(qindexes(gquants)$S))
  qsthresholds = sort(infolist(gquants)$thresholds$S)
  mv = inputvalues(gobject)[,selcols,drop=FALSE]
  msinds = t(apply(mv, 1, calc.quantile.nearest.sindex,
                   gquants=gquants, qsthresholds=qsthresholds, qsindexes=qsindexes))
  vsimil = apply(msinds, 1, row.factor.sindex.similarity.score, factors=factors)
  #penalty = 0.5
  #vsimil = (1 - penalty) + penalty * vsimil
  vsimil
}

# Calculates the factor-dependent p-values from a GEVAInput or GEVASummary object (Fisher method)
# Returns a vector of p-values representing factor dependency
factoring.dep.fisher <- function(gobject, factors=NULL, idxs=NULL, ...)
{
  if (is.null(factors)) factors = factors(gobject)
  factors = as.factor(factors)
  nr = nrow(gobject)
  if (is.null(idxs)) idxs = 1L:nr
  else if (is.logical(idxs)) idxs = which(idxs)
  assert.dim(factors, length=ncol(inputdata(gobject)))
  selcols = !is.na(factors)
  factors = factors[selcols]
  mv = inputvalues(gobject)[idxs,selcols,drop=F]
  mv = clamp(mv, ...)
  mw = inputweights(gobject)[idxs,selcols,drop=F]
  vpvals = rep(NA_real_, nr)
  modmat = row.model.matrix(factors)
  vpvals[idxs] = sapply(idxs, function(i) row.wanova(mv[i,], factors, mw[i,], modmat))
  vpvals
}


# Calculates the factor-specific p-values from a GEVAInput or GEVASummary object (Fisher method)
# Returns a matrix of p-values representing factor specificity
factoring.spec.fisher <- function(gobject, factors=NULL, idxs=NULL, ...)
{
  if (is.null(factors)) factors = factors(gobject)
  factors = as.factor(factors)
  ugroup = levels(factors)
  ngroups = length(ugroup)
  #if (!check.factors.are.specific(factors, warn=FALSE, msg=FALSE)) stop("'factors' must contain at least 2 levels for factor-specific analysis")
  nr = nrow(gobject)
  if (is.null(idxs)) idxs = 1L:nr
  else if (is.logical(idxs)) idxs = which(idxs)
  assert.dim(factors, length=ncol(inputdata(gobject)))
  selcols = !is.na(factors)
  factors = factors[selcols]
  mv = inputvalues(gobject)[idxs,selcols,drop=F]
  mw = inputweights(gobject)[idxs,selcols,drop=F]
  mpvals = matrix(NA_real_, nrow=nr, ncol=ngroups, dimnames = list(rownames(mv), ugroup))
  # Use case: 3 or more levels are required to use fisher
  if (ngroups < 3L)
    return(mpvals)
  mv = clamp(mv, ...)
  for (gi in 1L:ngroups)
  {
    gsel = factors == ugroup[gi]
    modelmat = row.model.matrix(gsel)
    vpvals = sapply(idxs, function(i) row.wanova(mv[i,], factors, mw[i,], modelmat))
    mpvals[idxs,gi] = vpvals
  }
  mpvals
}


# Calculates the factor-specific p-values from a GEVAInput or GEVASummary object (Levene method)
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
  assert.dim(factors, length=ncol(inputdata(gobject)))
  selcols = !is.na(factors)
  factors = factors[selcols]
  mv = inputvalues(gobject)[idxs,selcols,drop=F]
  mv = clamp(mv, ...)
  mw = inputweights(gobject)[idxs,selcols,drop=F]
  
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
  prevent.zeroweights <- function(w) if(sum(w) == 0) rep(1, length(w)) else w 
  for (gi in 1L:ngroups)
  {
    gsel = factors == ugroup[gi]
    mw.partial <- mw
    mw.partial[, gsel] = t(apply(mw.partial[, gsel,drop=F], 1, prevent.zeroweights))
    mw.partial[, !gsel] = t(apply(mw.partial[, !gsel,drop=F], 1, prevent.zeroweights))
    meds[,1L] = rows.weighted.summary(mv[,gsel,drop=F], mw.partial[,gsel,drop=F], summary.method)
    meds[,2L] = rows.weighted.summary(mv[,!gsel,drop=F], mw.partial[,!gsel,drop=F], summary.method)
    mvars[,1L] = rows.weighted.variation(mv[,gsel,drop=F], mw.partial[,gsel,drop=F], centers = meds[,1L], variation.method)
    mvars[,2L] = rows.weighted.variation(mv[,!gsel,drop=F], mw.partial[,!gsel,drop=F], centers = meds[,2L], variation.method)
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


# Gets a data.frame with the classification for specific factors from a p-value matrix
factoring.table.spec <- function(mpvals, p.cutoff)
{
  facts = colnames(mpvals)
  dtfac = data.frame(row.names=rownames(mpvals))
  mpvals[is.na(mpvals)] = 1
  mpvals = clamp(mpvals, 0, 1)
  mpsigs = mpvals < p.cutoff
  vspec.count = base::rowSums(mpsigs)
  sel.spec = vspec.count == 1
  ind.min.pval = apply(mpvals, 1, base::which.min)
  dtfac$spec.count = vspec.count
  dtfac$is.spec = sel.spec
  dtfac$min.pval = apply(mpvals, 1, base::min)
  dtfac$min.pval.index = ind.min.pval
  dtfac
}

# Initializes the classification table for factoring analysis
factoring.init.cltable <- function(gq)
{
  rnames = names(scores(gq))
  nprobes = length(rnames)
  cltable = data.frame(row.names = rnames,
                       qindex.summary = rep(NA_integer_, nprobes),
                       qindex.variation = rep(NA_integer_, nprobes),
                       factoring.dep.pval = rep(NA_real_, nprobes),
                       factoring.spec.pval = rep(NA_real_, nprobes))
  cltable$qindex.summary = as.integer(qindexes(gq)$S[groups(gq)])
  cltable$qindex.variation = as.integer(qindexes(gq)$V[groups(gq)])
  cltable
}

# Combines the specific p-value matrices, selecting the most suitable row between both matrices
# Returns data.frame (similar to factoring.table.spec) with the best classification
factoring.combine.spec.classif <- function(mpvals.levene, mpvals.fisher, p.cutoff)
{
  dtfac.levene = factoring.table.spec(mpvals.levene, p.cutoff)
  dtfac.fisher = factoring.table.spec(mpvals.fisher, p.cutoff)
  dtfac = dtfac.levene
  sel.conflicts = dtfac.levene$is.spec & dtfac.fisher$is.spec
  sel.usefisher = !sel.conflicts & dtfac.fisher$is.spec
  mpvals = mpvals.levene
  facts = colnames(mpvals)
  if (any(sel.conflicts))
  {
    sel.usefisher = sel.usefisher | sel.conflicts & (dtfac.fisher$min.pval < dtfac.levene$min.pval)
  }
  if (any(sel.usefisher))
  {
    dtfac[sel.usefisher,] = dtfac.fisher[sel.usefisher,]
    mpvals[sel.usefisher,] = mpvals.fisher[sel.usefisher,]
  }
  nr = nrow(dtfac)
  fspec.name = factor(rep(NA_character_, nr), levels=colnames(mpvals))
  if (any(dtfac$is.spec))
  {
    inds.facts = as.integer(dtfac$min.pval.index[which(dtfac$is.spec)])
    fspec.name[inds.facts] = levels(fspec.name)[inds.facts]
  }
  dtfac$spec.name = fspec.name
  for (fcolnm in facts)
  {
    fnewcolnm = sprintf("spec.pval.levene.%s", fcolnm)
    dtfac[,fnewcolnm] = mpvals.levene[,fcolnm]
  }
  for (fcolnm in facts)
  {
    fnewcolnm = sprintf("spec.pval.fisher.%s", fcolnm)
    dtfac[,fnewcolnm] = mpvals.fisher[,fcolnm]
  }
  dtfac
}

# Adjusts the p-values 
factoring.pval.adjust <- function(pvals, gobject, gq, facts=NULL,
                                  p.val.adjust=options.factoring.p.adjust, ...)
{
  p.val.adjust = match.arg(p.val.adjust)
  if (p.val.adjust == 'none') return(pvals)
  if (is.null(facts))
    facts = as.factor(factors(gobject))
  if (p.val.adjust %in% stats::p.adjust.methods)
    return(stats::p.adjust(pvals, p.val.adjust))
  argls = list(...)
  if (p.val.adjust == 'partial.quantiles')
  {
    vqsil = argls[['partial.quantiles']]
    if (is.null(vqsil))
      vqsil = factoring.silhouette.partial.quantiles(gobject, gquants = gq, factors = facts)
    vqsil = 0.5 + vqsil
    vqsil[vqsil < 1e-16] = 1e-16
    pvals = pvals^vqsil
  }
  return(pvals)
}

# Performs factors analysis if factors are present
# Returns a list where:
# $factable = factoring results
# $cltable = classification results
# $spec.name = name of factor-specific variation results
geva.factoring <- function(gsummary, gq, facts=NULL, cltable=NULL, p.value=0.05,
                           p.val.adjust=options.factoring.p.adjust,
                           constraint.factors=TRUE, ...)
{
  verbose = is.verbose()
  if (is.null(facts))
    facts = as.factor(factors(gsummary))
  if (is.null(cltable))
    cltable = factoring.init.cltable(gq)
  nprobes = nrow(gsummary)
  vspec.name = factor(rep(NA_character_, nprobes))
  levels(vspec.name) = levels(facts)
  min.value = NA_real_
  max.value = NA_real_
  if (constraint.factors)
  {
    vscents = centroids(gq)$S
    if (length(vscents) != 0L)
    {
      vrange = range(vscents)
      min.value = min(vrange)
      max.value = max(vrange) 
    }
  }
  if (verbose)
    catline("Searching for dependent factors...")
  vdep.pvals = factoring.dep.fisher(gsummary, facts,
                                    min.value=min.value, max.value=max.value)
  vdep.pvals = factoring.pval.adjust(vdep.pvals, gsummary, gq, facts, p.val.adjust)
  cltable$factoring.dep.pval = vdep.pvals
  factable = data.frame(row.names = rownames(gsummary),
                        dep.pval = cltable$factoring.dep.pval)
  if (check.factors.are.specific(facts, warn=FALSE, msg=verbose))
  {
    if (verbose)
      catline("Searching for specific factors...")
    mpvals.levene = factoring.spec.levene(gsummary,
                                          min.value=min.value, max.value=max.value)
    mpvals.fisher = factoring.spec.fisher(gsummary,
                                          min.value=min.value, max.value=max.value)
    partquants = NULL
    for (fact in levels(facts))
    {
      pfacts = as.factor(ifelse(fact == facts, '1', '2'))
      if ('partial.quantiles' %in% p.val.adjust)
      {
        partquants = factoring.silhouette.partial.quantiles(gsummary, gquants = gq, factors = pfacts)
      }
      mpvals.levene[,fact] = factoring.pval.adjust(mpvals.levene[,fact], gsummary, gq, pfacts, p.val.adjust,
                                                   partial.quantiles = partquants)
      mpvals.fisher[,fact] = factoring.pval.adjust(mpvals.fisher[,fact], gsummary, gq, pfacts, p.val.adjust,
                                                   partial.quantiles = partquants)
    }
    
    factable.spec = factoring.combine.spec.classif(mpvals.levene, mpvals.fisher, p.value)
    factable = cbind(factable, factable.spec)
    sel.spec = factable.spec$is.spec
    if (any(sel.spec))
    {
      cltable$factoring.spec.pval[sel.spec] = factable.spec$min.pval[sel.spec]
      vspec.name[sel.spec] = levels(facts)[factable.spec$min.pval.index][sel.spec]
    }
  }
  list(
    factable=factable,
    cltable=cltable,
    spec.name=vspec.name
  )
}
