
##########################
# FINALIZE METHODS
# -----------------------
# 
# Functions to join the obtained information in GEVA and finalize the analysis
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

#' @include scoremerge.R
#' @include factoring.R

# Merges the obtained information and applies the final steps to produce the classification results for the data points
#' @export
geva.finalize <- function(gsummary, ..., p.value=0.05, constraint.factors=TRUE)
{
  assert.class(gsummary, inherits='GEVASummary')
  nprobes = nrow(gsummary)
  cltable = data.frame(row.names = rownames(gsummary),
                       qindex.summary = rep(NA_integer_, nprobes),
                       qindex.variation = rep(NA_integer_, nprobes),
                       factoring.dep.pval = rep(NA_real_, nprobes),
                       factoring.spec.pval = rep(NA_real_, nprobes))
  factable = data.frame(row.names = rownames(gsummary))
  # Merges the quantiles and clustering results by score
  catline("Merging scores...")
  gq = quantiles.scores.merge(gsummary, ...)
  
  cltable$qindex.summary = as.integer(qindexes(gq)$S[groups(gq)])
  cltable$qindex.variation = as.integer(qindexes(gq)$V[groups(gq)])
  facts = as.factor(factors(gsummary))
  vspec.name = rep(NA_character_, nprobes)
  vspec.pvals = rep(NA_real_, nprobes)
  if (check.suitable.factors(facts))
  {
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
    catline("Searching for dependent factors...")
    cltable$factoring.dep.pval = factoring.dep.fisher(gsummary, factors(gsummary),
                                                      min.value=min.value, max.value=max.value)
    factable$dep.pval = cltable$factoring.dep.pval
    if (check.factors.are.specific(facts, warn = FALSE))
    {
      mpvals.levene = factoring.spec.levene(gsummary,
                                            min.value=min.value, max.value=max.value)
      mpvals.fisher = factoring.spec.fisher(gsummary,
                                            min.value=min.value, max.value=max.value)
      factable.spec = factoring.combine.spec.classif(mpvals.levene, mpvals.fisher)
      factable = cbind(factable, factable.spec)
      sel.spec = factable.spec$is.spec
      if (any(sel.spec))
      {
        vspec.pvals[sel.spec] = factable.spec$min.pval[sel.spec]
        vspec.name[sel.spec] = factable.spec$spec.name[sel.spec]
      }
      # 
      # mpsigs = mpvals < p.value
      # mpsigs[is.na(mpsigs)] = FALSE
      # vspecount = base::rowSums(mpsigs)
      # selspecs = vspecount == 1
      # has.specinds = any(selspecs)
      # for (fcolnm in colnames(mpvals))
      # {
      #   fnewcolnm = sprintf("spec.pval.%s", fcolnm)
      #   factable[,fnewcolnm] = mpvals[,fcolnm]
      #   if (has.specinds)
      #   {
      #     selcurrspecs = mpsigs[,fcolnm] & selspecs
      #     if (any(selcurrspecs))
      #     {
      #       vspec.name[selcurrspecs] = fcolnm
      #       vspec.pvals[selcurrspecs] = mpvals[selcurrspecs, fcolnm]
      #       
      #     }
      #   }
      # }
      # factable$spec.count = vspecount
      # factable$spec.name = vspec.name
      cltable$factoring.spec.pval = vspec.pvals
    }
  }
  restable = results.table.from.classif(gq, cltable, p.value)
  sel.spec = restable$classification %in% 'factor-specific'
  vspec.name[!sel.spec] = NA_character_
  restable$specific.factor = vspec.name
  new('GEVAResults',
      resultstable = restable,
      svdata = gsummary,
      quantdata = gq,
      factoring = factable,
      classiftable = cltable)
}

# Builds the results table based on the data quantiles and the classification table
results.table.from.classif <- function(gq, cltable, p.value=0.05)
{
  assert.dim(gq, nrow=nrow(cltable))
  restable = data.frame(row.names=rownames(cltable))
  nprobes = nrow(cltable)
  vcat = rep("", nprobes)
  mclassif = get.quantiles.default.classification(gq)
  vquantnms = rownames(mclassif)
  sel.basal = groups(gq) %in% vquantnms[mclassif[,'basal'] == 1]
  sel.sparse = groups(gq) %in% vquantnms[mclassif[,'sparse'] == 1]
  sel.consistent = groups(gq) %in% vquantnms[mclassif[,'consistent'] == 1]
  classification = rep(NA_character_, nprobes)
  classification[sel.basal] = 'basal'
  classification[sel.sparse] = 'sparse'
  classification[sel.consistent] = 'consistent'
  vdep.pvals = cltable$factoring.dep.pval
  if (any(is.na(vdep.pvals)))
    vdep.pvals[is.na(vdep.pvals)] = 1
  sel.factor.dep = sel.sparse & (vdep.pvals < p.value)
  classification[sel.factor.dep] = 'factor-dependent'
  vspec.pvals = cltable$factoring.spec.pval
  if (any(is.na(vspec.pvals)))
    vspec.pvals[is.na(vspec.pvals)] = 1
  sel.factor.spec = sel.sparse & !sel.factor.dep & (vspec.pvals < p.value)
  classification[sel.factor.spec] = 'factor-specific'
  restable$classification = classification
  restable$specific.factor = rep(NA_character_, nprobes)
  restable$specific.factor[sel.factor.spec] = '<unitialized>'
  restable
}