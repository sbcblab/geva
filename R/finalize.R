
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
geva.finalize <- function(gsummary, ..., p.value=0.05, p.val.adjust=options.factoring.p.adjust,
                          constraint.factors=TRUE)
{
  verbose = is.verbose()
  assert.class(gsummary, inherits='GEVASummary')
  assert.operator(p.value, `>=` = 0.00, `<=` = 1.00)
  p.val.adjust = match.arg(p.val.adjust)
  constraint.factors = assert.class(constraint.factors, class='logical')
  nprobes = nrow(gsummary)
  factable = data.frame(row.names = rownames(gsummary))
  an.pars = list(p.value=p.value,
                 p.val.adjust=p.val.adjust,
                 constraint.factors=constraint.factors)
  # Merges the quantiles and clustering results by score
  if (verbose)
    catline("Merging scores...")
  lsqmerge = quantiles.scores.merge(gsummary, ...)
  gsummary = lsqmerge$gsummary
  gq = lsqmerge$gquant.adj
  cltable = factoring.init.cltable(gq)
  facts = as.factor(factors(gsummary))
  vspec.name = factor(rep(NA_character_, nprobes))
  if (check.suitable.factors(facts, warn=FALSE, msg=verbose))
  {
    factls = geva.factoring(gsummary, gq,
                            facts=facts,
                            cltable=cltable,
                            p.value=p.value,
                            p.val.adjust=p.val.adjust,
                            constraint.factors=constraint.factors)
    cltable = factls$cltable
    factable = factls$factable
    vspec.name = factls$spec.name
  }
  restable = results.table.from.classif(gq, cltable, p.value)
  sel.spec = restable$classification %in% 'factor-specific'
  vspec.name[!sel.spec] = NA
  restable$specific.factor = vspec.name
  infols = list(analysis.params=an.pars)
  gres = new('GEVAResults',
              resultstable = restable,
              svdata = gsummary,
              quantdata = gq,
              factoring = factable,
              classiftable = cltable,
              info=infols)
  if (verbose)
  {
    topgs = top.genes(gres)$classification
    if (length(topgs) == 0)
      catline("No significant results found")
    else
    {
      catline("Found %d significant genes:", length(topgs))
      topcls = unique(topgs)
      for (cl in topcls)
        catline("%s: %d", cl, sum(topgs == cl, na.rm = TRUE))  
    }
  }
  gres 
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
  sel.factor.spec = sel.sparse & ((vdep.pvals - vspec.pvals) < 0) & (vspec.pvals < p.value)
  classification[sel.factor.spec] = 'factor-specific'
  restable$classification = classification
  restable$specific.factor = rep(NA_character_, nprobes)
  restable$specific.factor[sel.factor.spec] = '<unitialized>'
  restable
}

# Performs a quick analysis from a GEVAInput, GEVASummary o GEVAResults.
# Optional (ellipsis) parameters are applied in intermediate steps.
# If the object is a GEVASummary or GEVAResults, the parameters used to obtain these objects are taken into account
#' @export
geva.quick <- function(gobject, ...)
{
  assert.class(gobject, inherits=findMethodSignatures(inputdata)[,1])
  ginput = inputdata(gobject)
  if (!inherits(gobject, 'GEVAInput'))
  {
    expr = as.expression(sv.data(gobject), ginput=ginput, grouped.return=TRUE, ...)
    gsummary = eval(expr)
    expr = as.expression(quantiles(gobject), sv=gsummary, ...)
    gquants = eval(expr)
    gres = geva.finalize(gsummary, gquants=gquants, ...)
  }
  else
  {
    gsummary = geva.summarize(ginput, ...)
    gquants = geva.quantiles(gsummary, ...)
    gclust = geva.cluster(gsummary, ...)
    gres = geva.finalize(gsummary, gquants=gquants, gclust=gclust, ...)
  }
  gres
}
