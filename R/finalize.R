
##########################
# FINALIZE METHODS
# -----------------------
# 
# Functions to join the obtained information in GEVA and finalize the analysis
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al

#' @include scoremerge.R
#' @include factoring.R
NULL

#' @title Concatenating GEVA calculations into the final results
#' 
#' @description Merges the obtained information (Summarization, Clustering, and Quantiles), then applies the final steps to produce the classification results for the SV points (genes).
#' 
#' @param gsummary a [`GEVASummary-class`] object
#' @param ... Intermediate results produced from the `gsummary` object, such as clusters ([`GEVACluster-class`]), quantiles ([`GEVAQuantiles-class`]), or any other object inherited from [`GEVAGroupSet-class`]
#' @param p.value `numeric` (0 to 1), p-value cutoff used in the ANOVA procedures (factor analysis only)
#' @param p.val.adjust `character`, p-value correction method (factor analysis only). Possible values are: `r paste(paste0('"', options.factoring.p.adjust, '"'), collapse=', ')`
#' @param constraint.factors `logical`. If `TRUE`, the S values are restricted to the range within the quantile centroids (factor analysis only)
#' 
#' @return A [`GEVAResults-class`] object, containing the entire set of results. The relevant genes can be retrieved using [`top.genes()`]
#'
#' @details In this procedure, the SV points (*i.e.*, each row in the `GEVASummary` object) are classified according to the detected quantiles (see \link{geva.quantiles}), whose results can be adjusted using other grouping analysis results such as clusters (see \link{geva.cluster}). To achieve the best statistical accuracy, both `GEVAQuantiles` and `GEVACluster` objects must be given in the `...` as optional arguments. If a `GEVAQuantiles` argument is not present, it is automatically calculated using the default parameters.
#' 
#' If multiple factors are present in the `GEVASummary` object (retrieved by `factors(gsummary)`), a factor analysis is also performed, giving two additional possible classifications (*factor-dependent* and *factor-specific*) besides the default ones (*similar*, *basal*, and *sparse*).
#' 
#' In factor analysis, an ANOVA is applied for each gene using Fisher's and Levene's tests to distinguish genes whose *logFC* (differential expression) variation is dependent or specific to the analyzed factors based on the p-value cutoff. The `p.val.adjust` argument defines how these p-values will be adjusted: by quantile separation between each factor (`"partial.quantiles"` method); or by one of the default methods listed in [stats::p.adjust.methods].
#' 
#' The `constraint.factors` argument determines if the `S` values (summarized *logFC*) will be limited to the range between the quantile centroids during factor analysis. For example, if the quantile centroids were -0.90, 0.00, and 0.90 in the S axis, values such as -1.53 and 2.96 would be converted to -0.90 and 0.90, respectively. This constraint is particularly applied to avoid significative observations from ANOVA based on multiple degrees of differential expression.
#' \cr
#' In another example to illustrate the constraint of factors, given two sets of values: A = (-1.00, -1,10, 0.00, 0.20, 1.00, 1.15), and B = (0.00, 0.12, 1.11, 1.00, 1.95, 2.00), with the centroids located in C = (-0.90, 0.00, 0.90), and the factors F = (Cond1, Cond1, Cond2, Cond2, Cond3, Cond3). If \code{constraint.factors} is \code{FALSE}, both A and B are considered as significantly separated factors, whereas if \code{TRUE}, only A will present a significant separation, since in B the values 1.11, 1.00, 1.95, and 2.00 are converted to 0.90. In qualitative terms, if \code{constraint.factors} is \code{TRUE}, all values above 0.90 are considered the same over-expressed values, ensuring that they will fit in the same degree of differential expression. Hence, in this example using the constrained values, B would not represent a significant separation between the factors Cond1, Cond2, and Cond3.
#' 
#' @note
#' To perform factor analysis, the following observations must be considered:
#' \itemize{
#' \item The factors must be defined in the provided data. They can be retrieved using the \code{\link{factors}} accessor. If factors are not present or are entirelly composed by \code{NA}, they can be assigned through \code{\link{factors<-}} by providing a \code{factor} or \code{character} vector of the same length of the input columns;
#' \item Each factor must include two or more values, since the factor analysis is based on ANOVA and at least two values are needed to variance calculation;
#' \item Columns whose factor value is \code{NA} are not considered.
#' }
#' 
#' @examples
#' ## Finalizing example using a random generated input
#' ginput <- geva.ideal.example()       # Generates a random input (for testing purposes only)
#' gsummary <- geva.summarize(ginput)   # Summarizes the input
#' gquant <- geva.quantiles(gsummary)   # Calculates the quantiles
#' gclust <- geva.cluster(gsummary)     # Calculates the clusters
#' gresults <- geva.finalize(gsummary, gquant, gclust)  # Finishes the results
#' 
#' head(top.genes(gresults))            # Prints the final results
#' plot(gresults)                       # Plots the final SV-plot
#' 
#' @seealso \link[stats]{p.adjust.methods}
#' 
#' @export
#' @rdname geva.finalize
#' @order 1
#' @md
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
  sel.similar = groups(gq) %in% vquantnms[mclassif[,'similar'] == 1]
  classification = rep(NA_character_, nprobes)
  classification[sel.basal] = 'basal'
  classification[sel.sparse] = 'sparse'
  classification[sel.similar] = 'similar'
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



# Performs a quick analysis from a GEVAInput, GEVASummary or GEVAResults.
# Optional (ellipsis) parameters are applied in intermediate steps.
# If the object is a GEVASummary or GEVAResults, the parameters used to obtain these objects are taken into account
# 
#' @title All-In-One Function for GEVA Intermediate Procedures
#' 
#' @description Given a [`GEVAInput-class`] object, applies the [`geva.summarize()`], [`geva.quantiles`], [`geva.cluster`], and [`geva.finalize`] in a single call. Optional arguments are passed to the internal calls of these functions.
#' 
#' @param gobject A `GEVAInput`, or any object that returns a `GEVAInput` upon calling `inputdata(gobject)` (*e.g.*, [`GEVASummary-class`] or [`GEVAResults-class`]).
#' @param ... Optional arguments passed to [`geva.summarize()`], [`geva.quantiles()`], [`geva.cluster()`], and [`geva.finalize()`]
#' 
#' @details
#' This function performs the summarization, quantile detecetion, and clustering of an input data, then merges the results together and, if applicable, performs a factor analysis. If the \code{gobject} is not a \code{GEVAInput}, it must provide a valid \code{GEVAInput} object when called by \code{inputdata(gobject)}. Moreover, all parameters used in previous analysis will be taken into account. For instance, if \code{gobject} is a \code{GEVASummary} obtained by using \code{variation.method='mad'}, the internal call to \code{geva.summarize} in this function will use \code{variation.method='mad'} as well, unless if another parameter for \code{variation.method} is specified in the \code{...} arguments.
#' 
#' Therefore, this function can be useful not only as a shortcut to analyze \code{GEVAInput} but also for parameter testing when applied to a \code{GEVAResults} object, since the previous parameters are reused, while the specified parameters are overriden.
#' 
#' @examples
#' ## Basic usage using a random generated input
#' ginput <- geva.ideal.example()   # Generates a random input example
#' gresults <- geva.quick(ginput)   # Performs the entire analysis (default parameters)
#' 
#' print(head(top.genes(gresults))) # Prints the results
#' plot(gresults)                        # Plots the final SV-plot
#' 
#' 
#' ## Example with non-default parameters
#' ginput <- geva.ideal.example()   # Generates a random input example
#' gresults <- geva.quick(ginput,
#'                        summary.method="median",
#'                        variation.method="mad",
#'                        quantiles.method="density",
#'                        cluster.method="density",
#'                        resolution=0.32)
#' 
#' print(head(top.genes(gresults))) # Prints the results
#' plot(gresults)                   # Plots the final SV-plot
#' 
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
