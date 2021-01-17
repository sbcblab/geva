
##########################
# DENSITY CLUSTERING METHODS
# -----------------------
# 
# Functions to perform density clustering on GEVA summarized data
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al

#' @include asserts.R
#' @include clusteringbase.R
#' @include statmath.R
NULL

# Calculates the density points tree for density clustering
calc.dclust.knnd <- function(sv, k=4)
{
  svmatrix.norm = normalize.scale.numeric(as.matrix(sv))
  knnd = dbscan::kNNdist(svmatrix.norm, k)
  knnd
}

# Estimates the epsilon for density clustering based on the resolution parameter
calc.eps.from.resolution <- function(svmatrix.norm, resolution, knnd=NULL)
{
  if (is.null(knnd))
    knnd = calc.dclust.knnd(svmatrix.norm)
  vrng = range(knnd)
  eps = diff(vrng) * (1 - resolution)^5
  eps
}

# Distance metric between points to find hierarchical clusters
#' @options [geva.dcluster]
#' @order 7
options.dcluster.method <- c('dbscan', 'optics')

#' @title GEVA Density Clustering
#' 
#' @description Performs a density cluster analysis from summarized data.
#' 
#' @param sv a `numeric` [`SVTable-class`] object (usually [`GEVASummary-class`])
#' @param resolution `numeric` (`0` to `1`), used as a "zoom" parameter for cluster detection. A zero value returns the minimum number of clusters that can detected, while `1` returns the maximum amount of detectable clusters. Ignored if `eps` is specified
#' @param dcluster.method `character`, density-based method for cluster separation
#' @param cl.score.method `character`, method used to calculate the cluster scores for each point. If `"auto"`, the `"density"` method is selected
#' @param minpts `integer`, minimum number of points required to form a cluster
#' @param ... additional arguments. Accepts `verbose` (`logical`, default is `TRUE`) to enable or disable printing the current progress
#' @param eps `numeric`, maximum neighborhood distance between points to be clustered
#' @param include.raw.results `logical`, whether to attach intermediate results to the returned object
#' 
#' @return A [`GEVACluster-class`] object
#' 
#' @details
#' This function performs a density cluster analysis with the aid of implemented methods from the [`dbscan::dbscan`] package. The available methods for the `dcluster.method` arguments are `"dbscan"` and `"options"`, which internally call [`dbscan::dbscan()`] and [`dbscan::optics()`], respectively.
#' 
#' The `resolution` value is an accessible way to define the cluster separation threshold used in density clustering. The *DBSCAN* algorithm uses an *epsilon* value that represents the minimum distance of separation, and `resolution` translates a value between `0` and `1` to a propotional value within the acceptable range of *epsilon* values. This allows defining the rate of clusters from `0` to `1`, which results in the least number of possible clusters for `0` and the highest number for `1`. Nevertheless, if *epsilon* is specified as `eps` in the optinal arguments, its value is used and `resolution` is ignored.
#' 
#' The `cl.score.method` argument defines how scores are calculated for each SV point (row in `sv`) that was assigned to a cluster, (*i.e.*, excluding non-clustered points). If specified as `"auto"`, the parameter will be selected based on the rate of neighbor points (`"density"`).
#' 
#' If `include.raw.results` is `TRUE`, some aditional data will be attached to the `info` slot of the returned `GEVACluster` objects, including the *kNN* tree generated during the intermediate steps.
#' 
#' @note In density clustering, only the most dense points are clustered. For the unclustered points, the grouping value is set to `NA`.
#' 
#' @examples 
#' ## Density clustering from a randomly generated input 
#' 
#' # Preparing the data
#' ginput <- geva.ideal.example()      # Generates a random input example
#' gsummary <- geva.summarize(ginput)  # Summarizes with the default parameters
#' 
#' # Density clustering
#' gclust <- geva.dcluster(gsummary)
#' plot(gclust)
#' 
#' # Density clustering with slightly more resolution
#' gclust <- geva.dcluster(gsummary, resolution=0.35)
#' plot(gclust)
#' 
#' @family geva.cluster
#' @rdname geva.dcluster
#' @export
#' @order 2
geva.dcluster <- function(sv, resolution=0.3, dcluster.method=options.dcluster.method, cl.score.method=options.cl.score.method, minpts=2, ..., eps=NA_real_, include.raw.results=FALSE)
{
  cl.score.method = assert.choices(cl.score.method)
  assert.operator(resolution, `>` = 0, `<=` = 1)
  svmatrix = as.matrix(sv)
  dcluster.method = match.arg(dcluster.method)
  assert.names.equal(svmatrix, colnames=c('S', 'V'))
  an.pars = list(resolution=resolution,
                 dcluster.method=dcluster.method,
                 cl.score.method=cl.score.method,
                 minpts = minpts,
                 eps = eps)
  if (cl.score.method == 'auto') cl.score.method = 'density'
  svmatrix.norm = normalize.scale.numeric(svmatrix)
  vprint("Calculating density clustering...")
  knnd=NULL
  if (is.null(eps) || is.na(eps))
  {
    knnd = calc.dclust.knnd(svmatrix.norm)
    eps = calc.eps.from.resolution(svmatrix.norm, resolution, knnd=knnd)
  }
  #else
    #svmatrix.norm = svmatrix
  clust.res = switch (dcluster.method,
    dbscan = dbscan::dbscan(svmatrix.norm, eps, minPts = minpts),
    optics = {
      opt.res = dbscan::optics(svmatrix.norm, eps, minPts = minpts)
      extractDBSCAN(opt.res, eps)
    },
  )
  class(clust.res) = c(sprintf("%s.geva", class(clust.res)[1]), class(clust.res))
  cl = clust.res$cluster
  cl[cl %in% 0] = NA
  cl = as.factor(cl)
  
  vprint(sprintf("Found %d clusters", length(unique(na.omit(cl)))))
  
  # Getting the centroids
  centroid.method = if (inherits(sv, 'GEVASummary')) sv.method(sv)$S else options.summary[1]
  svcents = calc.cluster.centroids(svmatrix, cl, centroid.method)
  svcents = as.SVTable(svcents[order(rownames(svcents)),,drop=FALSE])
  
  # Calculating the scores based on the centroids
  distance.method = ...arg(distance.method, options.distance[1])
  an.pars$distance.method = distance.method
  vscores = calc.cluster.scores(sv, resolution, cl=cl, sv.centroids = svcents,
                                distance.method = distance.method,
                                cl.score.method = cl.score.method,
                                knnd=knnd)
  
  # Preparing the cluster object and finishing
  hgrouping = as.factor(cl)
  moffsets = calc.cluster.offsets(sv, cl, svcents)
  infols = list(dcluster.method=dcluster.method,
                analysis.params=an.pars)
  if (include.raw.results)
  {
    clust.res$kNN = knnd
    infols$raw.results = clust.res
  }
  
  new('GEVACluster',
      grouping=hgrouping,
      scores=vscores,
      centroids=svcents,
      offsets=moffsets,
      info=infols,
      cluster.method='density')
}
