
##########################
# BASE CLUSTERING METHODS
# -----------------------
# 
# Essential functions to perform clustering on GEVA summarized data
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al

#' @include linq.R
#' @include summarization.R
#' @include c_GEVAGroupedSummary.R
NULL

# Supported clustering methods
#' @options [geva.cluster]
#' @order 4
options.cluster.method <- c('hierarchical', 'density', 'quantiles')

# Cluster score calculation methods
#' @options [geva.cluster]
#' @order 5
options.cl.score.method <- c('auto', 'hclust.height', 'density', 'centroid')

# Distance options
#' @options [geva.cluster]
#' @order 6
options.distance <- c('euclidean', 'manhattan')


# Calculates the centroids of the cluster points based on mean or median parameters
calc.cluster.centroids <- function(sv, cl, centroid.method=options.summary)
{
  svmatrix = as.matrix(sv)
  vsm = svmatrix[,'S']
  vvr = svmatrix[,'V']
  centroid.method = match.arg(centroid.method)
  summf = get.summary.method(centroid.method)
  svcents = svtable(tapply(vsm, cl, summf), tapply(vvr, cl, summf))
  svcents
}

# Calculates the offsets of the cluster points
calc.cluster.offsets <- function(sv, cl, mcents)
{
  cl = as.factor(cl)
  assert.names.equal(mcents, rownames=sort(levels(cl)))
  mdists = as.matrix(sv)
  for (ci in rownames(mcents))
  {
    selcl = cl %in% ci
    if (!any(selcl)) next
    msv = sv[selcl,,drop=FALSE]
    r = as.numeric(mcents[ci,,drop=TRUE])
    mdists[selcl,] = t(apply(msv, 1, function(x, r) x - r, r=r))
  }
  svdists = svtable(mdists[,'S'], mdists[,'V'], rownames(mdists))
  svdists
}

# Calculates the scores based on the distances between the SV points and each cluster's centroid
calc.cluster.scores.centroid <- function(sv, cl, sv.centroids, distance.method=options.distance)
{
  if (is.null(sv.centroids) || !inherits(sv, c('matrix', 'SVTable')))
    stop("'sv.centroids' must be a SVTable or a two-column numeric matrix")
  distance.method = match.arg(distance.method)
  cl = as.factor(cl)
  assert.names.equal(sv.centroids, rownames=sort(levels(cl)))
  vscores = rep(0, nrow(sv))
  fndist = get.distance.method(distance.method)
  for (ci in rownames(sv.centroids))
  {
    selcl = cl %in% ci
    if (!any(selcl)) next
    msv = sv[selcl,,drop=FALSE]
    r = as.numeric(sv.centroids[ci,,drop=TRUE])
    vdists = apply(msv, 1, fndist, r=r)
    maxdist = max(vdists)
    vdists.rel = if (maxdist == 0) vdists else vdists / maxdist
    vscores[selcl] = 1 - vdists.rel
  }
  vscores = setNames(vscores, rownames(sv))
  vscores
}

# Calculates the cluster scores based on the hierarchy height
calc.cluster.scores.hclust.height <- function(sv, resolution, cl, hctree=NULL, ...)
{
  if (is.null(hctree))
    hctree = calc.hclust.tree(sv, resolution, ...)
  if (is.list(hctree) && (is.null(names(hctree)) || any(!(c('height', 'order', 'threshold') %in% names(hctree)))))
    stop("'hctree' must be a result list from hclust or hclust.vector")
  
  hcinds = data.frame(index=rep(1L:nrow(hctree$merge), 2), map=as.integer(-hctree$merge))
  hcinds = hcinds[hcinds$map > 0,,drop=FALSE]
  vheights = hctree$height[hcinds$index][hcinds$map]
  ls.lochs = tapply(-log10(vheights), cl, function(x) { normalize.scale.numeric(abs(x - median(x)), default=1)})
  for (g in names(ls.lochs))
    vheights[cl %in% g] = ls.lochs[[g]]
  vheights
}

# Calculates the cluster scores based on the point density
calc.cluster.scores.density <- function(sv.norm, cl, knnd=NULL, ...)
{
  if (inherits(sv.norm, "SVTable"))
  {
    sv.norm = normalize.scale.numeric(sv(sv.norm))
  }
  if (is.null(knnd))
    knnd = calc.dclust.knnd(sv.norm)
  vscores = rep(1.0, length(cl))
  min.knd = min(knnd)
  knnd.log = -log10(clamp(knnd, 1e-20, 1))
  ls.local.knn = tapply(knnd.log, cl, function(x) normalize.scale.numeric(x, min = min.knd, default=1))
  knnd.norm = normalize.scale.numeric(knnd, min=min.knd)
  w0 = length(cl)
  for (g in names(ls.local.knn))
  {
    sel.cl = cl %in% g
    w1 = sum(sel.cl)
    if (w1 > 1)
    {
      #w1 = w1 * 2
      vscores[sel.cl] = (ls.local.knn[[g]] + knnd.norm[sel.cl]) / 2
      #vscores[sel.cl] = (w0 * ls.local.knn[[g]] + w1 * knnd.norm[sel.cl]) / (w1 + w0)
    }
  }
  vscores
}

# Calculates the scores of the cluster points
calc.cluster.scores <- function(sv, resolution, cl, cl.score.method=options.cl.score.method, ...,
                                sv.centroids, distance.method, hctree, knnd)
{
  if (missing(sv.centroids)) sv.centroids = NULL
  if (missing(distance.method)) distance.method = NULL
  if (missing(hctree)) hctree = NULL
  if (missing(knnd)) knnd = NULL
  cl.score.method = match.arg(cl.score.method)
  if (cl.score.method == 'auto')
    cl.score.method = tail(options.cl.score.method, n = 1L)
  cl = as.factor(cl)
  vscores = switch (cl.score.method,
                    hclust.height = calc.cluster.scores.hclust.height(sv, resolution, cl, hctree, ...),
                    density = calc.cluster.scores.density(sv, cl, knnd, ...),
                    centroid = calc.cluster.scores.centroid(sv, cl, sv.centroids, distance.method),
                    stop("Invalid argument for 'cl.score.method': ", cl.score.method)
  )
  names(vscores) = rownames(sv)
  vscores
}

# Appends one or more groupsets in a GEVASummary object, returning a GEVAGroupedSummary
append.groupsets <- function(gsummary, ...)
{
  assert.class(gsummary, inherits='GEVASummary')
  if (...length() == 0L) return(gsummary)
  tls = groupsets(gsummary)
  args = list(...)
  argnms = call.dots.namesorargs(...)
  for (i in 1:...length())
  {
    argnm = argnms[i]
    ggset = ...elt(i)
    if (inherits(ggset, 'GEVAGroupSet'))
    {
      tls[[argnm]] = ggset
    }
  }
  groupsets(gsummary) = tls
  gsummary
}

# Gets the distance calculation function from the character
get.distance.method.character <- function(method=options.distance)
{
  method = match.arg(method)
  fn = switch(method,
              euclidean = function(x, r) sqrt(sum((r-x)^2)),
              manhattan = function(x, r) sum(abs(r-x))
  )
  fn
}

#' @title GEVA Cluster Analysis
#' 
#' @description Performs a cluster analysis from summarized data.
#' 
#' @param sv a `numeric` [`SVTable-class`] object (usually [`GEVASummary-class`])
#' @param cluster.method `character`, one of the main grouping methods (see `Details')
#' @param cl.score.method `character`, method used to calculate the cluster scores for each point. Ignored if `cluster.method` is `quantiles`
#' @param resolution `numeric` (`0` to `1`), used as a "zoom" parameter for cluster detection. A zero value returns the minimum number of clusters that can detected by the `cluster.method`, while `1` returns the maximum amount of clusters. Ignored if `cluster.method` is `quantiles`
#' @param distance.method `character`, two-point distance calculation method. Options are `"eucludian"` or `"manhattan"` distances
#' @param ... further arguments passed to [`geva.dcluster()`], [`geva.hcluster()`], or [`geva.quantiles()`].
#' \cr In addition, the following arguments are accepted:
#' \itemize{
#' \item{`eps` : `numeric`, defines the *epsilon* coefficient for density clustering (see 'Details')}
#' \item{`mink.p` : `numeric`, parameter for the Minkowsky metric used in hierarchial clustering. Used as the `p` argument for [fastcluster::hclust.vector()]}
#' \item{`verbose` : `logical`, whether to print the current progress (default is `TRUE`)}
#' }
#' @param grouped.return `logical`, whether to concatenate the clustered and summarized data into a single object
#' 
#' @return This function produces a [`GEVAGroupSet-class`]-derived object, particularly a [`GEVACluster-class`] for the `"hierarchical"` and `"density"` cluster methods or a [`GEVAQuantiles-class`] for the `"quantiles"` method.
#' 
#' However, if `grouped.return` is `TRUE` and `sv` is a [`GEVASummary-class`] object, the produced `GEVAGroupSet` data will be concatenated to the input and returned as a [`GEVAGroupedSummary-class`]
#' 
#' @details
#' The `cluster.method` determines which grouping subroutine is used to classify the summarized data points based on distance and partitioning. Each option has their equivalent functions that can be called directly: `"density"` uses [`geva.dcluster()`]; `"hierarchical"` uses [`geva.hcluster()`]; and `"quantiles"` calls [`geva.quantiles()`]. However, this wrapper function can also be used to join `GEVASummary` and `GEVAGroupSet` objects into a single `GEVAGroupedSummary` object by setting `grouped.return` to `TRUE`.
#' 
#' The `cl.score.method` argument defines how scores are calculated for each SV point (row in `sv`) that was assigned to a cluster, (*i.e.*, excluding non-clustered points). If specified as `"auto"`, the parameter will be selected based on the `cluster.method`: `"density"` (rate of neighbor points) for the density method; and `"hclust.height"` (local hierarchy height) for the hierarchical method. The `"centroid"` method calculates the scores based on the proportional distance between each point to its cluster's centroid. Note that the `cl.score.method` argument is ignored if `cluster.method` is `"quantiles"`, since quantile scores are always based on their local centroid distances.
#' 
#' The `resolution` value is a more accessible way to define the cluster separation threshold used in density and hierarchical clustering methods. Density clusters uses an *epsilon* value that represents the minimum distance of separation, whereas hierarchical clusters are defined by cutting the hierarchy tree wherever there is a minimum distance between two hierarchies. In this sense, `resolution` translates a value between `0` and `1` to propotional value for *epsilon* or hierarchical height (depending on the `cluster.method`) that would result in the least number of possible clusters for `0` and the highest number for `1`. Nevertheless, if *epsilon* is specified as `eps` in the optinal arguments, its value is used and `resolution` is ignored.
#' 
#' @examples
#' ## Cluster analysis from a randomly generated input 
#' 
#' # Preparing the data
#' ginput <- geva.ideal.example()      # Generates a random input example
#' gsummary <- geva.summarize(ginput)  # Summarizes with the default parameters
#' 
#' # Hierarchical clustering
#' gclust <- geva.cluster(gsummary, cluster.method="hierarchical")
#' plot(gclust)
#' 
#' # Density clustering
#' gclust <- geva.cluster(gsummary, cluster.method="density")
#' plot(gclust)
#' 
#' # Density clustering with slightly more resolution
#' gclust <- geva.cluster(gsummary,
#'                        cluster.method="density",
#'                        resolution=0.35)
#' plot(gclust)
#' 
#' @export
#' @family geva.cluster
#' @rdname geva.cluster
#' @order 1
geva.cluster <- function(sv, cluster.method=options.cluster.method, cl.score.method=options.cl.score.method, resolution=0.3, distance.method=options.distance, ..., grouped.return=FALSE)
{
  cluster.method = match.arg(cluster.method)
  distance.method = match.arg(distance.method)
  if (inherits(sv, 'GEVAInput'))
  {
    sv = geva.summarize(sv, ...)
  }
  argls = list(...)
  gclust = switch(cluster.method,
                  density = geva.dcluster(sv, resolution=resolution, cl.score.method=cl.score.method,
                                          distance.method=distance.method, ...),
                  hierarchical = geva.hcluster(sv, resolution=resolution, cl.score.method=cl.score.method,
                                               distance.method=distance.method, ...),
                  quantiles = geva.quantiles(sv, distance.method=distance.method, ...)
                  )
  if (grouped.return && inherits(sv, 'GEVASummary'))
  {
    groupsets(sv)[[make.unique(tail(c(names(groupsets(sv)), cluster.method), n=1L))]] = gclust
    return(sv)
  }
  gclust
}
