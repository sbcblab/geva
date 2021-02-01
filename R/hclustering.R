
##########################
# HIERARCHICAL CLUSTERING METHODS
# -----------------------
# 
# Functions to perform hierarchichal clustering on GEVA summarized data
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al

#' @include asserts.R
#' @include clusteringbase.R
#' @include statmath.R
NULL

# Distance metric between points to find hierarchical clusters
#' @options [geva.hcluster]
#' @order 8
options.hc.metric <- c('euclidean', 'maximum', 'manhattan', 'canberra', 'binary', 'minkowski')

# Method to find hierarchical clusters based on relationship between points
# The methods 'centroid', 'median' and 'ward' require hc.metric='euclidean'
#' @options [geva.hcluster]
#' @order 9
options.hc.method <- c('centroid', 'median', 'ward', 'single')

# Calculates the hierarchical tree through a cluster analysis
calc.hclust.tree <- function(sv, resolution, hc.method=options.hc.method, hc.metric=options.hc.metric, mink.p=NULL, ...)
{
  hc.method = match.arg(hc.method)
  hc.metric = match.arg(hc.metric)
  assert.operator(resolution, `>` = 0, `<=` = 1)
  svmatrix.norm = normalize.scale.numeric(as.matrix(sv))
  hc = hclust.vector(X = svmatrix.norm,
                     method = hc.method, metric = hc.metric,
                     p = (if (hc.metric == "minkowski") mink.p else NULL))
  horder = order(hc$height)
  # Finding the threshold based on resolution
  logres = -log10(hc$height)
  sel.outliers = seq_along(logres) %in% which.outliers(logres)
  hrange = range(logres[!sel.outliers])
  hsize = diff(hrange)
  logcutpos = hsize * resolution^2 + hrange[1] # Resolution is squared to have a more comprehensible scale
  indcutpos = which.min(logres[logres > logcutpos])
  tcut = hc$height[indcutpos]
  # Cutting the tree based on the found threshold
  hctree = list(height=hc$height[horder], labels=hc$labels, order=hc$order, merge=hc$merge, threshold=tcut)
  class(hctree) = c('hclust.geva', 'hclust')
  hctree
}

#' @title GEVA Hierarchical Clustering
#' 
#' @description Performs a hierarchical cluster analysis from summarized data.
#' 
#' @param sv a `numeric` [`SVTable-class`] object (usually [`GEVASummary-class`])
#' @param resolution `numeric` (`0` to `1`), used as a "zoom" parameter for cluster detection. A zero value returns the minimum number of clusters that can detected, while `1` returns the maximum amount of detectable clusters
#' @param hc.method `character`, the agglomeration method to be used. Used as the `method` argument for [fastcluster::hclust.vector()]
#' @param hc.metric `character`, the distance measure to be used. Used as the `metric` argument for [fastcluster::hclust.vector()]
#' @param ... additional arguments:
#' \itemize{
#' \item{`mink.p` : `numeric`, parameter for the Minkowsky metric. Used as the `p` argument for [fastcluster::hclust.vector()]}
#' \item{`verbose` : `logical`, whether to print the current progress (default is `TRUE`)}
#' }
#' @param cl.score.method `character`, method used to calculate the cluster scores for each point. If `"auto"`, the `"hclust.height"` method is selected
#' @param include.raw.results `logical`, whether to attach intermediate results to the returned object
#' 
#' @details
#' This function performs a hierarchical cluster analysis with the aid of implemented methods from the [`fastcluster::fastcluster`] package, particularly the [fastcluster::hclust.vector()] function. The available methods for the `hc.method` and `hc.metric` are described in the function's documentation page (see [fastcluster::hclust.vector()]).
#' 
#' The `resolution` value is an accessible way to define the cluster separation threshold used in hierarchical clustering. The algorithm produces a dendrogram-like hierarchy in which each level/node is separated by a distance (sometimes called "height") to the next level/node, and the `resolution` translates a value between `0` and `1` to a propotional value within the total hierarchy height. This allows defining the rate of clusters from `0` to `1`, which results in the least number of possible clusters (usually two) for `0`, and the highest number (approximately one cluster per point) for `1`.
#' 
#' If `include.raw.results` is `TRUE`, some aditional data will be attached to the `info` slot of the returned `GEVACluster` objects, including the *kNN* tree generated during the intermediate steps.
#' 
#' @note In hierarchical clustering, all points are clustered. Therefore, setting `resolution` to `1` will result into one cluster per point, where the cluster analysis may become pointless (no pun intended).
#' 
#' @return A [`GEVACluster-class`] object
#' 
#' @examples 
#' ## Hierarchical clustering from a randomly generated input 
#' 
#' # Preparing the data
#' ginput <- geva.ideal.example()      # Generates a random input example
#' gsummary <- geva.summarize(ginput)  # Summarizes with the default parameters
#' 
#' # Hierarchical clustering
#' gclust <- geva.hcluster(gsummary)
#' plot(gclust)
#' 
#' # Hierarchical clustering with slightly more resolution
#' gclust <- geva.hcluster(gsummary,
#'                        resolution=0.35)
#' plot(gclust)
#' 
#' @family geva.cluster
#' @rdname geva.hcluster
#' @export
#' @order 3
geva.hcluster <- function(sv, resolution=0.3, hc.method=options.hc.method, hc.metric=options.hc.metric,
                          cl.score.method=options.cl.score.method, ...,
                          include.raw.results=FALSE)
{
  cl.score.method = match.arg(cl.score.method)
  svmatrix = as.matrix(sv)
  hc.method = match.arg(hc.method)
  hc.metric = match.arg(hc.metric)
  assert.names.equal(svmatrix, colnames=c('S', 'V'))
  assert.operator(resolution, `>` = 0, `<=` = 1)
  an.pars = list(resolution=resolution,
                 cl.score.method = cl.score.method,
                 hc.method=hc.method,
                 hc.metric=hc.metric)
  if (cl.score.method == 'auto') cl.score.method = 'hclust.height'
  mink.p = ...arg(mink.p, NULL)
  if (!is.null(mink.p))
    an.pars$mink.p = mink.p
  vprint("Calculating hierarchical clustering...")
  hcx = calc.hclust.tree(sv=svmatrix, resolution=resolution, hc.method = hc.method, hc.metric = hc.metric, ...)
  tcut = hcx$threshold
  hcl = cutree(tree=hcx, h=tcut)
  vprint("Found %d clusters", length(unique(hcl)))
  # Getting the centroids
  centroid.method = if (inherits(sv, 'GEVASummary')) sv.method(sv)$S else options.summary[1]
  svcents = calc.cluster.centroids(svmatrix, hcl, centroid.method)
  svcents = as.SVTable(svcents[order(rownames(svcents)),,drop=FALSE])
  # Calculating the scores based on the centroids
  distance.method = ...arg(distance.method, if (hc.metric %in% options.distance) hc.metric else options.distance[1])
  an.pars$distance.method = distance.method
  hgrouping = as.factor(hcl)
  vscores = calc.cluster.scores(sv, resolution, cl = hgrouping, hctree=hcx,
                                sv.centroids=svcents, distance.method = distance.method)
  
  # Preparing the cluster object and finishing
  moffsets = calc.cluster.offsets(sv, hcl, svcents)
  infols = list(hc.method=hc.method,
                hc.metric=hc.metric,
                analysis.params=an.pars)
  if (include.raw.results)
    infols$raw.results = hcx
  
  new('GEVACluster',
      grouping=hgrouping,
      scores=vscores,
      centroids=svcents,
      offsets=moffsets,
      info=infols,
      cluster.method='hierarchical')
}
