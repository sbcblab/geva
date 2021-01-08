
##########################
# HIERARCHICAL CLUSTERING METHODS
# -----------------------
# 
# Functions to perform hierarchichal clustering on GEVA summarized data
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

#' @include asserts.R
#' @include clusteringbase.R
#' @include statmath.R




# Distance metric between points to find hierarchical clusters
options.hc.metric <- c('euclidean', 'maximum', 'manhattan', 'canberra', 'binary', 'minkowski')

# Method to find hierarchical clusters based on relationship between points
# The methods 'centroid', 'median' and 'ward' require hc.metric='euclidean'
options.hc.method <- c('centroid', 'median', 'ward', 'single')

# Calculates the hierarchical tree through a cluster analysis
calc.hclust.tree <- function(sv, resolution, hc.method=options.hc.method, hc.metric=options.hc.metric, mink.p=NULL, ...)
{
  hc.method = match.arg(hc.method)
  hc.metric = match.arg(hc.metric)
  assert.operator(resolution, `>` = 0, `<=` = 1)
  svmatrix.norm = normalize.scale.numeric(as.matrix(sv))
  hc = hclust.vector(X = svmatrix.norm, method = hc.method, metric = hc.metric, p = mink.p)
  horder = order(hc$height)
  # Finding the threshold based on resolution
  logres = -log10(hc$height)
  sel.outliers = 1:length(logres) %in% which.outliers(logres)
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

# Performs a hierarchical clustering analysis
geva.hcluster <- function(sv, resolution=0.3, hc.method=options.hc.method, hc.metric=options.hc.metric,
                          cl.score.method=options.cl.score.method, ...,
                          include.raw.results=FALSE)
{
  cl.score.method = assert.choices(cl.score.method)
  svmatrix = as.matrix(sv)
  hc.method = assert.choices(hc.method)
  hc.metric = assert.choices(hc.metric)
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
  hcx = calc.hclust.tree(svmatrix, resolution, ...)
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