
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


# Distance metric between points to find hierarchical clusters
options.hc.metric <- c('euclidean', 'maximum', 'manhattan', 'canberra', 'binary', 'minkowski')

# Method to find hierarchical clusters based on relationship between points
# The methods 'centroid', 'median' and 'ward' require hc.metric='euclidean'
options.hc.method <- c('centroid', 'median', 'ward', 'single')

# Performs hierarchical clustering
geva.hcluster <- function(sv, resolution=0.25, hc.method=options.hc.method, hc.metric=options.hc.metric, ...)
{
  svmatrix = as.matrix(sv)
  hc.method = assert.choices(hc.method)
  hc.metric = assert.choices(hc.metric)
  assert.names.equal(svmatrix, colnames=c('S', 'V'))
  assert.operator(resolution, `>` = 0, `<=` = 1)
  mink.p = ...arg(p, NULL)
  vprint("Calculating hierarchical clustering...")
  hc = hclust.vector(X = svmatrix, method = hc.method, metric = hc.metric, p = mink.p)
  fdt = data.frame(row.names = rownames(svmatrix))
  horder = order(hc$height)
  
  # Finding the threshold based on resolution
  logres = -log10(hc$height)
  hrange = range(logres)
  hsize = diff(hrange)
  logcutpos = hsize * resolution^2 + hrange[1] # Resolution is squared to have a more comprehensible scale
  indcutpos = which.min(logres[logres > logcutpos])
  tcut = hc$height[indcutpos]
  
  # Cutting the tree based on the found threshold
  hcx = list(height=hc$height[horder], labels=hc$labels, order=hc$order, merge=hc$merge)
  hctree = cutree(tree=hcx, h=tcut)
  vprint("Found %d clusters", length(unique(hctree)))
  
  # Getting the centroids
  centroid.method = if (inherits(sv, 'GEVASummary')) sv.method(sv)$S else options.summary[1]
  svcents = calc.cluster.centroids(svmatrix, hctree, centroid.method)
  svcents = as.SVTable(svcents[order(rownames(svcents)),])
  
  # Calculating the scores based on the centroids
  distance.method = ...arg(distance.method, if (hc.metric %in% options.distance) hc.metric else options.distance[1])
  vscores = calc.cluster.scores(sv, hctree, svcents, distance.method)
  
  # Preparing the cluster object and finishing
  hgrouping = as.factor(hctree)
  moffsets = calc.cluster.offsets(sv, hctree, svcents)
  infols = list(hc.method=hc.method, hc.metric=hc.metric)
  
  new('GEVACluster',
      grouping=hgrouping,
      scores=vscores,
      centroids=svcents,
      offsets=moffsets,
      info=infols)
}