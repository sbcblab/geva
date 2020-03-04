
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
geva.hcluster <- function(svmatrix, resolution=0.85, hc.method=options.hc.method, hc.metric=options.hc.metric, ...)
{
  assert.choices(hc.method)
  assert.choices(hc.metric)
  assert.names.equal(svmatrix, colnames=c('S', 'V'))
  assert.operator(resolution, `>` = 0, `<=` = 1)
  mink.p = ...arg(p, NULL)
  hc = hclust.vector(X = svmatrix, method = hc.method, metric = hc.metric, p = mink.p)
  fdt = data.frame(row.names = rownames(svmatrix))
  
  
}