
##########################
# DENSITY CLUSTERING METHODS
# -----------------------
# 
# Functions to perform density clustering on GEVA summarized data
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

#' @include asserts.R
#' @include clusteringbase.R
#' @include statmath.R

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
options.dcluster.method <- c('dbscan', 'optics')

# Performs a density clustering analysis
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
