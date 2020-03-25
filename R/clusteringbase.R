
##########################
# BASE CLUSTERING METHODS
# -----------------------
# 
# Essential functions to perform clustering on GEVA summarized data
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

#' @include summarization.R


# Distance options
#' @export
options.distance <- c('euclidean', 'manhattan')


# Calculates the centroids of the cluster points based on mean or median parameters
calc.cluster.centroids <- function(sv, cl, centroid.method=options.summary)
{
  svmatrix = as.matrix(sv)
  vsm = svmatrix[,'S']
  vvr = svmatrix[,'V']
  centroid.method = assert.choices(centroid.method)
  summf = get.summary.method(centroid.method)
  svcents = svtable(tapply(vsm, cl, summf), tapply(vvr, cl, summf))
  svcents
}

# Calculates the offsets of the cluster points
calc.cluster.offsets <- function(sv, cl, mcents)
{
  cl = as.factor(cl)
  assert.names.equal(mcents, rownames=sort(levels(cl)))
  mdists = as.matrix(sv)^0
  for (ci in rownames(mcents))
  {
    selcl = cl == ci
    if (!any(selcl)) next
    msv = sv[selcl,,drop=FALSE]
    r = as.numeric(mcents[ci,,drop=TRUE])
    mdists[selcl,] = t(apply(msv, 1, function(x, r) x - r, r=r))
  }
  svdists = svtable(mdists[,'S'], mdists[,'V'], rownames(mdists))
  svdists
}

# Calculates the scores of the cluster points
calc.cluster.scores <- function(sv, cl, mcents, distance.method=options.distance)
{
  distance.method = assert.choices(distance.method)
  cl = as.factor(cl)
  assert.names.equal(mcents, rownames=sort(levels(cl)))
  vscores = rep(0, nrow(sv))
  fndist = get.distance.method(distance.method)
  for (ci in rownames(mcents))
  {
    selcl = cl == ci
    if (!any(selcl)) next
    msv = sv[selcl,,drop=FALSE]
    r = as.numeric(mcents[ci,,drop=TRUE])
    vdists = apply(msv, 1, fndist, r=r)
    maxdist = max(vdists)
    vdists.rel = if (maxdist == 0) vdists else vdists / maxdist
    vscores[selcl] = 1 - vdists.rel
  }
  vscores = setNames(vscores, rownames(sv))
  vscores
}

# Gets the distance calculation function from the character
get.distance.method.character <- function(method=options.distance)
{
  method = assert.choices(method)
  fn = switch(method,
              euclidean = function(x, r) sqrt(sum((r-x)^2)),
              manhattan = function(x, r) sum(abs(r-x))
  )
  fn
}