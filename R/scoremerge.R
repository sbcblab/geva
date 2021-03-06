
##########################
# SCORE MERGE METHODS
# -----------------------
# 
# Functions to merge score data from clustered data in GEVA
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al

#' @include asserts.R
#' @include clusteringbase.R
#' @include quantiles.R
#' @include classhelpers.R
NULL

# Computes the association between a GEVAGroupSet and a GEVAQuantiles
# A group is associated to a quantile when its centroid is located inside each other's area
# Returns a named factor such as (group = quantile)
quantiles.assoc.gset <- function(gquant, ggset)
{
  assert.class(gquant, inherits='GEVAQuantiles')
  assert.class(ggset, inherits='GEVAGroupSet')
  ggs.cents = centroids(ggset)
  gq.cents = centroids(gquant)
  gq.aszs = qareasizes(gquant)
  gassocs = calc.quantile.nearest.SVTable(ggs.cents, gquant)
  gassocs
}

# Merges the scores between a GEVAQuantiles and other GEVAGroupset's given in dots
# Returns a GEVAQuantilesAdjusted if any adjustments were made, or a GEVAQuantiles if no quantile adjustement was done
quantiles.scores.merge <- function(gsumm, ...)
{
  assert.class(gsumm, inherits='GEVASummary')
  dotsls = call.dots.named.list(...)
  argls = list.merge(dotsls,
                     groupsets(gsumm))
  gquant = first(argls ~ inherits(arg, 'GEVAQuantiles'), default = geva.quantiles(gsumm))
  ggsets = where(argls ~ inherits(arg, 'GEVAGroupSet') && !inherits(arg, 'GEVAQuantiles'))
  fquant = groups(gquant)
  vqscore = scores(gquant)
  lfassocs = typed.list(elem.class = 'factor')
  ggnms = if (is.named(ggsets)) names(ggsets) else rep('', length(ggsets))
  ggnms = make.names(ggnms)
  moffs = as.matrix(offsets(gquant))
  gqcents = centroids(gquant)
  mqinds = sv(as.SVTable.GEVAQuantiles(gquant, 'qindexes'))
  mqinds.ref = sv(qindexes(gquant))
  i = 0L
  for (ggs in ggsets)
  {
    i = i + 1L
    gmethod = cluster.method(ggs)
    if (!is.na(gmethod)) ggnms[i] = sprintf("%s.%s", gmethod, ggnms[i])
    ggnm = ggnms[i]
    vggscore = scores(ggs)
    gassocs = quantiles.assoc.gset(gquant, ggs)
    lfassocs[[ggnm]] = gassocs
    fggs = gassocs[as.character(groups(ggs))]
    selmism = !is.na(fggs) & (fquant != fggs) # Groups with differing quantile definition/association
    if (any(selmism))
    {
      seladj = selmism
      mqinds.dist = mqinds[selmism,,drop=FALSE] - mqinds.ref[as.character(fggs[selmism]),,drop=FALSE]
      vqdist = sqrt(rowSums(mqinds.dist^2))
      vqscore.adj = vqscore[selmism]^(1/clamp(vqdist, min.value = 1L)) # Adjusts the score if the cluster's quantile is not a neighbor quantile
      seladj[selmism] = vggscore[selmism] > vqscore.adj # Groups with greater cluster score than the current quantile score
      if (any(seladj))
      {
        vcurrqs = as.character(fquant[seladj])
        fquant[seladj] = fggs[seladj]
        vchangedqs = as.character(fquant[seladj])
        moffstmp = gqcents[vcurrqs, ] - gqcents[vchangedqs, ]
        moffs[seladj, ] = moffs[seladj, ] + moffstmp
        vqscore[seladj] = vggscore[seladj]
      }
    }
  }
  if (length(ggnms) != 0L)
  {
    names(ggsets) = ggnms
    gsumm = promote.class(gsumm, 'GEVAGroupedSummary', groupsetlist=ggsets)
  }
  gquant.adj = if (length(lfassocs) != 0L)
  {
    attr(lfassocs, 'title') = 'List of GroupSet-Quantile relationships'
    promote.class(gquant, 'GEVAQuantilesAdjusted', grouprels=lfassocs, grouping=fquant, scores=vqscore, offsets=as.SVTable(moffs))
  }
  else gquant
  list(
    gsummary=gsumm,
    gquant.adj=gquant.adj)
}

