
##########################
# GEVACluster Class
# -----------------------
# 
# Represents classification data for GEVA summaries separated by clusters
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al

#' @include c_GEVAGroupSet.R
#' @include usecasechecks.R
NULL

#' @title GEVA Clustering Results
#'
#' @description The \code{GEVACluster} class represents the classification results from a cluster analysis. For each probe/gene, there is a assigned cluster among the \emph{g} defined clusters.
#' 
#' This class inherits from \code{\linkS4class{GEVAGroupSet}}.
#'
#' @slot grouping \code{factor} (\emph{m} elements, \emph{g} levels), cluster assignment for each gene/probe
#' \cr (Inherited from \code{\linkS4class{GEVAGroupSet}})
#' @slot scores \code{numeric} vector (\emph{m} elements) comprising a score value for each cluster assignment
#' \cr (Inherited from \code{\linkS4class{GEVAGroupSet}})
#' @slot ftable \code{data.frame} (\emph{m} lines) with additional cluster assignment features
#' \cr (Inherited from \code{\linkS4class{GEVAGroupSet}})
#' @slot centroids \code{numeric SVTable} (\emph{g} lines) with the S and V centroid coordinates for each cluster
#' \cr (Inherited from \code{\linkS4class{GEVAGroupSet}})
#' @slot offsets \code{numeric SVTable} (\emph{m} lines) with the S and V coordinate offsets each gene/probe from its cluster centroid
#' \cr (Inherited from \code{\linkS4class{GEVAGroupSet}})
#' @slot info \code{list} of supplementary information
#' \cr (Inherited from \code{\linkS4class{GEVAGroupSet}})
#'
#' @slot cluster.method \code{character}, method used in the cluster analysis (see \code{\link{geva.cluster}})
#'
#' @declareS4class
setClass('GEVACluster',
         slots = c(
           cluster.method = 'character'
         ), contains = 'GEVAGroupSet')


# INITIALIZE
setMethod('initialize', 'GEVACluster',
          function(.Object, ...)
          {
            .Object = callNextMethod(.Object, ...)
            argls = initialize.class.args(...)
            cluster.method = get.initialized(argls$cluster.method, 'generic')
            assert.dim(cluster.method, length=1L)
            .Object@cluster.method = cluster.method
            validObject(.Object)
            .Object
          }
)

# SHOW
#' @category Properties
#' @s4method
setMethod('show', 'GEVACluster',
          function(object)
          {
            catline('GEVA Cluster (%s-class)', class(object)[1])
            catline('Clusters (%d): %s', length(levels(object)), fmt.limit(levels(object)))
            catline('Scores (%d): %s', length(scores(object)), fmt.limit(round(scores(object), 3)  ))
            if (length(infolist(object)) != 0) catline('Additional information (%d): %s', length(infolist(object)), fmt.limit(names(infolist(object))))
          })

# PLOT

#' @category Plotting
#' @s4method Draws a SV-plot that highlights the clustered points. Convex hulls are included for visual purposes only and do not avoid enclosing points from other clusters on concave parts.
#' \cr Can be combined with another `SVTable` or `GEVAGroupSet` given as the `y` argument to include additional graphical elements
setMethod('plot', c('GEVACluster', 'SVTable'),
          function(x, y, ...)
          {
            plotres = callNextMethod(x, y, ..., cl=groups(x), plotfn = hull.plot)
          })

# S4 Methods

#' @methodsnote (See also the inherited methods from [`GEVAGroupSet-class`])

#' @category Properties

#' @s4method
#' @s4accessor
setMethod('cluster.method', 'GEVACluster', function(object) object@cluster.method)


# S3 Methods

#' @category Plotting

#' @s3method Draws convex hulls around the clustered points
lines.GEVACluster <- function(x, ...)
{
  do.call(hull.lines, list.merge(list(...), list(x=sv(x), cl=groups(x), col=color.values(x))))
  invisible()
}
