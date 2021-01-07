
##########################
# GEVACluster Class
# -----------------------
# 
# Represents classification data for GEVA summaries separated by clusters
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0


#' @include c_GEVAGroupSet.R
#' @include usecasechecks.R

#' @title GEVA Clustering Results
#'
#' @description The \code{GEVACluster} class inherits the grouping properties from \code{GEVAGroupSet}.
#'
#' @slot cluster.method Method used to obtain the clustering results
#'
#' @name GEVACluster-class
#' @rdname GEVACluster-class
#' @export
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
setMethod('show', 'GEVACluster',
          function(object)
          {
            catline('GEVA Cluster (%s-class)', class(object)[1])
            catline('Clusters (%d): %s', length(levels(object)), fmt.limit(levels(object)))
            catline('Scores (%d): %s', length(scores(object)), fmt.limit(round(scores(object), 3)  ))
            if (length(infolist(object)) != 0) catline('Additional information (%d): %s', length(infolist(object)), fmt.limit(names(infolist(object))))
          })

# PLOT
setMethod('plot', c('GEVACluster', 'SVTable'),
          function(x, y, ...)
          {
            plotres = callNextMethod(x, y, ..., cl=groups(x), plotfn = hull.plot)
          })

# S4 Methods
setMethod('cluster.method', 'GEVACluster', function(object) object@cluster.method)

# S3 Methods
lines.GEVACluster <- function(x, ...)
{
  do.call(hull.lines, list.merge(list(x=sv(x), cl=groups(x), col=color.values(x)), list(...)))
  invisible()
}
