
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
#' @name GEVACluster-class
#' @rdname GEVACluster-class
#' @export
setClass('GEVACluster', contains = 'GEVAGroupSet')


# INITIALIZE
setMethod('initialize', 'GEVACluster',
          function(.Object, ...)
          {
            .Object = callNextMethod(.Object, ...)
            argls = initialize.class.args(...)
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
            catline('Scores (%d): %s', length(object), fmt.limit(round(scores(object), 3)  ))
            if (length(infolist(object)) != 0) catline('Additional information (%d): %s', length(infolist(object)), fmt.limit(names(infolist(object))))
          })

# PLOT
setMethod('plot', c('GEVACluster', 'missing'),
          function(x, y, ...)
          {
            plotres = callNextMethod(x, ..., cl=groups(x), plotfn = hull.plot)
          })