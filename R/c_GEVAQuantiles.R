
##########################
# GEVAQuantiles Class
# -----------------------
# 
# Represents classification data for GEVA summaries separated by quantiles
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0


#' @include c_GEVAGroupSet.R
#' @include c_SVAttribute.R
#' @include usecasechecks.R

#' @title GEVA Quantiles Grouping Results
#'
#' @description The \code{GEVAQuantiles} class inherits the grouping properties from \code{GEVAGroupSet} and additionally stores the indexes associated to summary and variation.
#'
#' @slot qindexes SVTable with indexes representing the magnitude of summary and variation
#' @slot qcount SVIntAttribute with number of quantiles for summary and variation
#' @slot qcutoff SVNumAttribute with the initial quantile cut-offs, starting from zero
#'
#' @name GEVAQuantiles-class
#' @rdname GEVAQuantiles-class
#' @export
setClass('GEVAQuantiles',
         slots = c(
           qindexes = 'SVTable',
           qcount = 'SVIntAttribute',
           qcutoff = 'SVNumAttribute'
         ), contains = 'GEVAGroupSet')

# TODO: Include individual scores for SV (?)

# INITIALIZE
setMethod('initialize', 'GEVAQuantiles',
          function(.Object, ...)
          {
            .Object = callNextMethod(.Object, ...)
            argls = initialize.class.args(...)
            .Object@qindexes = argls$qindexes
            qcount = get.initialized(argls$qcount, svattr(3L, 2L))
            check.quantiles.count(summary(qcount), variation(qcount))
            .Object@qcount = qcount
            validObject(.Object)
            .Object
          }
          )

# SHOW
setMethod('show', 'GEVAQuantiles',
          function(object)
          {
            catline('GEVA Quantiles (%s-class)', class(object)[1])
            catline('Quantiles (%d): %s', length(levels(object)), fmt.limit(levels(object)))
            catline('Scores (%d): %s', length(object), fmt.limit(scores(object)))
            if (length(infolist(object)) != 0) catline('Additional information (%d): %s', length(infolist(object)), fmt.limit(names(infolist(object))))
          })


# S4 Methods
setMethod('qindexes', 'GEVAQuantiles', function(object) object@qindexes)
setMethod('quantiles', 'GEVAQuantiles', function(object) levels(object))
setMethod('qcount', 'GEVAQuantiles', function(object) object@qcount)

