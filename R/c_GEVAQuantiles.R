
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
#' @slot svscores SVTable with individual scores for summary and variation
#' @slot qindexes SVTable with indexes representing the magnitude of summary and variation
#' @slot qcount SVIntAttribute with number of quantiles for summary and variation
#' @slot qcutoff SVNumAttribute with the initial quantile cut-offs, starting from zero
#'
#' @name GEVAQuantiles-class
#' @rdname GEVAQuantiles-class
#' @export
setClass('GEVAQuantiles',
         slots = c(
           svscores = 'SVTable',
           qindexes = 'SVTable',
           qcount = 'SVIntAttribute',
           qcutoff = 'SVNumAttribute'
         ), contains = 'GEVAGroupSet')


# INITIALIZE
setMethod('initialize', 'GEVAQuantiles',
          function(.Object, ...)
          {
            .Object = callNextMethod(.Object, ...)
            argls = initialize.class.args(...)
            .Object@qindexes = argls$qindexes
            svscores = argls$svscores
            assert.dim(svscores, nrow=length(scores(.Object)))
            .Object@svscores = svscores
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

# PLOT
setMethod('plot', c('GEVAQuantiles', 'missing'),
          function(x, y, ...)
          {
            plotres = callNextMethod(x, ...)
            thres = infolist(x)$thresholds
            if (!is.null(thres) && is.list(thres))
            {
              ns = length(thres$S)
              nv = length(thres$V)
              if (ns >= 3L) abline(v=thres$S[c(-1, -ns)], lty=2, col='#33333344')
              if (nv >= 3L) abline(h=thres$V[c(-1, -nv)], lty=2, col='#33333344')
            }
            invisible(plotres)
          })


# S4 Methods
setMethod('qindexes', 'GEVAQuantiles', function(object) object@qindexes)
setMethod('quantiles', 'GEVAQuantiles', function(object) levels(object))
setMethod('qcount', 'GEVAQuantiles', function(object) object@qcount)
setMethod('sv.scores', 'GEVAQuantiles', function(object) object@svscores)

