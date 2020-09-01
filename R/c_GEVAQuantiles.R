
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
#' @slot qareasizes SVTable with nominal area sizes of the quantiles in terms of summary and variation
#' @slot qindexes SVTable with indexes representing the magnitude index of summary and variation
#' @slot qcount SVIntAttribute with number of quantiles for summary and variation
#' @slot qcutoff SVNumAttribute with the initial quantile cut-offs, starting from zero
#'
#' @name GEVAQuantiles-class
#' @rdname GEVAQuantiles-class
#' @export
setClass('GEVAQuantiles',
         slots = c(
           svscores = 'SVTable',
           qareasizes = 'SVTable',
           qindexes = 'SVTable',
           qcount = 'SVIntAttribute',
           qcutoff = 'SVNumAttribute',
           qmethod = 'character'
         ), contains = 'GEVAGroupSet')


# INITIALIZE
setMethod('initialize', 'GEVAQuantiles',
          function(.Object, ...)
          {
            .Object = callNextMethod(.Object, ...)
            argls = initialize.class.args(...)
            qareasizes = argls$qareasizes
            qindexes = argls$qindexes
            .Object@qcutoff = argls$qcutoff
            svscores = argls$svscores
            assert.dim(svscores, nrow=length(scores(.Object)))
            assert.dim(qareasizes, dim=dim(.Object@centroids))
            assert.names.equal(qareasizes, rownames=rownames(qindexes))
            .Object@svscores = svscores
            .Object@qareasizes = qareasizes
            .Object@qindexes = qindexes
            qcount = get.initialized(argls$qcount, svattr(3L, 2L))
            check.quantiles.count(summary(qcount), variation(qcount))
            .Object@qcount = qcount
            validObject(.Object, complete = T)
            .Object
          }
          )

# SHOW
setMethod('show', 'GEVAQuantiles',
          function(object)
          {
            catline('GEVA Quantiles (%s-class)', class(object)[1])
            catline('Quantiles (%d): %s', length(levels(object)), fmt.limit(levels(object)))
            catline('Scores (%d): %s', length(groups(object)), fmt.limit(scores(object)))
            if (length(infolist(object)) != 0) catline('Additional information (%d): %s', length(infolist(object)), fmt.limit(names(infolist(object))))
          })

# PLOT
setMethod('plot', c('GEVAQuantiles', 'SVTable'),
          function(x, y, ...)
          {
            plotres = callNextMethod(x, y, ...)
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

# INDEXERS
setMethod('[', c('GEVAQuantiles', 'ANY', 'ANY', 'ANY'),
          function(x, i, j, ... , drop = TRUE)
          {
            sv.scores(x)[i,j,drop=drop]
          })

# S4 Methods
setMethod('qindexes', 'GEVAQuantiles', function(object) object@qindexes)
setMethod('qareasizes', 'GEVAQuantiles', function(object) object@qareasizes)
setMethod('quantiles', 'GEVAQuantiles', function(object) levels(object))
setMethod('qcount', 'GEVAQuantiles', function(object) object@qcount)
setMethod('sv.scores', 'GEVAQuantiles', function(object) object@svscores)
setMethod('cluster.method', 'GEVAQuantiles', function(object) 'quantiles')
setMethod('quantiles.method', 'GEVAQuantiles', function(object) object@qmethod)
setMethod('dim', 'GEVAQuantiles', function(x) dim(sv.scores(x)))


setMethod('classification.table', 'GEVAQuantiles',
          function(object)
          {
            ctable = callNextMethod(object)
            if (is.null(ctable)) return(get.quantiles.default.classification(object))
            ctable
          })
setMethod('classification.table<-', c('GEVAQuantiles', 'data.frame'),
          function(object, value)
          {
            infolist(object)$classification.table = value
            object
          })
