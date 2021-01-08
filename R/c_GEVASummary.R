
##########################
# GEVASummary Class
# -----------------------
# 
# Table containing summary (S) and variation (V) of a GEVAInput
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

#' @include c_SVTable.R
#' @include c_GEVAInput.R
#' @include c_SVAttribute.R
#' @include c_TypedList.R

#' @title GEVA Summary-Variation Table
#'
#' @description The \code{GEVASummary} class represents the calculation results for summary and variation from a \code{GEVAInput}.
#' This class inherits from \code{SVTable}.
#'
#' @slot inputdata GEVAInput-class with the data input
#' @slot sv.method Names of the statistical methods used to summarize data
#' @slot info list with additional information
#' 
#'
#' @name GEVASummary-class
#' @rdname GEVASummary-class
#' @export
setClass('GEVASummary',
         slots = c(
           inputdata = 'GEVAInput',
           sv.method = 'SVChrAttribute',
           info = 'list'
         ),
         contains = 'SVTable')

# INITIALIZE
setMethod('initialize', 'GEVASummary',
          function(.Object, ...)
          {
            .Object = callNextMethod(.Object, ...)
            argls = initialize.class.args(...)
            sv = .Object@sv
            if (!is.numeric(sv)) stop("'sv' must be a numeric matrix")
            .Object@inputdata = argls$inputdata
            assert.names.equal(sv, rownames = rownames(.Object@inputdata))
            .Object@sv.method = argls$sv.method
            .Object@info = argls$info
            validObject(.Object)
            .Object
          }
)

# SHOW
setMethod('show', 'GEVASummary',
          function(object)
          {
            catline('GEVA Summary-Variation Table (%s-class)', class(object))
            catline('Columns: S (summary), V (variation)')
            catline('Rows (%d): %s', nrow(object), fmt.limit(rownames(object)))
            srng = range(summary(object))
            vrng = range(variation(object))
            catline('Summary range: %.2f to %.2f', srng[1], srng[2])
            catline('Variation range: %.2f to %.2f', vrng[1], vrng[2])
          })

# PLOT
setMethod('plot', c('GEVASummary', 'missing'),
          function(x, y, ...)
          {
            svmethname = sv.method(x)
            defargs = list.merge(list(xlab = sprintf('Summary (%s)', svmethname$S),
                                      ylab = sprintf('Variation (%s)', svmethname$V)),
                                 plotargs.sv.proportional(x))
            call.plot(sv(x), ..., defargs=defargs)
          })

# S4 Methods
setMethod('inputdata', 'GEVASummary', function(object) object@inputdata)
setMethod('inputvalues', 'GEVASummary', function(object) inputvalues(inputdata(object)))
setMethod('inputweights', c('GEVASummary', 'logical'), function(object, normalized) inputweights(inputdata(object), normalized))
setMethod('inputweights', c('GEVASummary', 'missing'), function(object, normalized=FALSE) inputweights(inputdata(object)))

setMethod('inputnames', 'GEVASummary', function(object) names(inputdata(object)))
setMethod('featureTable', 'GEVASummary', function(object) featureTable(inputdata(object)))
setMethod('factors', 'GEVASummary', function(object) factors(inputdata(object)))

setMethod('factors<-', c('GEVASummary', 'factor'),
          function(object, value)
          {
            inpdt = inputdata(object)
            factors(inpdt) = value
            object@inputdata = inpdt
            object
          })

setMethod('factors<-', c('GEVASummary', 'character'), function(object, value) { factors(object) = as.factor(value); object })

setMethod('infolist', c('GEVASummary', 'missing'),
          function(object, field=NULL, recursive = FALSE, ...)
          {
            if (!recursive) return(object@info)
            infols = unlist(list(object@info, infolist(inputdata(object))), recursive = FALSE)
            infols
          })
setMethod('infolist<-', c('GEVASummary', 'list'), function(object, value) { object@info = value; object })

setMethod('featureTable', 'GEVASummary', function(object) featureTable(inputdata(object)))

setMethod('sv.method', 'GEVASummary', function(gevasummary) gevasummary@sv.method)

setMethod('quantiles', 'GEVASummary', function(object) geva.quantiles(object))

setMethod('groupsets', 'GEVASummary', function(object) typed.list(elem.class = 'GEVAGroupSet') )
setMethod('groupsets<-', c('GEVASummary', 'TypedList'), function(object, value)
{
  gs2 = promote.class(object, 'GEVAGroupedSummary', groupsetlist=value)
  gs2
})

setMethod('analysis.params', 'GEVASummary', function(gobject)
{
  svmets = sv.method(gobject)
  list(summary.method=svmets$S,
       variation.method=svmets$V)
})


# S3 Methods
get.summary.method.GEVASummary <- function(gevasummary) get.summary.method(sv.method(gevasummary)$S)
get.variation.method.GEVASummary <- function(gevasummary) get.variation.method(sv.method(gevasummary)$V)

as.matrix.GEVASummary <- function(x, ...) sv(x)

as.expression.GEVASummary <- function(x, ginput, ...)
{
  parls = analysis.params(x)
  parls$ginput = if (missing(ginput))
    parse(text=sprintf("inputdata(%s)", deparse(substitute(x))))
  else
    substitute(ginput)
  expr = function2expression(geva.summarize,
                             args.list = parls,
                             ...)
  expr
}
