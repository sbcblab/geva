
##########################
# GEVAInput Class
# -----------------------
# 
# Stores the input to be processed by further GEVA functions
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0


#' @include asserts.R
#' @include classhelpers.R
#' @include printhelpers.R
#' @include generics.R
#' @include usecasechecks.R

#' @title GEVA Input Data
#'
#' @description The \code{GEVAInput} class contains the initial data for \code{GEVA} usage.
#' It stores numeric matrices of \emph{logFC} values from differential expression comparison results. Options for calculations and summarizing are also included.
#'
#' @slot values numeric matrix (m*n) of log-ratio values, usually \emph{logFC}
#' @slot weights numeric matrix (m*n) of weighted values. If not defined, all weight values are equal to 1
#' @slot factors factor representing the grouping of the n columns. If not defined, all factors are equal to NA
#' @slot ftable data.frame with m rows containing attribute columns associated to the features (e.g. probes or genes)
#' @slot info list containg additional information included in the input
#'
#' @name GEVAInput-class
#' @rdname GEVAInput-class
#' @export
setClass('GEVAInput',
         slots = c(
           values = 'matrix',
           weights = 'matrix',
           factors='factor',
           ftable = 'data.frame',
           info = 'list'
         ))

# INITIALIZE
setMethod('initialize', 'GEVAInput',
          function(.Object, ...)
          {
            argls = initialize.class.args(...)
            mvals = argls$values
            
            .Object@values = mvals
            .Object@weights = get.initialized(argls$weights, default = mvals^0)
            .Object@factors = get.initialized(argls$factors, default = factor(rep(NA, ncol(mvals))))
            .Object@ftable = get.initialized(argls$ftable, default = data.frame(row.names=rownames(mvals)))
            
            assert.dim(.Object@weights, ncol=ncol(mvals), nrow=nrow(mvals))
            assert.dim(.Object@factors, length=ncol(mvals))
            
            check.suitable.factors(.Object@factors)
            
            validObject(.Object)
            .Object
          }
          )

# SHOW
setMethod('show', 'GEVAInput',
          function(object)
          {
            catline('GEVA Input Data')
            catline('Columns (%d): %s', ncol(object), fmt.limit(colnames(object)))
            catline('Rows (%d): %s', nrow(object), fmt.limit(rownames(object)))
            catline('Factors (%d): %s', length(levels(object)), fmt.limit(levels(object) ))
            catline('Attribute columns (%d): %s', ncol(featureTable(object)), fmt.limit(colnames(featureTable(object))))
          })

# INDEXERS
setMethod('[', c('GEVAInput', 'ANY', 'ANY', 'ANY'),
          function(x, i, j, ... , drop = TRUE)
          {
            mv = inputvalues(x)[i,j,drop=drop]
            if (drop) return(mv)
            if (missing(j)) j = 1L:ncol(x)
            mw = inputweights(x)[i,j,drop=drop]
            dft = featureTable(x)[i,,drop=drop]
            facts = factors(x)[j]
            ginput = promote.class(x, class(x), values=mv, weights=mw, factors=facts, ftable=dft)
            validObject(ginput)
            ginput
          })

# S4 Methods
setMethod('inputdata', 'GEVAInput', function(object) object)
setMethod('inputvalues', 'GEVAInput', function(object) object@values)
setMethod('inputweights', 'GEVAInput', function(object) object@weights)
setMethod('inputnames', 'GEVAInput', function(object) names(object@values))
setMethod('featureTable', 'GEVAInput', function(object) object@ftable)
setMethod('featureTable<-', c('GEVAInput', 'data.frame'),
          function(object, value)
          {
            assert.dim(value, nrow=nrow(object))
            rowseq = match(rownames(value), rownames(object))
            if (anyNA(rowseq))
              rownames(value) = rownames(object)
            else
              value = value[rowseq,,drop=FALSE]
            object@ftable = value
            object
          })


setMethod('infolist', c('GEVAInput', 'missing'), function(object, recursive=FALSE) object@info)
setMethod('infolist<-', c('GEVAInput', 'list'), function(object, value) { object@info = value; object })

setMethod('factors', 'GEVAInput', function(object) object@factors)
setMethod('factors<-', c('GEVAInput', 'factor'),
          function(object, value)
          {
            if (length(value) == 0)
            {
              object@factors = factor(rep(NA, ncol(object)))
            } else {
              assert.dim(value, length = ncol(object))
              check.suitable.factors(value)
              object@factors = value
            }
            validObject(object)
            object
          })

setMethod('factors<-', c('GEVAInput', 'character'), function(object, value) { factors(object) = as.factor(value); object })

setMethod('dim', 'GEVAInput', function(x) dim(inputvalues(x)))
setMethod('dimnames', 'GEVAInput', function(x) dimnames(inputvalues(x)))
setMethod('dimnames<-', c('GEVAInput', 'list'),
  function(x, value)
  {
    dimnames(x@values) = value
    dimnames(x@weights) = value
    rownames(x@ftable) = value[[1]]
    x
  })
setMethod('names', 'GEVAInput', function(x) colnames(inputvalues(x)))

setMethod('as.indexes', 'matrix', function(x) matrix(1:length(x), ncol=ncol(x), dimnames = dimnames(x)))

setMethod('as.indexes', 'GEVAInput', function(x) as.indexes(inputvalues(x)))



# S3 Methods

levels.GEVAInput <- function(x) levels(factors(x))

head.GEVAInput <- function(x, n=6L, ...) head(inputvalues(x), n=n, ...)

as.array.GEVAInput <- function(x, ...)
{
  array(c(values=inputvalues(x), weights=inputweights(x)),
        dim = c(dim(x), 2L),
        dimnames = list(rownames(x), colnames(x), c('values', 'weights')))
}

