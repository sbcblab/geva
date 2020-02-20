
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

#' @title Class Containing Input Data for GEVA
#'
#' @description The \code{GEVAInput} class contains the initial data for \code{GEVA} usage.
#' It stores numeric matrices of \emph{logFC} values from differential expression comparison results. Options for calculations and summarizing are also included.
#'
#' @slot values numeric matrix (m*n) of log-ratio values, usually \emph{logFC}
#' @slot weights numeric matrix (m*n) of weighted values. If not defined, all weight values are equal to 1
#' @slot factors factor representing the grouping of the n columns. If not defined, all factors are equal to NA
#' @slot probeattrs data.frame with m rows containing attribute columns associated to the probes
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
                   probeattrs = 'data.frame',
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
            .Object@probeattrs = get.initialized(argls$factors, default = data.frame(row.names=rownames(mvals)))
            
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
            catline('Object used as Input for GEVA (GEVAInput-class)')
            catline('Columns (%d): %s', ncol(object), fmt.limit(colnames(object)))
            catline('Rows (%d): %s', nrow(object), fmt.limit(rownames(object)))
            catline('Factors (%d): %s', length(levels(object)), fmt.limit(levels(object) ))
            catline('Attribute columns (%d): %s', ncol(probeattrs(object)), fmt.limit(colnames(probeattrs(object))))
          })

# INDEXERS
setMethod('[', c('GEVAInput', 'ANY', 'ANY', 'ANY'),
          function(x, i, j, ... , drop = TRUE)
          {
            inputvalues(x)[i,j,drop=drop]
          })

# S4 Methods
setMethod('inputvalues', 'GEVAInput', function(object) object@values)
setMethod('inputweights', 'GEVAInput', function(object) object@weights)
setMethod('probeattrs', 'GEVAInput', function(object) object@probeattrs)

setMethod('infolist', 'GEVAInput', function(object) object@info)
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
            object
          })

setMethod('factors<-', c('GEVAInput', 'character'), function(object, value) { factors(object) = as.factor(value); object })

setMethod('dim', 'GEVAInput', function(x) dim(inputvalues(x)))
setMethod('dimnames', 'GEVAInput', function(x) dimnames(inputvalues(x)))
setMethod('names', 'GEVAInput', function(x) colnames(inputvalues(x)))


# S3 Methods

levels.GEVAInput <- function(x) levels(factors(x))

as.array.GEVAInput <- function(x, ...)
{
  array(c(values=inputvalues(x), weights=inputweights(x)),
        dim = c(dim(x), 2L),
        dimnames = list(rownames(x), colnames(x), c('values', 'weights')))
}
