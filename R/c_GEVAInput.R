
##########################
# GEVAInput Class
# -----------------------
# 
# Stores the input to be processed by further GEVA functions
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al


#' @include asserts.R
#' @include classhelpers.R
#' @include printhelpers.R
#' @include generics.R
#' @include usecasechecks.R
NULL

#' @title GEVA Input Data
#' 
#' @description The `GEVAInput` class contains the initial data for `GEVA` usage.
#' It stores numeric matrices of \emph{logFC} values from differential expression comparison results. Options for calculations and summarizing are also included.
#'
#' @slot values `numeric matrix` (\emph{m*n}) of log-ratio values, usually \emph{logFC}
#' @slot weights `numeric matrix` (\emph{m*n}) of weighted values. If not defined, all weight values are equal to 1
#' @slot factors `factor` (\emph{n} elements) representing the grouping of the n columns. If not defined, all factors are equal to `NA`
#' @slot ftable `data.frame` with \emph{m} rows containing attribute columns associated to the features (\emph{e.g.}, probes or genes)
#' @slot info `list` of supplementary information related to the input
#' 
#' @declareS4class
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
            
            validObject(.Object)
            .Object
          }
          )

# SHOW
#' @s4method
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
#' @s4method
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

# PLOT (Not intended to use)
#' @s4method
setMethod('plot', c('GEVAInput', 'missing'),
          function(x, y, ...)
          {
            warning("Input summarized using the default parameters")
            gsummary = geva.summarize(x, verbose=FALSE)
            plot(gsummary)
          })

# S4 Methods

#' @s4method
setMethod('inputdata', 'GEVAInput', function(object) object)

#' @s4method
#' @s4accessor values
setMethod('inputvalues', 'GEVAInput', function(object) object@values)

#' @s4method
#' @s4accessor weights
#' If `normalized` is `TRUE`, returns the weights matrix in the normalized form
setMethod('inputweights', c('GEVAInput', 'missing'), function(object, normalized=FALSE) object@weights)

#' @s4method
setMethod('inputweights', c('GEVAInput', 'logical'),
          function(object, normalized)
          {
            if (normalized) rows.normalize.weights(object@weights) else object@weights
          })


#' @s4method
#' @s4accessor ftable
setMethod('featureTable', 'GEVAInput', function(object) object@ftable)

#' @s4method
#' @s4accessor ftable
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

#' @s4method
#' @s4accessor info
#' If \code{field} is a \code{character}, returns the element with the matching name (\code{infolist(object)$<field name>})
setMethod('infolist', c('GEVAInput', 'missing'), function(object, field=NULL, ...) object@info)

#' @s4method
setMethod('infolist', c('GEVAInput', 'character'), function(object, field, ...) getElement(object@info, field))

#' @s4method
#' @s4accessor info
setMethod('infolist<-', c('GEVAInput', 'list'), function(object, value) { object@info = value; object })

#' @s4method
#' @s4accessor
setMethod('factors', 'GEVAInput', function(object) object@factors)

#' @s4method
#' @s4accessor
setMethod('factors<-', c('GEVAInput', 'factor'),
          function(object, value)
          {
            if (length(value) == 0)
            {
              object@factors = factor(rep(NA, ncol(object)))
            } else {
              assert.dim(value, length = ncol(object))
              check.suitable.factors(value, warn = FALSE, msg = FALSE)
              object@factors = value
            }
            validObject(object)
            object
          })

#' @s4method
setMethod('factors<-', c('GEVAInput', 'character'), function(object, value) { factors(object) = as.factor(value); object })

#' @category Dimension accessors

#' @s4method Gets the dimensions defined for both matrices in \code{values} and \code{weights} slots
setMethod('dim', 'GEVAInput', function(x) dim(inputvalues(x)))
#' @s4method Gets a \code{list} with the row and column names. \cr Individual dimension names can also be accessed through \code{rownames} and \code{colnames}
setMethod('dimnames', 'GEVAInput', function(x) dimnames(inputvalues(x)))
#' @s4method Sets the \code{list} with the row and column names. \cr Individual dimension names can also be set using \code{rownames<-} and \code{colnames<-}
setMethod('dimnames<-', c('GEVAInput', 'list'),
  function(x, value)
  {
    dimnames(x@values) = value
    dimnames(x@weights) = value
    rownames(x@ftable) = value[[1]]
    x
  })

#' @s4method Gets the input column names (same as `colnames(object)`)
setMethod('inputnames', 'GEVAInput', function(object) colnames(object@values))

#' @s4method
setMethod('names', 'GEVAInput', function(x) colnames(inputvalues(x)))

setMethod('as.indexes', 'matrix', function(x) matrix(1:length(x), ncol=ncol(x), dimnames = dimnames(x)))
setMethod('as.indexes', 'GEVAInput', function(x) as.indexes(inputvalues(x)))

#' @category Properties

#' @s4method Returns a `list` of analysis parameters passed to [`geva.merge.input`] or [`geva.read.tables`] to obtain this object
setMethod('analysis.params', 'GEVAInput', function(gobject)
{
  pars = infolist(gobject, 'input.params')
  if (is.null(pars)) pars = list()
  pars
})


# S3 Methods

#' @s3method
levels.GEVAInput <- function(x) levels(factors(x))

#' @s3method
head.GEVAInput <- function(x, n=6L, ...) head(inputvalues(x), n=n, ...)

#' @s3method
as.array.GEVAInput <- function(x, ...)
{
  array(c(values=inputvalues(x), weights=inputweights(x)),
        dim = c(dim(x), 2L),
        dimnames = list(rownames(x), colnames(x), c('values', 'weights')))
}

