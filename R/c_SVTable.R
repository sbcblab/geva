
##########################
# SVTable Class
# -----------------------
# 
# Represents a table for summary (S) and variation (V)
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0


#' @include asserts.R
#' @include classhelpers.R
#' @include generics.R
#' @include usecasechecks.R

#' @title Summary-Variation Table
#'
#' @description The \code{SVTable} class stores a \code{matrix} composed by two columns: \code{summary} and \code{variation}.
#'
#' @slot sv matrix composed by two columns: summary and variation
#'
#' @name SVTable-class
#' @rdname SVTable-class
#' @export
setClass('SVTable',
         slots = c(
           sv = 'matrix'
         ))

# INITIALIZE
setMethod('initialize', 'SVTable',
          function(.Object, ...)
          {
            argls = initialize.class.args(...)
            .Object@sv = argls$sv
            assert.names.equal(.Object@sv, colnames = c('S', 'V'))
            validObject(.Object)
            .Object
          }
          )

# SHOW
setMethod('show', 'SVTable',
          function(object)
          {
            catline('Summary-Variation Table (SVTable-class)')
            catline('Columns: S (summary), V (variation)')
            catline('Rows (%d): %s', nrow(object), fmt.limit(rownames(object)))
          })

# INDEXERS
setMethod('[', c('SVTable', 'ANY', 'ANY', 'ANY'),
          function(x, i, j, ... , drop = TRUE)
          {
            sv(x)[i,j,drop=drop]
          })


# S4 Methods
setMethod('sv', 'SVTable', function(object) object@sv)

setMethod('dim', 'SVTable', function(x) dim(sv(x)))
setMethod('dimnames', 'SVTable', function(x) dimnames(sv(x)))
setMethod('names', 'SVTable', function(x) colnames(sv(x)))


# S3 Methods
as.matrix.SVTable <- function(x, ...) sv(x)
as.data.frame.SVTable <- function(x, ...) as.data.frame(sv(x))
summary.SVTable <- function(object, ...) sv(x)[, 'S']
variation.SVTable <- function(object, ...) sv(x)[, 'V']

