
##########################
# SVTable Class
# -----------------------
# 
# Represents a table for summary (S) and variation (V)
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al

#' @include asserts.R
#' @include classhelpers.R
#' @include generics.R
#' @include usecasechecks.R
#' @include vectorhelpers.R
#' @include callhelpers.R
#' @include plotting.R
NULL

#' @title Summary-Variation Table
#'
#' @description The `SVTable` class stores a `matrix` composed by two columns: `S` (for *summary*) and `V` (for *variation*).
#' 
#' This class is inherited by [`GEVASummary-class`].
#'
#' @slot sv `matrix` composed by two columns: `S` (summary) and `V` (variation)
#' 
#' @note The matrix from `sv` slot can `numeric`, `character`, or any other supported type by `matrix`. The same slot from [`GEVASummary-class`], however, is always a numeric `matrix`.
#'
#' @aliases SVTable
#'
#' @declareS4class
setClass('SVTable',
         slots = c(
           sv = 'matrix'
         ))

# INITIALIZE
setMethod('initialize', 'SVTable',
          function(.Object, ...)
          {
            argls = initialize.class.args(...)
            sv = argls$sv
            assert.names.equal(sv, colnames = c('S', 'V'))
            .Object@sv = as.matrix(sv)
            validObject(.Object)
            .Object
          }
          )


# CONSTRUCTOR
#' @category Constructor

#' @s4method Creates a SVTable from the vectors `S` and `V`
svtable <- function(S, V, row.names=NULL)
{
  if (missing(V))
  {
    if (missing(S)) S = NA
    V = rep(NA, length(S))
  } else if (missing(S)) V = rep(NA, length(V))
  assert.dim(S, length=length(V))
  if (is.null(row.names))
  {
    row.names = if (is.named(S)) names(S) else if (is.named(V)) row.names = names(V) else as.character(1:length(S))
  }
  sv = as.matrix(data.frame(S=S, V=V, row.names=row.names))
  new('SVTable', sv=sv)
}

# SHOW
#' @s4method
setMethod('show', 'SVTable',
          function(object)
          {
            catline('Summary-Variation Table (SVTable-class)')
            catline('Columns: S (summary), V (variation)')
            catline('Rows (%d): %s', nrow(object), fmt.limit(rownames(object)))
          })

# PLOT
#' @s4method
setMethod('plot', c('SVTable', 'missing'),
          function(x, y, ...)
          {
            call.plot(sv(x), ...)
          })

# INDEXERS
#' @s4method
setMethod('[', c('SVTable', 'ANY', 'ANY', 'ANY'),
          function(x, i, j, ... , drop = TRUE)
          {
            sv(x)[i,j,drop=drop]
          })


# S4 Methods
#' @s4method
#' @s4accessor
setMethod('sv', 'SVTable', function(object) object@sv)

#' @category Dimension accessors

#' @s4method Gets the dimensions from the `sv` slot
setMethod('dim', 'SVTable', function(x) dim(sv(x)))

#' @s4method Gets a \code{list} with the row and column names from the `sv` slot. \cr Individual dimension names can also be accessed through \code{rownames} and \code{colnames}
setMethod('dimnames', 'SVTable', function(x) dimnames(sv(x)))

#' @s4method
setMethod('names', 'SVTable', function(x) colnames(sv(x)))

#' @s4method
setMethod('$', 'SVTable',
          function(x, name)
          {
            if (name %in% c('S', 's', 'summary')) return(summary(x))
            if (name %in% c('V', 'v', 'variation')) return(variation(x))
            NULL
          })

#' @s4method
setMethod('sv.data', 'SVTable', function(object) object)


# S3 Methods

#' @s3method
as.matrix.SVTable <- function(x, ...) sv(x)

#' @s3method
as.data.frame.SVTable <- function(x, ...) as.data.frame(sv(x))

#' @s3method
summary.SVTable <- function(object, ...) sv(object)[, 'S']

#' @s3method
variation.SVTable <- function(object, ...) sv(object)[, 'V']

#' @s3method
head.SVTable <- function(x, n = 6L, ...) head(sv(x), n=n, ...)

#' @s3method
is.na.SVTable <- function(x) is.na(sv(x))

#' @s3method
as.SVTable.matrix <- function(x, row.names=rownames(x), ...) if(all(c('S', 'V') %in% colnames(x))) svtable(x[,'S'], x[,'V'], row.names = row.names) else svtable(x[,1], x[,2], row.names = row.names)

#' @s3method
as.SVTable.data.frame <- function(x, row.names=rownames(x), ...) if(all(c('S', 'V') %in% names(x))) svtable(x$S, x$V, row.names=row.names) else svtable(x[,1], x[,2], row.names=row.names)

#' @s3method
as.SVTable.SVTable <- function(x, ...) x

#' @category Plotting

#' @s3method Draws the SV points in the plot
points.SVTable <- function(x, ...) call.plot(sv(x), ..., plotfn = points.default)

#' @s3method
with.SVTable <- function(data, expr, ...)
{
  dt = as.data.frame(data)
  eval(substitute(expr), dt, ...)
}

#' @s3method
format.SVTable <- function(x, ...) format(sv(x), ...)
