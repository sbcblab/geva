
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
#' @note The matrix from `sv` slot can be `numeric`, `character`, or any other supported type by `matrix`. The same slot from [`GEVASummary-class`], however, is always a numeric `matrix`.
#'
#' @aliases SVTable
#'
#' @examples
#' ## Creates a SV-table where:
#' # - S has elements from 1 to 10; and
#' # - V has elements from 10 to 1
#' svtab <- svtable(seq.int(1, 10), seq.int(10, 1))
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
    row.names = if (is.named(S)) names(S) else if (is.named(V)) row.names = names(V) else as.character(seq_along(S))
  }
  sv = as.matrix(data.frame(S=S, V=V, row.names=row.names))
  new('SVTable', sv=sv)
}

# SHOW
#' @category Properties
#' @s4method
setMethod('show', 'SVTable',
          function(object)
          {
            catline('Summary-Variation Table (SVTable-class)')
            catline('Columns: S (summary), V (variation)')
            catline('Rows (%d): %s', nrow(object), fmt.limit(rownames(object)))
          })

# INDEXERS
#' @s4method
setMethod('[', c('SVTable', 'ANY', 'ANY', 'ANY'),
          function(x, i, j, ... , drop = TRUE)
          {
            sv(x)[i,j,drop=drop]
          })

#' @s4method
setMethod('$', 'SVTable',
          function(x, name)
          {
            if (name %in% c('S', 's', 'summary')) return(summary(x))
            if (name %in% c('V', 'v', 'variation')) return(variation(x))
            NULL
          })

# PLOT
#' @category Plotting
#' @s4method Draws a SV-plot. The horizontal axis is for *summary* (S) and the vertical axis is for *variation* (V)
setMethod('plot', c('SVTable', 'missing'),
          function(x, y, ...)
          {
            call.plot(sv(x), ...)
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

#' @s4method Returns the number of rows in the `sv` slot
setMethod('length', 'SVTable', function(x) nrow(sv(x)))

#' @s4method Always returns `c('S', 'V')`
setMethod('names', 'SVTable', function(x) colnames(sv(x)))

#' @category Alternative accessors

#' @s4method Equivalent to returning this object itself
setMethod('sv.data', 'SVTable', function(object) object)


# S3 Methods


#' @category Alternative accessors

#' @s3method Returns the `S` column
summary.SVTable <- function(object, ...) sv(object)[, 'S']

#' @s3method Returns the `V` column
variation.SVTable <- function(object, ...) sv(object)[, 'V']

#' @category Subsetting

#' @s3method Returns the first parts of the matrix contents
head.SVTable <- function(x, n = 6L, ...) head(sv(x), n=n, ...)

#' @s3method Returns the last parts of the matrix contents
tail.SVTable <- function(x, n = 6L, ...) tail(sv(x), n=n, ...)

#' @category Validation

#' @s3method
is.na.SVTable <- function(x) is.na(sv(x))

#' @category Conversion and coercion

#' @s3method
as.matrix.SVTable <- function(x, ...) sv(x)

#' @s3method
as.data.frame.SVTable <- function(x, ...) as.data.frame(sv(x))

#' @s3method
as.SVTable.matrix <- function(x, row.names=rownames(x), ...) if(all(c('S', 'V') %in% colnames(x))) svtable(x[,'S'], x[,'V'], row.names = row.names) else svtable(x[,1], x[,2], row.names = row.names)

#' @s3method
as.SVTable.data.frame <- function(x, row.names=rownames(x), ...) if(all(c('S', 'V') %in% names(x))) svtable(x$S, x$V, row.names=row.names) else svtable(x[,1], x[,2], row.names=row.names)

#' @s3method Returns the same object
as.SVTable.SVTable <- function(x, ...) x

#' @category Plotting

#' @s3method Draws the SV points in the plot
points.SVTable <- function(x, ...) call.plot(sv(x), ..., plotfn = points.default)

#' @category Formatting and evaluation

#' @s3method
with.SVTable <- function(data, expr, ...)
{
  dt = as.data.frame(data)
  eval(substitute(expr), dt, ...)
}

#' @s3method
format.SVTable <- function(x, ...) format(sv(x), ...)
