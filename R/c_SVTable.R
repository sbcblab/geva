
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
#' @include vectorhelpers.R
#' @include callhelpers.R
#' @include plotting.R

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
            sv = argls$sv
            assert.names.equal(sv, colnames = c('S', 'V'))
            .Object@sv = as.matrix(sv)
            validObject(.Object)
            .Object
          }
          )


# CONSTRUCTOR
#' @export
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
setMethod('show', 'SVTable',
          function(object)
          {
            catline('Summary-Variation Table (SVTable-class)')
            catline('Columns: S (summary), V (variation)')
            catline('Rows (%d): %s', nrow(object), fmt.limit(rownames(object)))
          })

# PLOT
setMethod('plot', c('SVTable', 'missing'),
          function(x, y, ...)
          {
            call.plot(sv(x), ...)
            #argls = list(...)
            #validargs = c(names(graphics::par(no.readonly = TRUE)),
            #              names(formals(plot.default)))
            #argls = argls[!duplicated(names(argls)) & (names(argls) %in% validargs)]
            #argls$x = sv(x)
            #do.call(plot, argls)
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
setMethod('$', 'SVTable',
          function(x, name)
          {
            if (name %in% c('S', 's', 'summary')) return(summary(x))
            if (name %in% c('V', 'v', 'variation')) return(variation(x))
            NULL
          })



# S3 Methods
as.matrix.SVTable <- function(x, ...) sv(x)
as.data.frame.SVTable <- function(x, ...) as.data.frame(sv(x))
summary.SVTable <- function(object, ...) sv(object)[, 'S']
variation.SVTable <- function(object, ...) sv(object)[, 'V']
head.SVTable <- function(x, n = 6L, ...) head(sv(x), n=n, ...)

