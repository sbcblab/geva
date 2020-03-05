
##########################
# SVAttribute Class
# -----------------------
# 
# Represents two attribute fields, one for summary and other for variation
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0


#' @include asserts.R
#' @include generics.R

#' @title Summary-Variation Attribute Field
#'
#' @description This simple S4 class stores two character slots representing attribute fields for summary and variation.
#'
#' @slot S character or numeric of length one
#' @slot V character or numeric of length one
#'
#' @name SVAttribute-class
#' @rdname SVAttribute-class
#' @export
setClass('SVAttribute', representation('VIRTUAL'))

setClass('SVChrAttribute',
         slots = c(
           S = 'character',
           V = 'character'
         ), contains = 'SVAttribute')

setClass('SVNumAttribute',
         slots = c(
           S = 'numeric',
           V = 'numeric'
         ), contains = 'SVAttribute')

setClass('SVIntAttribute',
         slots = c(
           S = 'integer',
           V = 'integer'
         ), contains = 'SVAttribute')

# INITIALIZE
setMethod('initialize', 'SVAttribute',
          function(.Object, S, V, ...)
          {
            assert.dim(S, length=1)
            assert.dim(V, length=1)
            .Object@S = S
            .Object@V = V
            validObject(.Object)
            .Object
          })

setMethod('initialize', 'SVChrAttribute', function(.Object, S, V, ...) callNextMethod(.Object, S, V, ...))

setMethod('initialize', 'SVNumAttribute', function(.Object, S, V, ...) callNextMethod(.Object, S, V, ...))

setMethod('initialize', 'SVIntAttribute', function(.Object, S, V, ...) callNextMethod(.Object, S, V, ...))

# SHOW
setMethod('show', 'SVAttribute',
          function(object)
          {
            show(sv(object))
          })

# INDEXERS
setMethod('[', c('SVAttribute', 'ANY', 'ANY', 'ANY'),
          function(x, i, j, ... , drop = TRUE)
          {
            if (length(i) != 0)
            {
              if (length(i) > 1) return(sv(x)[i])
              if (i == 1 || i %in% c('s', 'S')) return(summary(x))
              if (i == 2 || i %in% c('v', 'V')) return(variation(x))  
            }
            NA_character_
          })

# S4 Methods
setMethod('dim', 'SVAttribute', function(x) NULL)
setMethod('names', 'SVAttribute', function(x) c('S', 'V') )
setMethod('$', 'SVAttribute', function(x, name) x[name] )

setMethod('sv', 'SVAttribute', function(object) setNames(c(object@S, object@V), c('S', 'V')))

setMethod('svattr', c(S='character', V='character'), function(S, V) new('SVChrAttribute', S=S, V=V))
setMethod('svattr', c(S='numeric', V='numeric'), function(S, V) new('SVNumAttribute', S=as.numeric(S), V=as.numeric(V)))
setMethod('svattr', c(S='integer', V='integer'), function(S, V) new('SVIntAttribute', S=S, V=V))

# S3 Methods
summary.SVAttribute <- function(object, ...) object@S
variation.SVAttribute <- function(object, ...) object@V
as.character.SVAttribute <- function(x, ...) c(summary(x), variation(x))
as.vector.SVAttribute <- function(x, ...) sv(x)


