
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

setClass('SVCharacterAttribute',
         slots = c(
           S = 'character',
           V = 'character'
         ), contains = 'SVAttribute')

setClass('SVNumericAttribute',
         slots = c(
           S = 'numeric',
           V = 'numeric'
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

setMethod('initialize', 'SVCharacterAttribute',
          function(.Object, S, V, ...)
          {
            .Object = callNextMethod(.Object, S, V, ...)
            validObject(.Object)
            .Object
          })

setMethod('initialize', 'SVNumericAttribute',
          function(.Object, S, V, ...)
          {
            .Object = callNextMethod(.Object, S, V, ...)
            validObject(.Object)
            .Object
          })


# SHOW
setMethod('show', 'SVAttribute',
          function(object)
          {
            show(sv(object))
          })

# INDEXERS
setMethod('[', c('SVTable', 'ANY', 'ANY', 'ANY'),
          function(x, i, j, ... , drop = TRUE)
          {
            if (lenth(i) != 0)
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

setMethod('sv', 'SVAttribute', function(object) setNames(c(object@S, object@V), c('S', 'V')))

# S3 Methods
summary.SVTable <- function(object, ...) x@S
variation.SVTable <- function(object, ...) x@V
as.character.SVAttribute <- function(x, ...) c(summary(x), variation(x))
as.vector.SVAttribute <- function(x, ...) sv(x)


