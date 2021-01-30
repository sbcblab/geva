
##########################
# SVAttribute Class
# -----------------------
# 
# Represents two attribute fields, one for summary and other for variation
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al

#' @include asserts.R
#' @include generics.R
NULL

#' @title Summary-Variation Attribute Field
#'
#' @description This S4 class stores two character slots representing attribute fields for summary and variation. The \code{SVAttribute} class is abstract and must be instantiated as \code{SVChrAttribute} (for \code{character}), \code{SVNumAttribute} (for \code{numeric}), or \code{SVIntAttribute} (for \code{integer}).
#'
#' @slot S either \code{character} or \code{numeric} or \code{integer} of length one
#' @slot V either \code{character} or \code{numeric} or \code{integer} of length one
#' 
#' @note The slots \code{S} and \code{V} must be of the same class (either \code{character}, \code{numeric}, or \code{integer}).
#'
#' @aliases SVAttribute SVNumAttribute SVIntAttribute SVChrAttribute SVNumAttribute-class SVIntAttribute-class SVChrAttribute-class
#' @declareS4class
setClass('SVAttribute', contains='VIRTUAL')

#' @export
setClass('SVChrAttribute',
         slots = c(
           S = 'character',
           V = 'character'
         ), contains = 'SVAttribute')

#' @export
setClass('SVNumAttribute',
         slots = c(
           S = 'numeric',
           V = 'numeric'
         ), contains = 'SVAttribute')

#' @export
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
#' @category Properties
#' @s4method
setMethod('show', 'SVAttribute',
          function(object)
          {
            show(sv(object))
          })

# INDEXERS
#' @s4method Indexer to access the vector values. Only accepts `'S'` or `'V'` as `i` arguments
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

#' @s4method Queries the vector contents (equivalent to the indexer). Only accepts `$S` and `$V`
setMethod('$', 'SVAttribute', function(x, name) x[name] )

# S4 Methods

#' @category Dimension accessors

#' @s4method For internal use, always returns `NULL`
setMethod('dim', 'SVAttribute', function(x) NULL)

#' @s4method Returns the slot names (always `c('S', 'V')`)
setMethod('names', 'SVAttribute', function(x) c('S', 'V') )

#' @category Alternative accessors

#' @s4method Returns the contents as a named vector
setMethod('sv', 'SVAttribute', function(object) setNames(c(object@S, object@V), c('S', 'V')))

#' @category Constructors

#' @s4method
setMethod('svattr', c(S='character', V='character'), function(S, V) new('SVChrAttribute', S=S, V=V))

#' @s4method
setMethod('svattr', c(S='numeric', V='numeric'), function(S, V) new('SVNumAttribute', S=as.numeric(S), V=as.numeric(V)))

#' @param S the *summary* value
#' @param V the *variation* value
#' @s4method Creates a new `SVAttribute`
setMethod('svattr', c(S='integer', V='integer'), function(S, V) new('SVIntAttribute', S=S, V=V))

#' @s4method For internal use. Returns the equivalent object
setMethod('sv.data', 'SVAttribute', function(object) object)

# S3 Methods

#' @category Alternative accessors

#' @s3method Returns the contents from `S` slot
#' @s4accessor S
summary.SVAttribute <- function(object, ...) object@S

#' @s3method Returns the contents from `S` slot
#' @s4accessor V
variation.SVAttribute <- function(object, ...) object@V

#' @category Conversion and coercion

#' @s3method
as.character.SVAttribute <- function(x, ...) c(summary(x), variation(x))

#' @s3method
as.vector.SVAttribute <- function(x, ...) sv(x)


