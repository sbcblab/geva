
##########################
# TypedList Class
# -----------------------
# 
# List whose elements have share a common class
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al

#' @include generics.R
#' @include asserts.R
#' @include callhelpers.R
#' @include vectorhelpers.R
NULL

#' @title Type-strict List (TypedList-class)
#'
#' @description List containing elements of the same class or inheritance.
#'
#' @slot .Data `list` of internal contents. Elements must match or inherit a common class
#' \cr (Inherited from `list`)
#' @slot elem.class `character` representing the class related to the elements
#'
#' @declareS4class
setClass('TypedList',
         slots = c(
           elem.class = 'character'
         ),
         contains = 'list')

# INITIALIZE
setMethod('initialize', 'TypedList',
          function(.Object, elem.class=NA_character_, ...)
          {
            if (...length() == 0)
            {
              .Object@elem.class = elem.class
              return(.Object)
            }
            assert.class(elem.class, class="character")
            assert.dim(elem.class, length=1L)
            argnms = call.dots.argnames(...)
            argls = list(...)
            .Object[1:...length()] = argls
            names(.Object) = argnms
            if (is.na(elem.class))
            {
              elem.class = class(...elt(1))[[1]]
            }
            cmatches = sapply(argls, is, class2 = elem.class)
            if (any(!cmatches))
            {
              fmism = which(!cmatches)[1]
              stop(sprintf("Element %s is not a %s", if (is.null(argnms)) fmism else sprintf("'%s' at index %d", argnms[fmism], fmism), elem.class))
            }
            .Object@elem.class = elem.class
            validObject(.Object)
            .Object
          }
          )

# DEFAULT CONSTRUCTOR
#' @category Constructors
#' @s4method Creates a TypedList from the elements in `...` derived from the class `elem.class`
typed.list <- function(..., elem.class=NA_character_) new('TypedList', elem.class=elem.class, ...)


# S4 METHODS

#' @s4method
#' @s4accessor
setMethod('elem.class', 'TypedList', function(typedlist) typedlist@elem.class)

#' @s4method
#' @s4accessor
setMethod('elem.class<-', c(typedlist='TypedList', value='character'),
          function(typedlist, value)
          {
            assert.notempty(value, .posmsg = "a valid class name must be specified")
            Class = value[1]
            if (is.na(elem.class(typedlist)))
            {
              typedlist@elem.class = Class
              if (length(typedlist) == 0L)
                return(typedlist)
            }
            if (elem.class(typedlist) %in% Class && all(sapply(typedlist, is, class2 = Class))) return(typedlist)
            args = as.list(typedlist)
            assert.notempty(value)
            args$elem.class = Class
            typedlist = do.call('typed.list', args = args)
            typedlist
          })

#' @category Properties
#' @s4method
setMethod('show', 'TypedList',
          function(object)
          {
            title = attr(object, 'title')
            if (is.null(title)) title = sprintf('TypedList<%s>', elem.class(object))
            catline(title)
            show(object[1:length(object), drop=TRUE])
            invisible(object)
          })

#' @s4method
setMethod('[', c('TypedList', 'ANY', 'missing', 'missing'),
          function (x, i, j, ..., drop = FALSE)
          {
            elems = callNextMethod(x=x, i=i, drop=drop, ...)
            if (drop) return(elems)
            return(do.call('typed.list', args=elems))
          })

#' @s4method Sets a value to this list. The `value` argument must be compatible to the current list type
setMethod('[<-', c(x='TypedList', i='character', j='missing', value='ANY'),
          function (x, i, j, ..., value)
          {
            if (is.na(elem.class(x)))
              elem.class(x) = class(value)
            assert.class(value, is=elem.class(x))
            x[[i]] = value
            x
          })

# S3 Methods

#' @category Conversion and coercion

#' @s3method
as.list.TypedList <- function(x, ...) x[1:length(x), drop=TRUE]

#' @s3method Converts a vector to a `TypedList`
as.typed.list.vector <- function(x, elem.class=NA_character_) do.call('typed.list', list.merge(as.list(x), list(elem.class=elem.class)))

#' @s3method Converts a `list` to a `TypedList` if its elements inherit the same type
as.typed.list.list <- function(x, elem.class=NA_character_) do.call('typed.list', list.merge(x, list(elem.class=elem.class)))

#' @s3method Coerces a `TypedList` to support the inherited class indicated by `elem.class`
as.typed.list.TypedList <- function(x, elem.class=NA_character_)
{
  if (is.na(elem.class)) return(x)
  elem.class(x) = elem.class
  x
}
