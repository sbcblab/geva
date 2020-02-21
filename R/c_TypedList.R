
##########################
# TypedList Class
# -----------------------
# 
# List whose elements have share a common class
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

#' @include generics.R
#' @include callhelpers.R

#' @title Type-strict List (TypedList-class)
#'
#' @description List containing elements of the same class.
#'
#' @slot elem.class character representing the class related to the elements
#' 
#'
#' @name TypedList-class
#' @rdname TypedList-class
#' @export
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
typed.list <- function(elem.class=NA_character_, ...) new('TypedList', elem.class=elem.class, ...)


# S4 METHODS
setMethod('elem.class', 'TypedList', function(object) object@elem.class)

setMethod('show', 'TypedList',
          function(object)
          {
            catline('TypedList<%s>', elem.class(object))
            show(object[1:length(object), drop=TRUE])
            invisible(object)
          })

setMethod('[', c('TypedList', 'ANY', 'missing', 'missing'),
          function (x, i, j, ..., drop = FALSE)
          {
            elems = callNextMethod(x=x, i=i, drop=drop, ...)
            if (drop) return(elems)
            return(do.call('typed.list', args=elems))
          })
