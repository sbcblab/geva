
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
#' @include asserts.R
#' @include callhelpers.R
#' @include vectorhelpers.R

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
typed.list <- function(..., elem.class=NA_character_) new('TypedList', elem.class=elem.class, ...)


# S4 METHODS
setMethod('elem.class', 'TypedList', function(typedlist) typedlist@elem.class)
setMethod('elem.class<-', c(typedlist='TypedList', value='character'),
          function(typedlist, value)
          {
            assert.notempty(value, .posmsg = "a valid class name must be specified")
            Class = value[1]
            if (elem.class(typedlist) %in% Class && all(sapply(typedlist, is, class2 = Class))) return(typedlist)
            args = as.list(typedlist)
            assert.notempty(value)
            args$elem.class = Class
            typedlist = do.call('typed.list', args = args)
            typedlist
          })

setMethod('show', 'TypedList',
          function(object)
          {
            title = attr(object, 'title')
            if (is.null(title)) title = sprintf('TypedList<%s>', elem.class(object))
            catline(title)
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

# S3 Methods
as.list.TypedList <- function(x, ...) x[1:length(x), drop=TRUE]
as.typed.list.vector <- function(x, elem.class=NA_character_) do.call('typed.list', list.merge(as.list(x), list(elem.class=elem.class)))
as.typed.list.list <- function(x, elem.class=NA_character_) do.call('typed.list', list.merge(x, list(elem.class=elem.class)))
as.typed.list.TypedList <- function(x, elem.class=NA_character_)
{
  if (is.na(elem.class)) return(x)
  elem.class(x) = elem.class
  x
}

