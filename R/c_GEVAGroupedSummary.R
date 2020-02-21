##########################
# GEVAGroupedSummary Class
# -----------------------
# 
# Table containing summary (S) and variation (V) of a GEVAInput and a list of GEVAGroupSet for data classification.
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

#' @include c_GEVASummary.R

#' @title GEVA Grouped Summary-Variation Table
#'
#' @description The \code{GEVAGroupedSummary} class inherits from \code{GEVASummary}.
#'
#' @slot groupsetlist TypedList containing elements of GEVAGroupSet-class
#'
#' @name GEVAGroupedSummary-class
#' @rdname GEVAGroupedSummary-class
#' @export
setClass('GEVAGroupedSummary',
         slots = c(
           groupsetlist = 'TypedList'
         ),
         contains = 'GEVASummary')


# INITIALIZE
setMethod('initialize', 'GEVAGroupedSummary',
          function(.Object, ...)
          {
            .Object = callNextMethod(.Object, ...)
            argls = initialize.class.args(...)
            if (!is.unitialized(argls$groupsetlist)) .Object@groupsetlist = argls$groupsetlist
            validObject(.Object)
            .Object
          })


# S4 METHODS
setMethod('groupsets', 'GEVAGroupedSummary', function(object) object@groupsetlist)

