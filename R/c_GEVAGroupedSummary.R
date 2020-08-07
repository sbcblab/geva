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
#' @include c_GEVAGroupSet.R
#' @include c_TypedList.R
#' @include linq.R

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
            groupsetlist = argls$groupsetlist
            assert.notempty(groupsetlist)
            elem.class(groupsetlist) = 'GEVAGroupSet'
            check.typed.list.class(groupsetlist, 'GEVAGroupSet')
            attr(groupsetlist, 'title') = 'List of GroupSets'
            .Object@groupsetlist = groupsetlist
            validObject(.Object)
            .Object
          })


# S4 METHODS
setMethod('groupsets', 'GEVAGroupedSummary', function(object) object@groupsetlist)
#setMethod('groupsets<-', 'GEVAGroupedSummary', function(object, value) new('GEVAGroupedSummary', inputdata=inputdata(object), sv.method=sv.method(object), info=infolist(object), groupsetlist=value))

setMethod('quantiles', 'GEVAGroupedSummary',
          function(object)
          {
            gsets = groupsets(object)
            if ('quantiles' %in% names(gsets)) return(gsets[['quantiles']])
            gq = first(gsets ~ inherits(g, 'GEVAQuantiles'))
            gq
          })

setMethod('cluster.method', 'GEVAGroupedSummary', function(object) sapply(cluster.method, groupsets(object)))
