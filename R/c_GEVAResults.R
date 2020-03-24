
##########################
# GEVAResults Class
# -----------------------
# 
# Contains the final results from GEVA analyses.
# 
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

#' @include c_GEVAGroupedSummary.R
#' @include c_GEVAQuantiles.R

#' @title GEVA Results Table
#'
#' @description The \code{GEVAResults} class contains the final results from GEVA analyses. It represents the results of multiple statistical approaches from summary/variation data, groupings and quantiles.
#'
#' @slot sv matrix composed by two columns: summary and variation
#'
#' @name GEVAResults-class
#' @rdname GEVAResults-class
#' @export
setClass('GEVAResults',
         slots = c(
           resultstable = 'data.frame',
           svdata = 'GEVASummary',
           adjquantdata = 'GEVAQuantiles',
           factoring = 'data.frame'
         ))

# INITIALIZE
setMethod('initialize', 'GEVAResults',
          function(.Object, ...)
          {
            argls = initialize.class.args(...)
            resultstable = argls$resultstable
            svdata = argls$svdata
            adjquantdata = argls$adjquantdata
            factoring = argls$factoring
            # TODO: Asserts of the input arguments
            .Object@resultstable = resultstable
            .Object@svdata = svdata
            .Object@adjquantdata = adjquantdata
            .Object@factoring = factoring
            validObject(.Object)
            .Object
          }
          )


# SHOW
setMethod('show', 'GEVAResults',
          function(object)
          {
            catline('GEVA Results Table')
            # TODO: Show results contents
            
          })

# S4 Methods
setMethod('quantiles', 'GEVAResults', function(object) object@adjquantdata)

