
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
#' @slot resultstable data.frame with classification results for data points
#' @slot svdata GEVASummary object with summarized data
#' @slot quantdata GEVAQuantiles object with final quantiles definitions (e.g. adjusted quantiles)
#' @slot factoring data.frame with detailed factoring information
#' @slot classiftable data.frame used as reference to classify the data points
#'
#' @name GEVAResults-class
#' @rdname GEVAResults-class
#' @export
setClass('GEVAResults',
         slots = c(
           resultstable = 'data.frame',
           svdata = 'GEVASummary',
           quantdata = 'GEVAQuantiles',
           factoring = 'data.frame',
           classiftable = 'data.frame'
         ))

# INITIALIZE
setMethod('initialize', 'GEVAResults',
          function(.Object, ...)
          {
            argls = initialize.class.args(...)
            resultstable = argls$resultstable
            svdata = argls$svdata
            quantdata = argls$quantdata
            factoring = argls$factoring
            classiftable = argls$classiftable
            # TODO: Asserts of the input arguments
            .Object@resultstable = resultstable
            .Object@svdata = svdata
            .Object@quantdata = quantdata
            .Object@factoring = factoring
            .Object@classiftable = classiftable
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

# PLOT
setMethod('plot', c('GEVAResults', 'missing'), function(x, y, ...) plot(quantiles(x), ...))

# S4 Methods
setMethod('quantiles', 'GEVAResults', function(object) object@quantdata)
setMethod('results.table', 'GEVAResults', function(gres) gres@resultstable)
