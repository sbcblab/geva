
##########################
# GEVASummary Class
# -----------------------
# 
# Table containing summary (S) and variation (V) of a GEVAInput
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

#' @include c_SVTable.R

#' @title GEVA Summary-Variation Table
#'
#' @description The \code{GEVASummary} class inherits from \code{SVTable}.
#'
#' @slot inputdata GEVAInput-class with the data input
#' @slot info list with additional information
#' 
#'
#' @name GEVASummary-class
#' @rdname GEVASummary-class
#' @export
setClass('GEVASummary',
         slots = c(
           inputdata = 'GEVAInput',
           info = 'list'
         ),
         contains = 'SVTable')

# INITIALIZE
setMethod('initialize', 'GEVASummary',
          function(.Object, ...)
          {
            .Object = callNextMethod(.Object, ...)
            argls = initialize.class.args(...)
            .Object@inputdata = argls$inputdata
            assert.names.equal(.Object@sv, rownames = rownames(.Object@inputdata))
            .Object@info = argls$info
            validObject(.Object)
            .Object
          }
)

# SHOW
setMethod('show', 'GEVASummary',
          function(object)
          {
            catline('GEVA Summary-Variation Table (GEVASummary-class)')
            catline('Columns: S (summary), V (variation)')
            catline('Rows (%d): %s', nrow(object), fmt.limit(rownames(object)))
          })

# S4 Methods
setMethod('inputdata', 'GEVASummary', function(object) object@inputdata)
setMethod('inputvalues', 'GEVASummary', function(object) inputvalues(inputdata(object)))
setMethod('inputweights', 'GEVASummary', function(object) inputweights(inputdata(object)))
setMethod('probeattrs', 'GEVASummary', function(object) probeattrs(inputdata(object)))
setMethod('factors', 'GEVASummary', function(object) factors(inputdata(object)))

setMethod('infolist', c('GEVASummary', 'missing'),
          function(object, recursive=FALSE)
          {
            if (!recursive) return(object@info)
            infols = unlist(list(object@info, infolist(inputdata(object))), recursive = FALSE)
            infols
          })
setMethod('infolist<-', c('GEVASummary', 'list'), function(object, value) { object@info = value; object })



