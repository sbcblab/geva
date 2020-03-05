
##########################
# GEVACorrectedQuantiles Class
# -----------------------
# 
# Represents classification data for GEVA summaries separated by quantiles corrected using parameters from other grouping methods
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0


#' @include c_GEVAQuantiles.R

#' @slot grouprels named factor representing external group relationships to the current quantiles
#'
#' @name GEVAQuantilesAdjusted-class
#' @rdname GEVAQuantiles-class
#' @export
setClass('GEVAQuantilesAdjusted',
         slots = c(
           grouprels = 'factor'
         ), contains = 'GEVAQuantiles')

# INITIALIZE
setMethod('initialize', 'GEVAQuantiles',
          function(.Object, ...)
          {
            .Object = callNextMethod(.Object, ...)
            argls = initialize.class.args(...)
            grouprels = argls$grouprels
            assert.notempty(names(grouprels), .posmsg = sprintf("'grouprels' must be a non-empty named factor"))
            validObject(.Object)
            .Object
          })

# SHOW
setMethod('show', 'GEVAQuantiles',
          function(object)
          {
            callNextMethod(object)
            catline('Group Relationships (%d): %s', length(group.rels), fmt.limit(group.rels))
          })

# S4 Methods
setMethod('group.rels', 'GEVAQuantilesAdjusted', function(object) object@grouprels)
