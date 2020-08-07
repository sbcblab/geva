
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

#' @slot grouprels typed list of named factors representing external group relationships to the current quantiles
#'
#' @name GEVAQuantilesAdjusted-class
#' @rdname GEVAQuantiles-class
#' @export
setClass('GEVAQuantilesAdjusted',
         slots = c(
           grouprels = 'TypedList'
         ), contains = 'GEVAQuantiles')

# INITIALIZE
setMethod('initialize', 'GEVAQuantilesAdjusted',
          function(.Object, ...)
          {
            .Object = callNextMethod(.Object, ...)
            argls = initialize.class.args(...)
            grouprels = argls$grouprels
            elem.class(grouprels) = 'factor'
            check.typed.list.class(grouprels, 'factor')
            for (i in 1L:length(grouprels))
            {
              e = grouprels[[i]]
              assert.notempty(names(e), .posmsg = sprintf("element [[%d]] in 'grouprels' must be a non-empty named factor", i))
            }
            .Object@grouprels = grouprels
            validObject(.Object)
            .Object
          })

# SHOW
setMethod('show', 'GEVAQuantilesAdjusted',
          function(object)
          {
            callNextMethod(object)
            grels = as.character(group.rels(object))
            catline('Group Relationships (%d): %s', length(grels), fmt.limit(grels))
          })

# S4 Methods
setMethod('group.rels', 'GEVAQuantilesAdjusted', function(object) object@grouprels)
