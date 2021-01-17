
##########################
# GEVACorrectedQuantiles Class
# -----------------------
# 
# Represents classification data for GEVA summaries separated by quantiles corrected using parameters from other grouping methods
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al


#' @include c_GEVAQuantiles.R
NULL

#' @title GEVA Adjusted Quantiles Results
#' 
#' @description The \code{GEVAQuantilesAdjusted} class represents the results of a quantile detection analysis with adjusted assignments based on relationships with other \code{GEVAGroupSet} objects. For each probe/gene, there is a assigned quantile among the \emph{g} defined quantiles.
#' 
#' This class inherits from \code{\linkS4class{GEVAQuantiles}}.
#' 
#' @slot grouping \code{factor} (\emph{m} elements, \emph{g} levels), quantile assignment for each gene/probe
#' \cr (Inherited from \code{\linkS4class{GEVAGroupSet}})
#' @slot scores \code{numeric} vector (\emph{m} elements) with the assigned quantile scores for each gene/probe
#' \cr (Inherited from \code{\linkS4class{GEVAGroupSet}})
#' @slot ftable \code{data.frame} (\emph{m} lines) with additional quantile assignment data
#' \cr (Inherited from \code{\linkS4class{GEVAGroupSet}})
#' @slot centroids \code{numeric SVTable} (\emph{g} lines) with the S and V centroid coordinates for each quantile
#' \cr (Inherited from \code{\linkS4class{GEVAGroupSet}})
#' @slot offsets \code{numeric SVTable} (\emph{m} lines) with the S and V coordinate offsets each gene/probe from its quantile centroid
#' \cr (Inherited from \code{\linkS4class{GEVAGroupSet}})
#' @slot info \code{list} of additional information
#' \cr (Inherited from \code{\linkS4class{GEVAGroupSet}})
#' @slot svscores \code{numeric \linkS4class{SVTable}} (\emph{m} lines) with individual partial scores for the assigned quantiles
#' \cr (Inherited from \code{\linkS4class{GEVAQuantiles}})
#' @slot qareasizes \code{numeric \linkS4class{SVTable}} (\emph{g} lines) with the S and V sizes for each quantile
#' \cr (Inherited from \code{\linkS4class{GEVAQuantiles}})
#' @slot qindexes \code{integer \linkS4class{SVTable}} (\emph{g} lines) representing the position index to each quantile, in terms of summary and variation
#' \cr (Inherited from \code{\linkS4class{GEVAQuantiles}})
#' @slot qcount integer attributes (\code{\linkS4class{SVIntAttribute}}) with the defined number of quantiles for the S and V axes
#' \cr (Inherited from \code{\linkS4class{GEVAQuantiles}})
#' @slot qcutoff numeric attributes (\code{\linkS4class{SVNumAttribute}}) with the initial quantile cutoff in S and V, starting from the point zero
#' \cr (Inherited from \code{\linkS4class{GEVAQuantiles}})
#' 
#' @slot grouprels \code{\linkS4class{TypedList}} of \code{named factor} elements representing external group relationships to the current quantiles
#'
#' @declareS4class
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
#' @s4method
setMethod('show', 'GEVAQuantilesAdjusted',
          function(object)
          {
            callNextMethod(object)
            grels = as.character(group.rels(object))
            catline('Group Relationships (%d): %s', length(grels), fmt.limit(grels))
          })

# S4 Methods

#' @methodsnote (See also the inherited methods from [`GEVAQuantiles-class`] and [`GEVAGroupSet-class`])

#' @s4method
#' @s4accessor grouprels
setMethod('group.rels', 'GEVAQuantilesAdjusted', function(object) object@grouprels)
