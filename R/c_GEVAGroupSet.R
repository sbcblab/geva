
##########################
# GEVAGroupSet Class
# -----------------------
# 
# Represents classification data for GEVA summaries
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0


#' @include c_SVTable.R

#' @title GEVA Grouping Results
#'
#' @description The \code{GEVAGroupSet} class stores a factor that confers the grouping of summary/variation (SV) data.
#'
#' @slot grouping factor used to group genes or probes
#' @slot scores numeric vector with grouping scores
#' @slot ftable data.frame with additional information related to the grouped features (genes or probes)
#' @slot centroids SVTable describing the position of centroids
#' @slot offsets SVTable with distances between the centroids and SV data
#' @slot info list with additional information
#'
#' @name GEVAGroupSet-class
#' @rdname GEVAGroupSet-class
#' @export
setClass('GEVAGroupSet',
         slots = c(
           grouping = 'factor',
           scores = 'numeric',
           ftable = 'data.frame',
           centroids = 'SVTable',
           offsets = 'SVTable',
           info = 'list'
         ))

# INITIALIZE
setMethod('initialize', 'GEVAGroupSet',
          function(.Object, ...)
          {
            argls = initialize.class.args(...)
            grouping = as.factor(argls$grouping)
            centroids = argls$centroids
            offsets = argls$offsets
            scores = argls$scores
            assert.notempty(names(scores))
            assert.dim(scores, length=length(grouping))
            ftable = get.initialized(argls$ftable, data.frame(row.names = names(scores)))
            assert.included(levels(grouping), rownames(centroids), "centroid groups")
            assert.dim(offsets, nrow=length(grouping))
            assert.names.equal(ftable, rownames=names(scores))
            .Object@ftable = ftable
            .Object@grouping = grouping
            .Object@scores = scores
            .Object@centroids = centroids
            .Object@offsets = offsets
            .Object@info = argls$info
            validObject(.Object)
            .Object
          }
          )

# SHOW
setMethod('show', 'GEVAGroupSet',
          function(object)
          {
            catline('GEVA Grouping Results (%s-class)', class(object)[1])
            catline('Groups (%d): %s', length(levels(object)), fmt.limit(levels(object)))
            catline('Scores (%d): %s', length(object), fmt.limit(scores(object)))
            if (length(infolist(object)) != 0) catline('Additional information (%d): %s', length(infolist(object)), fmt.limit(names(infolist(object))))
          })

# S4 Methods
setMethod('groups', 'GEVAGroupSet', function(object) object@grouping)

setMethod('scores', c('GEVAGroupSet', 'missing'), function(object, group) object@scores)
setMethod('scores', c('GEVAGroupSet', 'character'), function(object, group) object@scores[groups(object) %in% group])

setMethod('infolist', c('GEVAGroupSet', 'missing'), function(object, recursive) object@info )
setMethod('infolist<-', c('GEVAGroupSet', 'list'), function(object, value) { object@info = value; object })

setMethod('featureTable', 'GEVAGroupSet', function(object) object@ftable)

# S3 Methods
levels.GEVAGroupSet <- function(x) levels(groups(x))

