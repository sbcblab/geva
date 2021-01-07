
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
         ), contains = 'VIRTUAL')

# INITIALIZE
setMethod('initialize', 'GEVAGroupSet',
          function(.Object, ...)
          {
            argls = initialize.class.args(...)
            grouping = argls$grouping
            if (!is.factor(grouping)) grouping = as.factor(grouping)
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

# PLOT
setMethod('plot', c('GEVAGroupSet', 'missing'),
          function(x, y, ...)
          {
            svdata = sv.data(x)
            plot(x, y=svdata, ...)
          })

setMethod('plot', c('GEVAGroupSet', 'SVTable'),
          function(x, y, ...)
          {
            svdata = y
            clrs = color.values(x, ...)
            defargs = list.merge(list(col=clrs),
                                 plotargs.sv.proportional(svdata))
            call.plot(svdata, ..., defargs=defargs)
          })

setMethod('plot', c('SVTable', 'GEVAGroupSet'),
          function(x, y, ...) plot(y, x, ...))

setMethod('plot', c('GEVAGroupSet', 'GEVAGroupSet'),
          function(x, y, ...) 
          {
            plotres = plot(x, ...)
            if (isS3implemented('lines', class(y)))
            {
              lines(y, ...)
            }
            invisible(plotres)
          })

# S4 Methods
setMethod('groups', 'GEVAGroupSet', function(object) object@grouping)
setMethod('centroids', 'GEVAGroupSet', function(object) object@centroids)
setMethod('offsets', 'GEVAGroupSet', function(object) object@offsets)

setMethod('names', 'GEVAGroupSet', function(x) names(scores(x)))

setMethod('scores', c('GEVAGroupSet', 'missing'), function(object, group) object@scores)
setMethod('scores', c('GEVAGroupSet', 'character'), function(object, group) object@scores[groups(object) %in% group])


setMethod('infolist', c('GEVAGroupSet', 'missing'), function(object, field, ...) object@info )
setMethod('infolist', c('GEVAGroupSet', 'character'), function(object, field, ...) getElement(object@info, field) )
setMethod('infolist<-', c('GEVAGroupSet', 'list'), function(object, value) { object@info = value; object })

setMethod('featureTable', 'GEVAGroupSet', function(object) object@ftable)

setMethod('sv.data', 'GEVAGroupSet',
          function(object)
          {
            mcentoffs = sv(object)
            mcentoffs = svtable(mcentoffs[,1], mcentoffs[,2], names(scores(object)))
            mcentoffs
          })

setMethod('sv', 'GEVAGroupSet',
          function(object) 
          {
            mcents = sv(centroids(object))
            gs = groups(object)
            sel.valid.gs = !is.na(gs)
            ginds = match(gs[sel.valid.gs], rownames(mcents))
            mcentoffs = sv(offsets(object))
            mcentoffs[sel.valid.gs, ] = mcentoffs[sel.valid.gs,,drop=FALSE] + mcents[ginds,,drop=FALSE]
            mcentoffs
          })

setMethod('cluster.method', 'GEVAGroupSet', function(object) NA_character_ )

setMethod('classification.table', 'GEVAGroupSet', function(object) infolist(object)$classification.table  )
setMethod('classification.table<-', c('GEVAGroupSet', 'data.frame'),
          function(object, value)
          {
            infolist(object)$classification.table = value
            object
          })

setMethod('analysis.params', 'GEVAGroupSet', function(gobject)
  list.merge(list(cluster.method=cluster.method(gobject)), infolist(gobject, 'analysis.params')))
  
# S3 Methods

levels.GEVAGroupSet <- function(x) levels(groups(x))

as.data.frame.GEVAGroupSet <- function(x, row.names=names(x), ...)
{
  data.frame(groups=groups(x), scores=scores(x), row.names = row.names)
}

color.values.GEVAGroupSet <- function(x, point.col=NULL, ...)
{
  if (!is.null(point.col))
  {
    clrs = point.col
    if (length(point.col) == 1L)
      clrs = rep(point.col, length(groups(x)))
    if (length(clrs) != length(groups(x)))
      stop("'point.col' (color vector) must be of the same length as the grouping from the given groupset")
    clrs = color.adjust(clrs, ...)
    return(clrs)
  }
  clrs = infolist(x)$colors
  if (is.null(clrs))
  {
    clrs = color.values.factor(x=groups(x), ...)
  } else {
    clrs = color.adjust(clrs[groups(x)])
  }
  clrs
}

points.GEVAGroupSet <- function(x, ...)
{
  call.plot(sv(x), ..., defargs = list(col=color.values(x, ...)), plotfn = points.default)
}

as.expression.GEVAGroupSet <- function(x, sv, ...)
{
  parls = analysis.params(x)
  parls$sv = if (missing(sv))
    parse(text=sprintf("sv.data(%s)", deparse(substitute(x))))
  else
    substitute(sv)
  expr = function2expression(geva.cluster,
                             args.list = parls,
                             ...)
  expr
}

as.SVTable.GEVAGroupSet <- function(x, which=c('sv', 'offsets', 'centroids'), ...)
{
  which = match.arg(which)
  if (which == 'sv') return(sv.data(x))
  if (which == 'offsets') return(offsets(x))
  if (which == 'centroids')
  {
    mcents = sv(centroids(x))
    gs = groups(x)
    sel.valid.gs = !is.na(gs)
    ginds = match(gs[sel.valid.gs], rownames(mcents))
    mcentoffs = sv(offsets(x))
    mcentoffs[sel.valid.gs, ] = mcents[ginds,,drop=FALSE]
    as.SVTable(mcentoffs, ...)
  }
}

# Helper methods

score.plot <- function(ggset, y, ..., clrs=NULL)
{
  assert.class(ggset, inherits='GEVAGroupSet')
  if (missing(y)) y = sv.data(ggset)
  
  if (is.null(clrs) || length(clrs) == 0L)
  {
    clrs = if (inherits(ggset, 'GEVAQuantiles')) '#0000FF' else '#FF0000'
  }
  plot(x=ggset, y=y, clrs=clrs, alpha=clamp(scores(ggset), min.value = 0, max.value = 1), ...)
}
