
##########################
# GEVAGroupSet Class
# -----------------------
# 
# Represents classification data for GEVA summaries
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al


#' @include c_SVTable.R
NULL

#' @title GEVA Grouping Results
#'
#' @description The \code{GEVAGroupSet} class represents the classification of summarized values from a \code{\linkS4class{SVTable}}, where each gene/probe has one assigned group among \code{g} defined groups. This is an abstract class. Inherits the \code{\linkS4class{GEVACluster}} and \code{\linkS4class{GEVAQuantiles}} classes.
#'
#' @slot grouping \code{factor} (\emph{m} elements, \emph{g} levels) used to group the genes/probes
#' @slot scores \code{numeric} vector (\emph{m} elements) with the assigned grouping scores for each gene/probe
#' @slot ftable \code{data.frame} (\emph{m} lines) with additional grouping features
#' @slot centroids \code{numeric SVTable} (\emph{g} lines) with the S and V centroid coordinates for each group
#' @slot offsets \code{numeric SVTable} (\emph{m} lines) with the S and V coordinate offsets each gene/probe from its group centroid
#' @slot info \code{list} of additional information
#'
#' @declareS4class
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
#' @category Properties
#' @s4method
setMethod('show', 'GEVAGroupSet',
          function(object)
          {
            catline('GEVA Grouping Results (%s-class)', class(object)[1])
            catline('Groups (%d): %s', length(levels(object)), fmt.limit(levels(object)))
            catline('Scores (%d): %s', length(object), fmt.limit(scores(object)))
            if (length(infolist(object)) != 0) catline('Additional information (%d): %s', length(infolist(object)), fmt.limit(names(infolist(object))))
          })

# PLOT
#' @category Plotting
#' @s4method
setMethod('plot', c('GEVAGroupSet', 'missing'),
          function(x, y, ...)
          {
            svdata = sv.data(x)
            plot(x, y=svdata, ...)
          })

#' @s4method
setMethod('plot', c('GEVAGroupSet', 'SVTable'),
          function(x, y, ...)
          {
            svdata = y
            clrs = color.values(x, ...)
            defargs = list.merge(list(col=clrs),
                                 plotargs.sv.proportional(svdata))
            call.plot(svdata, ..., defargs=defargs)
          })

#' @s4method
setMethod('plot', c('SVTable', 'GEVAGroupSet'),
          function(x, y, ...) plot(y, x, ...))

#' @s4method Draws a SV-plot that highlights the grouped information.
#' \cr Can be combined with another `SVTable` or `GEVAGroupSet` given as the `y` argument to include additional graphical elements
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

#' @s4method
#' @s4accessor
setMethod('groups', 'GEVAGroupSet', function(object) object@grouping)

#' @s4method
#' @s4accessor
setMethod('centroids', 'GEVAGroupSet', function(object) object@centroids)

#' @s4method
#' @s4accessor
setMethod('offsets', 'GEVAGroupSet', function(object) object@offsets)


#' @s4method
#' @s4accessor
#' If `scores` is a group name, returns only the scores from this group
setMethod('scores', c('GEVAGroupSet', 'missing'), function(object, group) object@scores)

#' @s4method
setMethod('scores', c('GEVAGroupSet', 'character'), function(object, group) object@scores[groups(object) %in% group])

#' @s4method
#' @s4accessor info
#' If \code{field} is a \code{character}, returns the element with the matching name (\code{infolist(object)$<field name>})
setMethod('infolist', c('GEVAGroupSet', 'missing'), function(object, field, ...) object@info )

#' @s4method
setMethod('infolist', c('GEVAGroupSet', 'character'), function(object, field, ...) getElement(object@info, field) )

#' @s4method
#' @s4accessor info
setMethod('infolist<-', c('GEVAGroupSet', 'list'), function(object, value) { object@info = value; object })

#' @s4method
#' @s4accessor ftable
setMethod('featureTable', 'GEVAGroupSet', function(object) object@ftable)


#' @category Dimension accessors

#' @s4method Gets the assigned names by the classification and scores
setMethod('names', 'GEVAGroupSet', function(x) names(scores(x)))

#' @s4method Returns the number of rows in the `sv` slot
setMethod('length', 'GEVAGroupSet', function(x) length(scores(x)))


#' @category Sub-slot accessors

#' @s4method Returns the `numeric matrix` in the `SVTable` from `sv.data(object)`
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

#' @s4method Returns a `SVTable` with the source SV coordinates
setMethod('sv.data', 'GEVAGroupSet',
          function(object)
          {
            mcentoffs = sv(object)
            mcentoffs = svtable(mcentoffs[,1], mcentoffs[,2], names(scores(object)))
            mcentoffs
          })


#' @s4method Returns a `data.frame` of predicted classifications, if supported by this object
setMethod('classification.table', 'GEVAGroupSet', function(object) infolist(object)$classification.table  )
#' @s4method Stores the classification `data.frame` on this object
setMethod('classification.table<-', c('GEVAGroupSet', 'data.frame'),
          function(object, value)
          {
            infolist(object)$classification.table = value
            object
          })

#' @category Properties

#' @s4method Returns the option used as the `cluster.method` argument when calling `geva.cluster`
setMethod('cluster.method', 'GEVAGroupSet', function(object) NA_character_ )

#' @s4method Returns a `list` of analysis parameters passed to [`geva.cluster`] to obtain this object
setMethod('analysis.params', 'GEVAGroupSet', function(gobject)
  list.merge(list(cluster.method=cluster.method(gobject)), infolist(gobject, 'analysis.params')))
  
# S3 Methods

#' @category Alternative accessors

#' @s3method Returns the unique group names included in the group set.
#' \cr Equivalent to `levels(groups(x))`
levels.GEVAGroupSet <- function(x) levels(groups(x))


#' @category Plotting

#' @s3method Gets the colors associated to the grouped data points. If not present, generates random group colors.
#' \cr If `point.col` is a single `character` or an vector of the same length of data points, adjusts the color values to web RGBA
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
    if (length(clrs) != length(groups(x)) && length(clrs) == length(levels(x)))
      clrs = color.adjust(clrs[groups(x)])
    else
      clrs = color.adjust(clrs)
  }
  clrs
}

#' @s3method Draws the grouped points
points.GEVAGroupSet <- function(x, ...)
{
  call.plot(sv(x), ..., defargs = list(col=color.values(x, ...)), plotfn = points.default)
}

#' @category Conversion and coercion

#' @s3method Returns a `data.frame` with the `groups` and `scores` slots as columns
as.data.frame.GEVAGroupSet <- function(x, row.names=names(x), ...)
{
  data.frame(groups=groups(x), scores=scores(x), row.names = row.names)
}

#' @s3method Gets the expression that reproduces this `GEVAGroupSet` object, including function parameters used by `geva.cluster`. The `sv` argument is optional but can be specified to replace the source `SVTable`
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

#' @s3method Retrieves a `SVTable` based on the contents indicated by `which`. The accepted arguments are: `sv` for the source data; `offsets` for the `offsets` slots; and `centroids` for the `centroids` slot
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
