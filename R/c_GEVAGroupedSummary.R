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
            groupsetlist = as.typed.list(groupsetlist, elem.class='GEVAGroupSet')
            check.typed.list.class(groupsetlist, 'GEVAGroupSet')
            attr(groupsetlist, 'title') = 'List of GroupSets'
            .Object@groupsetlist = groupsetlist
            validObject(.Object)
            .Object
          })

# SHOW
setMethod('show', 'GEVAGroupedSummary', function(object)
{
  callNextMethod(object)
  gsets = groupsets(object)
  catline('Group Sets (%d): %s', length(gsets), fmt.limit(names(gsets)))
  # TODO: Show results contents
  
})

# PLOT
setMethod('plot', c('GEVAGroupedSummary', 'missing'),
          function(x, y, ..., include.groupsets = TRUE)
          {
            plotres = callNextMethod(x, y, ...)
            if (include.groupsets)
            {
              for (gg in groupsets(x))
              {
                col = color.values(gg, ...)
                if (isS3implemented('points', class(gg)))
                {
                  points(gg, col=col, ...)
                }
                if (isS3implemented('lines', class(gg)))
                {
                  lines(gg, col=col, ...)
                }
              } 
            }
            invisible(plotres)
          })

# S4 METHODS
setMethod('groupsets', 'GEVAGroupedSummary', function(object) object@groupsetlist)

setMethod('quantiles', 'GEVAGroupedSummary',
          function(object)
          {
            gsets = groupsets(object)
            if ('quantiles' %in% names(gsets)) return(gsets[['quantiles']])
            gq = first(gsets ~ inherits(g, 'GEVAQuantiles'))
            gq
          })

setMethod('cluster.method', 'GEVAGroupedSummary', function(object) sapply(cluster.method, groupsets(object)))

setMethod('analysis.params', 'GEVAGroupedSummary', function(gobject)
{
  parls = callNextMethod(gobject=gobject)
  parls = list.merge(parls,
                     do.call(list.merge, lapply(groupsets(gobject), infolist, field="analysis.params")) )
  parls
})

# S3 METHODS
as.matrix.GEVAGroupedSummary <- function(x, ...) sv(x)

as.expression.GEVAGroupedSummary <- function(x, ginput, ...)
{
  parls = analysis.params(x)
  ginputcall = if(missing(ginput))
  {
    sprintf("inputdata(%s)", deparse(substitute(x)))
  }
  else
  {
    deparse(substitute(ginput))
  }
  parls$sv = parse(text=sprintf("inputdata(%s)", deparse(substitute(x))))
  parls$grouped.return = TRUE
  expr = function2expression(geva.cluster,
                             args.list = parls,
                             ...)
  expr
}

lines.GEVAGroupedSummary <- function(x, ...)
{
  for (gg in groupsets(x))
  {
    col = color.values(gg, ...)
    if (isS3implemented('lines', class(gg)))
    {
      lines(gg, col=col, ...)
    }
  }
}

points.GEVAGroupedSummary <- function(x, ...)
{
  for (gg in groupsets(x))
  {
    col = color.values(gg, ...)
    if (isS3implemented('points', class(gg)))
    {
      points(gg, col=col, ...)
    }
  }
}
