##########################
# GEVAGroupedSummary Class
# -----------------------
# 
# Table containing summary (S) and variation (V) of a GEVAInput and a list of GEVAGroupSet for data classification.
# 
# ########################
# Written by Nunes et al

#' @include c_GEVASummary.R
#' @include c_GEVAGroupSet.R
#' @include c_TypedList.R
#' @include linq.R
NULL

#' @title GEVA Grouped Summary-Variation Table
#'
#' @description The \code{GEVAGroupedSummary} class inherits the \code{\linkS4class{GEVASummary}} class and includes group analysis data (\emph{e.g.}, clustering and quantile detection).
#'
#'
#' @slot sv \code{numeric matrix} composed by two columns: \code{S} (summary) and \code{V} (variation)
#' \cr (Inherited from \code{\linkS4class{SVTable}})
#' @slot inputdata GEVAInput-class with the data input
#' \cr (Inherited from \code{\linkS4class{GEVASummary}})
#' @slot sv.method Names of the statistical methods used to summarize data
#' \cr (Inherited from \code{\linkS4class{GEVASummary}})
#' @slot info list with additional information
#' \cr (Inherited from \code{\linkS4class{GEVASummary}})
#'
#' @slot groupsetlist \code{\linkS4class{TypedList}} of \code{\linkS4class{GEVAGroupSet}} objects
#'
#' @declareS4class
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
#' @category Properties
#' @s4method
setMethod('show', 'GEVAGroupedSummary', function(object)
{
  callNextMethod(object)
  gsets = groupsets(object)
  catline('Group Sets (%d): %s', length(gsets), fmt.limit(names(gsets)))
  # TODO: Show results contents
  
})

# PLOT
#' @category Plotting
#' @s4method Draws a SV-plot. The horizontal axis is for *summary* (S) and the vertical axis is for *variation* (V).
#' \cr In addition, highlights the included group sets
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

#' @methodsnote (See also the inherited methods from [`GEVASummary-class`])

#' @s4method
#' @s4accessor groupsetlist
setMethod('groupsets', 'GEVAGroupedSummary', function(object) object@groupsetlist)

#' @s4method
setMethod('groupsets<-', c('GEVAGroupedSummary', 'GEVAGroupSet'), function(object, value)
{
  argnm = deparse(substitute(value))[[1]]
  ggs = groupsets(object)
  ggs[[argnm]] = value
  object@groupsetlist = ggs
  object
})

#' @category Sub-slot accessors

#' @s4method Gets the [`GEVAQuantiles-class`], or `NULL` if not present
setMethod('quantiles', 'GEVAGroupedSummary',
          function(object)
          {
            gsets = groupsets(object)
            if ('quantiles' %in% names(gsets)) return(gsets[['quantiles']])
            gq = first(gsets ~ inherits(g, 'GEVAQuantiles'))
            gq
          })

#' @s4method Gets a `character` vector listing the `cluster.method` from each group set
setMethod('cluster.method', 'GEVAGroupedSummary', function(object) vapply(groupsets(object), cluster.method, ''))

#' @category Properties

#' @s4method Returns a `list` of analysis parameters passed to [`geva.cluster`] to obtain this object
setMethod('analysis.params', 'GEVAGroupedSummary', function(gobject)
{
  parls = callNextMethod(gobject=gobject)
  parls = list.merge(parls,
                     do.call(list.merge, lapply(groupsets(gobject), infolist, field="analysis.params")) )
  parls
})

# S3 METHODS

#' @category Conversion and coercion

#' @s3method
as.matrix.GEVAGroupedSummary <- function(x, ...) sv(x)

#' @s3method
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

#' @category Plotting

#' @s3method Draws delimiters within quantiles and convex hulls around the clustered points
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

#' @s3method
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
