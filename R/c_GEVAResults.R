
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
#' @slot info list with additional information
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
           classiftable = 'data.frame',
           info = 'list'
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
            .Object@info = argls$info
            validObject(.Object)
            .Object
          }
          )


# SHOW
setMethod('show', 'GEVAResults',
          function(object)
          {
            catline('GEVA Results Table')
            topgs.table = top.genes(object)
            topgs = topgs.table$classification
            if (length(topgs) == 0L)
              catline("No significant results available")
            else
            {
              catline("Contains %d significant genes:", length(topgs))
              topcls = sort(unique(topgs))
              for (cl in topcls)
              {
                gsel = which(topgs == cl)
                catline('- %s (%d): %s', cl, length(gsel), fmt.limit(rownames(topgs.table)[gsel], limit = 3L))
              }
            }
          })

# PLOT
setMethod('plot', c('GEVAResults', 'missing'), function(x, y, ...) plot(quantiles(x), ...))

# INDEXERS
setMethod('[', c('GEVAResults', 'ANY', 'ANY', 'ANY'),
          function(x, i, j, ... , drop = TRUE)
          {
            mv = results.table(x)[i,j,drop=drop]
            mv
          })
setMethod('$', 'GEVAResults',
          function(x, name)
          {
            if (name %in% colnames(x)) return(results.table(x)[[name]])
            NULL
          })

# S4 Methods
setMethod('quantiles', 'GEVAResults', function(object) object@quantdata)
setMethod('results.table', 'GEVAResults', function(gres) gres@resultstable)

setMethod('sv.data', 'GEVAResults', function(object) object@svdata)
setMethod('sv', 'GEVAResults', function(object) sv(sv.data(object)))

setMethod('inputdata', 'GEVAResults', function(object) inputdata(object@svdata))
setMethod('inputvalues', 'GEVAResults', function(object) inputvalues(object@svdata))
setMethod('inputweights', c('GEVAResults', 'logical'), function(object, normalized) inputweights(object@svdata, normalized))
setMethod('inputweights', c('GEVAResults', 'missing'), function(object, normalized=FALSE) inputweights(object@svdata))

setMethod('infolist', c('GEVAResults', 'missing'), function(object, field, ...) object@info)
setMethod('infolist', c('GEVAResults', 'character'), function(object, field, ...) getElement(object@info, field) )

setMethod('featureTable', 'GEVAResults', function(object) featureTable(inputdata(object)))

setMethod('dim', 'GEVAResults', function(x) dim(results.table(x)))
setMethod('dimnames', 'GEVAResults', function(x) dimnames(results.table(x)))
setMethod('names', 'GEVAResults', function(x) colnames(results.table(x)))


setMethod('analysis.params', 'GEVAResults', function(gobject)
{
  parls = list.merge(analysis.params(sv.data(gobject)), analysis.params(quantiles(gobject)), infolist(gobject, 'analysis.params') )
  parls
})

# S3 Methods

levels.GEVAResults <- function(x) levels(inputdata(x))
with.GEVAResults <- function(data, expr, ...)
{
  dtres = results.table(data)
  eval(substitute(expr), dtres, ...)
}

points.GEVAResults <- function(x, which, ..., classif)
{
  
  if (missing(which))
  {
    if (!missing(classif))
    {
      which = top.genes(x, classif = classif, names.only = TRUE)
    }
    else
    {
      which = 1L:nrow(x)
    }
  }
  dt = sv(x)[which,,drop=FALSE]
  points.default(dt, ...)
}

as.expression.GEVAResults <- function(x, gsummary, gquants, ...)
{
  parls = list()
  parls$gsummary = if (missing(gsummary))
    parse(text=sprintf("sv.data(%s)", deparse(substitute(x))))
  else
    substitute(gsummary)
  parls$gquants = if (missing(gquants))
    parse(text=sprintf("quantiles(%s)", deparse(substitute(x))))
  else
    substitute(gquants)
  
  
  
  
  parls = list.merge(parls, infolist(x, 'analysis.params'))
  expr = function2expression(geva.finalize,
                             args.list = parls,
                             ...)
  expr
}


# Specific methods
top.genes <- function(gevaresults, classif=c('consistent', 'factor-dependent', 'factor-specific'),
                      which.spec=levels(gevaresults), add.cols=NULL, ..., names.only=FALSE)
{
  classif = match.arg(classif, c(call.default.arg(classif), 'basal', 'sparse'), several.ok = TRUE)
  classif = unique(classif)
  dtres = results.table(gevaresults)
  sel.genes = dtres$classification %in% classif
  if (length(which.spec) != 0L && length(which.spec) != length(levels(dtres$specific.factor)))
  {
    sel.spec = dtres$classification %in% 'factor-specific'
    sel.genes = sel.genes & (!sel.spec | (dtres$specific.factor %in% which.spec))
  }
  if (names.only)
    return(rownames(dtres)[sel.genes])
  dt = dtres[sel.genes,,drop=FALSE]
  add.cols = na.replace(add.cols)
  if (!is.null(add.cols))
  {
    dtfeats = featureTable(gevaresults)
    if (ncol(dtfeats) != 0)
    {
      if (is.numeric(add.cols))
        add.cols = na.replace(colnames(dtfeats)[add.cols])
      add.cols = unique(na.replace(as.character(add.cols)))
      sel.cols = add.cols %in% colnames(dtfeats)
      if (any(sel.cols))
        dt = cbind(dtfeats[sel.genes,add.cols[sel.cols],drop=FALSE], dt)
      if ('INDEX' %in% add.cols && !('INDEX' %in% colnames(dtfeats)))
        dt = cbind(data.frame(INDEX=which(sel.genes), row.names=rownames(dt)), dt)
    }
  }
  dt
}


