
##########################
# GEVAQuantiles Class
# -----------------------
# 
# Represents classification data for GEVA summaries separated by quantiles
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al


#' @include c_GEVAGroupSet.R
#' @include c_SVAttribute.R
#' @include usecasechecks.R
NULL

#' @title GEVA Quantiles Grouping Results
#'
#' @description The \code{GEVAQuantiles} class represents the results of a quantile detection analysis. For each probe/gene, there is a assigned quantile among the \emph{g} defined quantiles.
#' 
#' This class inherits from \code{\linkS4class{GEVAGroupSet}} and is inherited by \code{\linkS4class{GEVAQuantilesAdjusted}}.
#' 
#' @slot grouping \code{factor} (\emph{m} elements, \emph{g} levels), quantile assignment for each gene/probe
#' \cr (Inherited from \code{\linkS4class{GEVAGroupSet}})
#' @slot scores \code{numeric} vector (\emph{m} elements) with the assigned quantile scores for each gene/probe
#' \cr (Inherited from \code{\linkS4class{GEVAGroupSet}})
#' @slot ftable \code{data.frame} (\emph{m} lines) with additional quantile assignment features
#' \cr (Inherited from \code{\linkS4class{GEVAGroupSet}})
#' @slot centroids \code{numeric SVTable} (\emph{g} lines) with the S and V centroid coordinates for each quantile
#' \cr (Inherited from \code{\linkS4class{GEVAGroupSet}})
#' @slot offsets \code{numeric SVTable} (\emph{m} lines) with the S and V coordinate offsets each gene/probe from its quantile centroid
#' \cr (Inherited from \code{\linkS4class{GEVAGroupSet}})
#' @slot info \code{list} of additional information
#' \cr (Inherited from \code{\linkS4class{GEVAGroupSet}})
#' 
#' @slot svscores \code{numeric \linkS4class{SVTable}} (\emph{m} lines) with individual partial scores for the assigned quantiles
#' @slot qareasizes \code{numeric \linkS4class{SVTable}} (\emph{g} lines) with the S and V sizes for each quantile
#' @slot qindexes \code{integer \linkS4class{SVTable}} (\emph{g} lines) representing the position index to each quantile, in terms of summary and variation
#' @slot qcount integer attributes (\code{\linkS4class{SVIntAttribute}}) with the defined number of quantiles for the S and V axes
#' @slot qcutoff numeric attributes (\code{\linkS4class{SVNumAttribute}}) with the initial quantile cutoff in S and V, starting from the point zero
#'
#' @declareS4class
setClass('GEVAQuantiles',
         slots = c(
           svscores = 'SVTable',
           qareasizes = 'SVTable',
           qindexes = 'SVTable',
           qcount = 'SVIntAttribute',
           qcutoff = 'SVNumAttribute',
           qmethod = 'character'
         ), contains = 'GEVAGroupSet')


# INITIALIZE
setMethod('initialize', 'GEVAQuantiles',
          function(.Object, ...)
          {
            .Object = callNextMethod(.Object, ...)
            argls = initialize.class.args(...)
            qareasizes = argls$qareasizes
            qindexes = argls$qindexes
            .Object@qcutoff = argls$qcutoff
            svscores = argls$svscores
            assert.dim(svscores, nrow=length(scores(.Object)))
            assert.dim(qareasizes, dim=dim(.Object@centroids))
            assert.names.equal(qareasizes, rownames=rownames(qindexes))
            .Object@svscores = svscores
            .Object@qareasizes = qareasizes
            .Object@qindexes = qindexes
            qcount = get.initialized(argls$qcount, svattr(3L, 2L))
            check.quantiles.count(summary(qcount), variation(qcount))
            .Object@qcount = qcount
            validObject(.Object, complete = T)
            .Object
          }
          )

# SHOW
#' @s4method
setMethod('show', 'GEVAQuantiles',
          function(object)
          {
            catline('GEVA Quantiles (%s-class)', class(object)[1])
            catline('Quantiles (%d): %s', length(levels(object)), fmt.limit(levels(object)))
            catline('Scores (%d): %s', length(groups(object)), fmt.limit(scores(object)))
            if (length(infolist(object)) != 0) catline('Additional information (%d): %s', length(infolist(object)), fmt.limit(names(infolist(object))))
          })

# PLOT
#' @s4method
setMethod('plot', c('GEVAQuantiles', 'SVTable'),
          function(x, y, ...)
          {
            plotres = callNextMethod(x, y, ...)
            lines(x, ...)
            invisible(plotres)
          })

# INDEXERS
#' @s4method
setMethod('[', c('GEVAQuantiles', 'ANY', 'ANY', 'ANY'),
          function(x, i, j, ... , drop = TRUE)
          {
            sv.scores(x)[i,j,drop=drop]
          })

# S4 Methods

#' @methodsnote (See also the inherited methods from [`GEVAGroupSet-class`])

#' @s4method
#' @s4accessor
setMethod('qindexes', 'GEVAQuantiles', function(object) object@qindexes)

#' @s4method
#' @s4accessor
setMethod('qareasizes', 'GEVAQuantiles', function(object) object@qareasizes)



#' @s4method
#' @s4accessor
setMethod('qcount', 'GEVAQuantiles', function(object) object@qcount)

#' @s4method
#' @s4accessor svscores
setMethod('sv.scores', 'GEVAQuantiles', function(object) object@svscores)

#' @s4method
setMethod('cluster.method', 'GEVAQuantiles', function(object) 'quantiles')

#' @s4method
#' @s4accessor qmethod
setMethod('quantiles.method', 'GEVAQuantiles', function(object) object@qmethod)

#' @category Sub-slot accessors

#' @s4method Gets the unique quantile names
setMethod('quantiles', 'GEVAQuantiles', function(object) levels(object))

setMethod('classification.table', 'GEVAQuantiles',
          function(object)
          {
            ctable = callNextMethod(object)
            if (is.null(ctable)) return(get.quantiles.default.classification(object))
            ctable
          })
setMethod('classification.table<-', c('GEVAQuantiles', 'data.frame'),
          function(object, value)
          {
            infolist(object)$classification.table = value
            object
          })
          
#' @category Dimension accessors

#' @s4method
setMethod('dim', 'GEVAQuantiles', function(x) dim(sv.scores(x)))

# S3 Methods

#' @s3method Draws the quantile delimiter lines
lines.GEVAQuantiles <- function(x, ...)
{
  thres = infolist(x)$thresholds
  if (!is.null(thres) && is.list(thres))
  {
    ns = length(thres$S)
    nv = length(thres$V)
    if (ns >= 3L) abline(v=thres$S[c(-1, -ns)], lty=2, col='#33333344')
    if (nv >= 3L) abline(h=thres$V[c(-1, -nv)], lty=2, col='#33333344')
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

#' @s3method
as.expression.GEVAQuantiles <- function(x, sv, ...)
{
  parls = analysis.params(x)
  parls$cluster.method = NULL
  parls$sv = if (missing(sv))
    parse(text=sprintf("sv.data(%s)", deparse(substitute(x))))
  else
    substitute(sv)
  expr = function2expression(geva.quantiles,
                             args.list = parls,
                             ...)
  expr
}

#' @s3method
as.SVTable.GEVAQuantiles <- function(x, which=c('sv', 'offsets', 'centroids', 'qindexes'), ..., row.names=names(x))
{
  which = match.arg(which)
  if (which %in% c('sv', 'offsets', 'centroids'))
    return(as.SVTable.GEVAGroupSet(x, which=which, ...))
  if (which == 'qindexes')
  {
    minds = sv(qindexes(x))
    gs = groups(x)
    ginds = match(gs, rownames(minds))
    svinds = as.SVTable(qindexes(x)[ginds,,drop=FALSE], row.names=row.names, ...)
    return(svinds)
  }
}
