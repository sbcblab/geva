
##########################
# GEVAResults Class
# -----------------------
# 
# Contains the final results from GEVA analyses.
# 
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al

#' @include c_GEVAGroupedSummary.R
#' @include c_GEVAQuantiles.R
NULL

#' @title GEVA Results Table
#'
#' @description The \code{GEVAResults} class contains the final results from GEVA analyses. It represents the results of multiple statistical approaches from summary/variation data, clustering, quantile detection, and factor analysis (if applicable).
#'
#' @slot resultstable \code{data.frame} (\emph{m} lines) with classification results for the genes/probes
#' @slot svdata \code{\linkS4class{GEVASummary}} used as input
#' @slot quantdata \code{\linkS4class{GEVAQuantiles}} or \code{\linkS4class{GEVAQuantilesAdjusted}} with the final quantile assignments for the summarized data
#' @slot factoring \code{data.frame} (\emph{m} lines) with detailed results for the factor analyses, such as p-values for each factor. If there was no factor analysis, this slot is \code{NULL} or empty
#' @slot classiftable \code{data.frame} used as reference for the final classification
#' @slot info \code{list} of supplementary information
#'
#' @declareS4class
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
#' @s4method
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
#' @s4method
setMethod('plot', c('GEVAResults', 'missing'), function(x, y, ...) plot(quantiles(x), ...))

# INDEXERS
#' @s4method
setMethod('[', c('GEVAResults', 'ANY', 'ANY', 'ANY'),
          function(x, i, j, ... , drop = TRUE)
          {
            mv = results.table(x)[i,j,drop=drop]
            mv
          })
#' @s4method
setMethod('$', 'GEVAResults',
          function(x, name)
          {
            if (name %in% colnames(x)) return(results.table(x)[[name]])
            NULL
          })

# S4 Methods

#' @s4method
#' @s4accessor quantdata
setMethod('quantiles', 'GEVAResults', function(object) object@quantdata)

#' @s4method
#' @s4accessor resultstable
setMethod('results.table', 'GEVAResults', function(gres) gres@resultstable)

#' @s4method
#' @s4accessor svdata
setMethod('sv.data', 'GEVAResults', function(object) object@svdata)

#' @s4method
setMethod('sv', 'GEVAResults', function(object) sv(sv.data(object)))

#' @s4method
#' @s4accessor info
setMethod('infolist', c('GEVAResults', 'missing'), function(object, field, ...) object@info)
#' @s4method
#' @s4accessor info
setMethod('infolist', c('GEVAResults', 'character'), function(object, field, ...) getElement(object@info, field) )

#' @category Sub-slot accessors

#' @s4method Returns the internal [`GEVAInput-class`]
setMethod('inputdata', 'GEVAResults', function(object) inputdata(object@svdata))
#' @s4method Returns the values `matrix` from the internal [`GEVAInput-class`]
setMethod('inputvalues', 'GEVAResults', function(object) inputvalues(object@svdata))
#' @s4method Returns the weights `matrix` from the internal [`GEVAInput-class`]
setMethod('inputweights', c('GEVAResults', 'logical'), function(object, normalized) inputweights(object@svdata, normalized))
setMethod('inputweights', c('GEVAResults', 'missing'), function(object, normalized=FALSE) inputweights(object@svdata))

#' @s4method Returns the features `data.frame` from the internal [`GEVAInput-class`]
setMethod('featureTable', 'GEVAResults', function(object) featureTable(inputdata(object)))


#' @category Dimension accessors

#' @s4method Gets the dimensions from the `results.table` slot
setMethod('dim', 'GEVAResults', function(x) dim(results.table(x)))
#' @s4method Gets a \code{list} with the row and column names from the `results.table` slot. \cr Individual dimension names can also be accessed through \code{rownames} and \code{colnames}
setMethod('dimnames', 'GEVAResults', function(x) dimnames(results.table(x)))
#' @s4method
setMethod('names', 'GEVAResults', function(x) colnames(results.table(x)))

#' @category Properties

#' @s4method Returns a `list` of analysis parameters passed to [`geva.finalize`] or [`geva.quick`] to obtain this object
setMethod('analysis.params', 'GEVAResults', function(gobject)
{
  parls = list.merge(analysis.params(sv.data(gobject)), analysis.params(quantiles(gobject)), infolist(gobject, 'analysis.params') )
  parls
})

# S3 Methods

#' @category Sub-slot accessors

#' @s3method Returns the factors used in factor analysis, if present
levels.GEVAResults <- function(x) levels(inputdata(x))

with.GEVAResults <- function(data, expr, ...)
{
  dtres = results.table(data)
  eval(substitute(expr), dtres, ...)
}

#' @category Plotting

#' @s3method Draws the results points.
#' \cr If `which` (`character` vector) is given, plots only the matching genes/probes.
#' \cr If `classif` (`character` vector) is given, plots only points with the matching classification
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

#' @s3method
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
#' @title Top Results from GEVA
#' 
#' @description Extracts the genes with a relevant classification according to the GEVA results.
#' @param gevaresults a [`GEVAResults-class`] object
#' @param classif `character` vector, filters the returned genes by their final classification. Possible options are `"similar"`, `"factor-dependent"`, `"factor-specific"`, `"sparse"`, and `"basal"`. Multiple options can be combined
#' @param which.spec `factor`, filters the specific factors to be returned
#' @param add.cols `character` vector with column names from the feature table (accessed by `featureTable(gevaresults)`). The matching columns will be added to the returned table
#' @param ... optional arguments (not used in this version)
#' @param names.only `logical`, set to `TRUE` to return only the table row names
#' 
#' @return
#' If `names.only` is `FALSE` (the default), returns a subset of the `resultstable` slot (`data.frame`) from the `gevaresults` that includes only the filtered genes according to the function parameters.
#' 
#' Otherwise, if `names.only` is `TRUE`, returns only the row names (`character` vector) of this table subset.
#'
#' @examples
#' ## Basic usage with a random generated input
#' ginput <- geva.ideal.example() # Generates a random input example
#' gresults <- geva.quick(ginput) # Performs the entire analysis (default parameters)
#' 
#' # Gets a table that includes all the top genes
#' dtgenes <- top.genes(gresults) # Gets the top genes table
#' head(dtgenes)                  # Prints the first results
#' 
#' # Appends the "Symbol" column to the results table
#' dtgenes <- top.genes(gresults, add.cols="Symbol")
#' head(dtgenes)                  # Prints the first results
#' 
#' # Appends all feature columns to the results table
#' dtgenes <- top.genes(gresults, add.cols=names(featureTable(gresults)))
#' head(dtgenes)                  # Prints the first results
#' 
#' # Gets only the factor-specific genes
#' dtgenes <- top.genes(gresults, "factor-specific")
#' head(dtgenes)                  # Prints the first results
#' 
#' # Gets only the factor-specific genes for "Cond_1" factor (if any)
#' dtgenes <- top.genes(gresults, "factor-specific", "Cond_1")
#' head(dtgenes)                  # Prints the first results
#' 

#'
#' @rdname top.genes
#' @export
top.genes <- function(gevaresults, classif=c('similar', 'factor-dependent', 'factor-specific'),
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


