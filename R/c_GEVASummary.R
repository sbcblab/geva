
##########################
# GEVASummary Class
# -----------------------
# 
# Table containing summary (S) and variation (V) of a GEVAInput
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al

#' @include c_SVTable.R
#' @include c_GEVAInput.R
#' @include c_SVAttribute.R
#' @include c_TypedList.R
#' @include c_GEVAGroupSet.R
NULL

#' @title GEVA Summary-Variation Table
#'
#' @description The `GEVASummary` class represents the calculation results for summary and variation from a [`GEVAInput-class`].
#' 
#' This class inherits from [`SVTable`].
#'
#' @slot sv `numeric matrix` composed by two columns: `S` (summary) and `V` (variation)
#' \cr (Inherited from [`SVTable-class`])
#' @slot inputdata GEVAInput-class with the data input
#' @slot sv.method Names of the statistical methods used to summarize data
#' @slot info list with additional information
#' 
#' @declareS4class
setClass('GEVASummary',
         slots = c(
           inputdata = 'GEVAInput',
           sv.method = 'SVChrAttribute',
           info = 'list'
         ),
         contains = 'SVTable')

# INITIALIZE
setMethod('initialize', 'GEVASummary',
          function(.Object, ...)
          {
            .Object = callNextMethod(.Object, ...)
            argls = initialize.class.args(...)
            sv = .Object@sv
            if (!is.numeric(sv)) stop("'sv' must be a numeric matrix")
            .Object@inputdata = argls$inputdata
            assert.names.equal(sv, rownames = rownames(.Object@inputdata))
            .Object@sv.method = argls$sv.method
            .Object@info = argls$info
            validObject(.Object)
            .Object
          }
)

# SHOW
#' @category Properties
#' @s4method
setMethod('show', 'GEVASummary',
          function(object)
          {
            catline('GEVA Summary-Variation Table (%s-class)', class(object))
            catline('Columns: S (summary), V (variation)')
            catline('Rows (%d): %s', nrow(object), fmt.limit(rownames(object)))
            srng = range(summary(object))
            vrng = range(variation(object))
            catline('Summary range: %.2f to %.2f', srng[1], srng[2])
            catline('Variation range: %.2f to %.2f', vrng[1], vrng[2])
          })

# PLOT
#' @category Plotting
#' @s4method Draws a SV-plot. The horizontal axis is for *summary* (S) and the vertical axis is for *variation* (V)
setMethod('plot', c('GEVASummary', 'missing'),
          function(x, y, ...)
          {
            svmethname = sv.method(x)
            defargs = list.merge(list(xlab = sprintf('Summary (%s)', svmethname$S),
                                      ylab = sprintf('Variation (%s)', svmethname$V)),
                                 plotargs.sv.proportional(x))
            call.plot(sv(x), ..., defargs=defargs)
          })

# S4 Methods

#' @methodsnote (See also the inherited methods from [`SVTable-class`])

#' @s4method
#' @s4accessor
setMethod('inputdata', 'GEVASummary', function(object) object@inputdata)

#' @s4accessor
setMethod('sv.method', 'GEVASummary', function(gevasummary) gevasummary@sv.method)

#' @category Sub-slot accessors

#' @s4method Gets the `matrix` from the `values` slot in the internal [`GEVAInput-class`]
setMethod('inputvalues', 'GEVASummary', function(object) inputvalues(inputdata(object)))

#' @s4method Gets the `matrix` from the `weights` slot in the internal [`GEVAInput-class`]
setMethod('inputweights', c('GEVASummary', 'logical'), function(object, normalized) inputweights(inputdata(object), normalized))

#' @s4method
setMethod('inputweights', c('GEVASummary', 'missing'), function(object, normalized=FALSE) inputweights(inputdata(object)))

#' @s4method
setMethod('inputnames', 'GEVASummary', function(object) names(inputdata(object)))

#' @s4method Gets the `data.frame` from the `ftable` slot in the internal [`GEVAInput-class`]
setMethod('featureTable', 'GEVASummary', function(object) featureTable(inputdata(object)))

#' @s4method Gets the `factor` defined in the `factors` slot in the internal [`GEVAInput-class`]
setMethod('factors', 'GEVASummary', function(object) factors(inputdata(object)))

#' @s4method Sets the value to the `factor` slot in the internal [`GEVAInput-class`]
setMethod('factors<-', c('GEVASummary', 'factor'),
          function(object, value)
          {
            inpdt = inputdata(object)
            factors(inpdt) = value
            object@inputdata = inpdt
            object
          })

#' @s4method
setMethod('factors<-', c('GEVASummary', 'character'), function(object, value) { factors(object) = as.factor(value); object })

#' @s4method Gets the `list` from the `info` slot.
#' \cr If `recursive` is `TRUE`, appends the contents from the `info` slot in the internal [`GEVAInput-class`]
setMethod('infolist', c('GEVASummary', 'missing'),
          function(object, field=NULL, recursive = FALSE, ...)
          {
            if (!recursive) return(object@info)
            infols = unlist(list(object@info, infolist(inputdata(object))), recursive = FALSE)
            infols
          })

#' @s4method
setMethod('infolist<-', c('GEVASummary', 'list'), function(object, value) { object@info = value; object })

#' @s4method
setMethod('quantiles', 'GEVASummary', function(object) geva.quantiles(object))


#' @category Properties

#' @s4method Returns a `list` of analysis parameters passed to [`geva.summarize`] to obtain this object
setMethod('analysis.params', 'GEVASummary', function(gobject)
{
  svmets = sv.method(gobject)
  list(summary.method=svmets$S,
       variation.method=svmets$V)
})


#' @category Grouping

#' @s4method Gets the list of [`GEVAGroupSet-class`] objects attached to this instance. Only applicable for [`GEVAGroupedSummary-class`] objects
setMethod('groupsets', 'GEVASummary', function(object) typed.list(elem.class = 'GEVAGroupSet') )

#' @s4method Converts this instance to [`GEVAGroupedSummary-class`] and sets the list of [`GEVAGroupSet-class`] objects.
#' Can be used with `$<name>` to specify the object name in the list.
#' If `value` is a `GEVAGroupSet`, inserts the element and sets the name based on the value call
setMethod('groupsets<-', c('GEVASummary', 'TypedList'), function(object, value)
{
  gs2 = promote.class(object, 'GEVAGroupedSummary', groupsetlist=value)
  gs2
})

#' @s4method
setMethod('groupsets<-', c('GEVASummary', 'GEVAGroupSet'), function(object, value)
{
  argnm = deparse(substitute(value))[[1]]
  ggls = typed.list(elem.class = 'GEVAGroupSet')
  ggls[argnm] = value
  gs2 = promote.class(object, 'GEVAGroupedSummary', groupsetlist=ggls)
  gs2
})


# S3 Methods

#' @category Properties

#' @s3method Gets a `character` for the summarization method name
get.summary.method.GEVASummary <- function(x) get.summary.method(sv.method(x)$S)

#' @s3method Gets a `character` for the variation calculation method name
get.variation.method.GEVASummary <- function(x) get.variation.method(sv.method(x)$V)

#' @category Conversion and coercion

#' @s3method Equivalent to `sv(x)`
as.matrix.GEVASummary <- function(x, ...) sv(x)

#' @s3method Gets the expression that reproduces this `GEVASummary` object, including function parameters used by `geva.summary`. The `ginput` argument is optional but can be specified to replace the internal `GEVAInput`
as.expression.GEVASummary <- function(x, ginput, ...)
{
  parls = analysis.params(x)
  parls$ginput = if (missing(ginput))
    parse(text=sprintf("inputdata(%s)", deparse(substitute(x))))
  else
    substitute(ginput)
  expr = function2expression(geva.summarize,
                             args.list = parls,
                             ...)
  expr
}
