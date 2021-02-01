

##########################
# GENERIC METHODS
# -----------------------
# 
# Common generic methods used among declared classes
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al

#' @include dochelpers.R
NULL

#' @title GEVA Generic Methods
#' @description Exhaustive list of generic methods exported from GEVA. Use `findMethods` to retrieve the specific usages.
#' @name generics
#' @rdname generics
#' 
#' @param object,x,gobject Primary object. See the documentation from each class for specific usages
#' @param normalized `logical`, whether to return values in the normalized scale
#' @param field When used with a information list, returns the information entry with the corresponding name
#' @param ... Additional parameters. If used with an imported S3 method, passes the arguments to the default `vector`, `matrix` or `data.frame` implementation
#' @param S Vector to construct the `S` slot
#' @param V Vector to construct the `V` slot
#' @param typedlist A [`TypedList-class`] object
#' @param group Character to filter the returned groups. Omit it to return all groups
#' @param gres A [`GEVAResults-class`] objec
#' @param value The value to be assigned
NULL

# === S4 ===

#' @rdname generics
setGeneric('inputvalues', function(object) standardGeneric('inputvalues'))

#' @rdname generics
setGeneric('inputweights', function(object, normalized) standardGeneric('inputweights'))

#' @rdname generics
setGeneric('inputdata', function(object) standardGeneric('inputdata'))

#' @rdname generics
setGeneric('inputnames', function(object) standardGeneric('inputnames'))

#' @rdname generics
setGeneric('infolist', function(object, field, ...) standardGeneric('infolist'))

#' @rdname generics
setGeneric('infolist<-', function(object, value) standardGeneric('infolist<-'))

#' @rdname generics
setGeneric('factors', function(object) standardGeneric('factors'))

#' @rdname generics
setGeneric('factors<-', function(object, value) standardGeneric('factors<-'))

#' @rdname generics
setGeneric('classification.table', function(object) standardGeneric('classification.table'))

#' @rdname generics
setGeneric('classification.table<-', function(object, value) standardGeneric('classification.table<-'))

#' @rdname generics
setGeneric('analysis.params', function(gobject) standardGeneric('analysis.params'))

setGeneric('as.indexes', function(x) standardGeneric('as.indexes'))

# GEVAInput-class
#' @rdname generics
setGeneric('featureTable', function(object) standardGeneric('featureTable'))
#' @rdname generics
setGeneric('featureTable<-', function(object, value) standardGeneric('featureTable<-'))

# SVTable-class
#' @rdname generics
setGeneric('sv', function(object) standardGeneric('sv'))

# GEVASummary-class
setGeneric('sv.method', function(gevasummary) standardGeneric('sv.method'))

# SVAttribute-class
#' @rdname generics
setGeneric('svattr', function(S, V) standardGeneric('svattr'))

# TypedList-class
#' @rdname generics
setGeneric('elem.class', function(typedlist) standardGeneric('elem.class'))
#' @rdname generics
setGeneric('elem.class<-', function(typedlist, value) standardGeneric('elem.class<-'))

# GEVAGroupedSummary-class
#' @rdname generics
setGeneric('groupsets', function(object) standardGeneric('groupsets'))
#' @rdname generics
setGeneric('groupsets<-', function(object, value) standardGeneric('groupsets<-'))


# GEVAGroupSet-class
#' @rdname generics
setGeneric('groups', function(object) standardGeneric('groups'))
#' @rdname generics
setGeneric('scores', function(object, group) standardGeneric('scores'))
#' @rdname generics
setGeneric('centroids', function(object) standardGeneric('centroids'))
#' @rdname generics
setGeneric('offsets', function(object) standardGeneric('offsets'))

# GEVAQuantiles-class
#' @rdname generics
setGeneric('sv.scores', function(object) standardGeneric('sv.scores'))
#' @rdname generics
setGeneric('qindexes', function(object) standardGeneric('qindexes'))
#' @rdname generics
setGeneric('qareasizes', function(object) standardGeneric('qareasizes'))
#' @rdname generics
setGeneric('qcount', function(object) standardGeneric('qcount'))
#' @rdname generics
setGeneric('quantiles', function(object) standardGeneric('quantiles'))
#' @rdname generics
setGeneric('quantiles.method', function(object) standardGeneric('quantiles.method'))

# GEVAQuantilesAdjusted-class
#' @rdname generics
setGeneric('group.rels', function(object) standardGeneric('group.rels'))

# GEVACluster-class
#' @rdname generics
setGeneric('cluster.method', function(object) standardGeneric('cluster.method'))

# GEVAResults-class
#' @rdname generics
setGeneric('results.table', function(gres) standardGeneric('results.table'))
#' @rdname generics
setGeneric('sv.data', function(object) standardGeneric('sv.data'))


# === S3 ===


# Summarization
#' @rdname generics
#' @export
variation <- function(object, ...) UseMethod('variation') # summary() generic already defined

#' @rdname generics
#' @export
get.summary.method <- function(x) UseMethod('get.summary.method')

#' @rdname generics
#' @export
get.variation.method <- function(x) UseMethod('get.variation.method')

# Analysis methods
#' @rdname generics
#' @export
get.distance.method <- function(x) UseMethod('get.distance.method')

# Converters
#' @rdname generics
#' @export
as.SVTable <- function(x, ...) UseMethod('as.SVTable')

# Linq-type
first <- function(x, ...) UseMethod('first')
where <- function(x, ...) UseMethod('where')
as.typed.list <- function(x, elem.class=NA_character_) UseMethod('as.typed.list')
distinct <- function(x, ..., incomparables=FALSE) UseMethod('distinct')

# Statistics
normalize.scale <- function(x, ...) UseMethod('normalize.scale')

# Plotting
color.values <- function(x, ...) UseMethod('color.values')
