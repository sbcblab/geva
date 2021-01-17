

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

# === S4 ===

setGeneric('inputvalues', function(object) standardGeneric('inputvalues'))
setGeneric('inputweights', function(object, normalized) standardGeneric('inputweights'))
setGeneric('inputdata', function(object) standardGeneric('inputdata'))
setGeneric('inputnames', function(object) standardGeneric('inputnames'))

setGeneric('infolist', function(object, field, ...) standardGeneric('infolist'))
setGeneric('infolist<-', function(object, value) standardGeneric('infolist<-'))

setGeneric('factors', function(object) standardGeneric('factors'))
setGeneric('factors<-', function(object, value) standardGeneric('factors<-'))

setGeneric('classification.table', function(object) standardGeneric('classification.table'))
setGeneric('classification.table<-', function(object, value) standardGeneric('classification.table<-'))

setGeneric('analysis.params', function(gobject) standardGeneric('analysis.params'))

setGeneric('as.indexes', function(x) standardGeneric('as.indexes'))

# GEVAInput-class
setGeneric('featureTable', function(object) standardGeneric('featureTable'))
setGeneric('featureTable<-', function(object, value) standardGeneric('featureTable<-'))

# SVTable-class
setGeneric('sv', function(object) standardGeneric('sv'))

# GEVASummary-class
setGeneric('sv.method', function(gevasummary) standardGeneric('sv.method'))

# SVAttribute-class
setGeneric('svattr', function(S, V) standardGeneric('svattr'))

# TypedList-class
setGeneric('elem.class', function(typedlist) standardGeneric('elem.class'))
setGeneric('elem.class<-', function(typedlist, value) standardGeneric('elem.class<-'))

# GEVAGroupedSummary-class
setGeneric('groupsets', function(object) standardGeneric('groupsets'))
setGeneric('groupsets<-', function(object, value) standardGeneric('groupsets<-'))


# GEVAGroupSet-class
setGeneric('groups', function(object) standardGeneric('groups'))
setGeneric('scores', function(object, group) standardGeneric('scores'))
setGeneric('centroids', function(object) standardGeneric('centroids'))
setGeneric('offsets', function(object) standardGeneric('offsets'))

# GEVAQuantiles-class
setGeneric('sv.scores', function(object) standardGeneric('sv.scores'))
setGeneric('qindexes', function(object) standardGeneric('qindexes'))
setGeneric('qareasizes', function(object) standardGeneric('qareasizes'))
setGeneric('qcount', function(object) standardGeneric('qcount'))
setGeneric('quantiles', function(object) standardGeneric('quantiles'))
setGeneric('quantiles.method', function(object) standardGeneric('quantiles.method'))

# GEVAQuantilesAdjusted-class
setGeneric('group.rels', function(object) standardGeneric('group.rels'))

# GEVACluster-class
setGeneric('cluster.method', function(object) standardGeneric('cluster.method'))

# GEVAResults-class
setGeneric('results.table', function(gres) standardGeneric('results.table'))
setGeneric('sv.data', function(object) standardGeneric('sv.data'))


# === S3 ===


# Summarization
variation <- function(object, ...) UseMethod('variation') # summary() generic already defined
get.summary.method <- function(x) UseMethod('get.summary.method')
get.variation.method <- function(x) UseMethod('get.variation.method')

# Analysis methods
get.distance.method <- function(x) UseMethod('get.distance.method')

# Converters
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
