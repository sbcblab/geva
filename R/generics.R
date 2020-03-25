

##########################
# GENERIC METHODS
# -----------------------
# 
# Common generic methods used among declared classes
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

# === S4 ===

setGeneric('inputvalues', function(object) standardGeneric('inputvalues'))
setGeneric('inputweights', function(object) standardGeneric('inputweights'))
setGeneric('inputdata', function(object) standardGeneric('inputdata'))

setGeneric('infolist', function(object, recursive) standardGeneric('infolist'))
setGeneric('infolist<-', function(object, value) standardGeneric('infolist<-'))

setGeneric('factors', function(object) standardGeneric('factors'))
setGeneric('factors<-', function(object, value) standardGeneric('factors<-'))

setGeneric('as.indexes', function(x) standardGeneric('as.indexes'))

# GEVAInput-class
setGeneric('featureTable', function(object) standardGeneric('featureTable'))

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
setGeneric('qcount', function(object) standardGeneric('qcount'))
setGeneric('quantiles', function(object) standardGeneric('quantiles'))

# GEVAQuantilesAdjusted-class
setGeneric('group.rels', function(object) standardGeneric('group.rels'))

# GEVAResults-class
setGeneric('results.table', function(gres) standardGeneric('results.table'))


# === S3 ===

variation <- function(object, ...) UseMethod('variation')

get.summary.method <- function(x) UseMethod('get.summary.method')
get.variation.method <- function(x) UseMethod('get.variation.method')

get.distance.method <- function(x) UseMethod('get.distance.method')
as.SVTable <- function(x) UseMethod('as.SVTable')

