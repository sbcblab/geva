

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

# GEVAInput-class
setGeneric('probeattrs', function(object) standardGeneric('probeattrs'))

# SVTable-class
setGeneric('sv', function(object) standardGeneric('sv'))

# TypedList-class
setGeneric('elem.class', function(object) standardGeneric('elem.class'))

# GEVAGroupedSummary-class
setGeneric('groupsets', function(object) standardGeneric('groupsets'))


# === S3 ===

variation = function(object, ...) UseMethod('variation')

