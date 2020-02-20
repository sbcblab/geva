

##########################
# GENERIC METHODS
# -----------------------
# 
# Common generic methods used among declared classes
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0


setGeneric('inputvalues', function(object) standardGeneric('inputvalues'))
setGeneric('inputweights', function(object) standardGeneric('inputweights'))

setGeneric('infolist', function(object) standardGeneric('infolist'))
setGeneric('infolist<-', function(object, value) standardGeneric('infolist<-'))

setGeneric('factors', function(object) standardGeneric('factors'))
setGeneric('factors<-', function(object, value) standardGeneric('factors<-'))

# GEVAInput-class
setGeneric('probeattrs', function(object) standardGeneric('probeattrs'))
