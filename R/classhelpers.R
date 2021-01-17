
##########################
# CLASS HELPER FUNCTIONS
# -----------------------
# 
# Helper methods for class definition and initialization
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al

# Returns a named character vector such as c(slotname = slottype)
# Called only inside 'initialize' generic methods
current.class.slots <- function()
{
  obj = eval.parent(substitute(.Object))
  slots = getClassDef(class(obj)[[1]])@slots
  slots = unlist(slots)
  slots
}

# Returns a list of class arguments
# Called only inside 'initialize' generic methods
initialize.class.args <- function(...)
{
  obj = eval.parent(substitute(.Object))
  slots = getClassDef(class(obj)[[1]])@slots
  slots = unlist(slots)
  argls = list(...)
  argnms = names(argls)
  for (nm in names(slots))
  {
    
    if (!(nm %in% argnms))
    {
      elem = slot(obj, nm)
      attr(elem, 'unitialized') = T
      argls[[nm]] = elem
    }
  }
  argls
}

# Checks if the argument is unitialized (i.e. has the 'unitialized' attribute)
is.unitialized <- function(elem)
{
  unitattr = attr(elem, 'unitialized')
  if (is.null(unitattr)) return(F)
  return(unitattr == T)
}

# Gets the elem parameter if the argument was initialized, or else the default parameter
get.initialized <- function(elem, default) { if (is.unitialized(elem)) default else elem }

# Creates a new instance of a class using the slots of an existing object, in addition to arguments passed in dots
promote.class <- function(obj, Class, ...)
{
  args = list.merge(list(Class=Class, ...),
                    setNames(lapply(slotNames(obj), function(snm) slot(obj, snm)), slotNames(obj)))
  do.call(new, args)
}

# Checks if a S3 method is implemented for a class or its derived classes
isS3implemented <- function(method, classname, include.default=FALSE)
{
  s3methods = as.character(.S3methods(method, classname))
  if (sprintf("%s.%s", method, classname) %in% s3methods)
    return(TRUE)
  if (include.default && sprintf("%s.default", method) %in% s3methods)
    return(TRUE)
  classes = getAllSuperClasses(getClassDef(classname))
  if (length(classes) == 0L) return(FALSE)
  return (any(sprintf("%s.%s", method, classes) %in% s3methods))
}
