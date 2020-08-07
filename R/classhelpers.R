
##########################
# CLASS HELPER FUNCTIONS
# -----------------------
# 
# Helper methods for class definition and initialization
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

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
