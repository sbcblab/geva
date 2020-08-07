

##########################
# CALL HELPER FUNCTIONS
# -----------------------
# 
# Helper methods for function calling at run-time
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

# Finds recursively the source name of 'obj' function used as argument in parent functions
call.objname <- function(obj, prevfns=0)
{
  prevfns = prevfns + 1
  nm = deparse(substitute(obj))
  if (prevfns > 1)
  {
    for (i in (2:prevfns))
    {
      ptxt = sprintf('deparse(substitute(%s))', nm)
      nm = eval.parent(parse(text=ptxt), n=i-1)
    }
  }
  nm
}

# Gets a list of the default arguments inside the current function
call.default.arg <- function(obj, prevfns=0L)
{
  prevfns = prevfns + 1L
  objname = call.objname(obj, prevfns)
  fms = eval.parent(formals(sys.function(sys.parent(prevfns))), prevfns)
  if (!(objname %in% names(fms))) return(NULL)
  argres = fms[[objname]]
  if (missing(argres)) argres = NULL
  if (is.call(argres)) argres = eval.parent(argres, prevfns)
  return(argres)
}

# Gets a character vector with the argument names used in dots. This does not evaluate the arguments
call.dots.argnames <- function(...)
{
  if (...length() == 0L) return(character(0))
  names(match.call(expand.dots = FALSE)$`...`)
}

# Gets a pairlist with the unvaluated arguments included in dots
call.dots.args <- function(..., .prevfns=0L)
{
  if (...length() == 0L) return(NULL)
  match.call(expand.dots = FALSE, envir = sys.frame(.prevfns))$`...`
  #eval.parent(quote(match.call(expand.dots = FALSE)$`...`), n = .prevfns + 1L)
}

# Gets only those arguments expressed as an 'as-is' call (e.g. I(expression))
call.dots.asis <- function(..., .prevfns=0L)
{
  if (...length() == 0L) return(NULL)
  lcalls = as.list(eval.parent(substitute(match.call(expand.dots = FALSE)[['...']])))
  argnms = names(lcalls)
  if (is.null(argnms)) argnms = rep('', ...length())
  unmds = nchar(argnms) == 0L
  argnms[unmds] = sprintf("..%d", 1L:...length())[unmds]
  argnms = argnms[sapply(lcalls, function(cl) is.call(cl) && length(cl) == 2L && cl[[1]] == 'I')]
  if (length(argnms) == 0) return(NULL)
  argnms
}

# Gets a named list with the arguments included in dots. When names are not provided, the call is used as the element name
call.dots.named.list <- function(..., .trimquotes=TRUE, .replacefn=NULL)
{
  nargs = ...length()
  if (nargs == 0L) return(setNames(list(), nm = character(0)))
  argls = list(...)
  argnms = names(argls)
  if (is.null(argnms)) argnms = rep('', nargs)
  selempts = nchar(argnms) == 0L
  if (any(selempts))
  {
    pls = match.call(expand.dots = FALSE)[['...']] #eval.parent(substitute(match.call(expand.dots = FALSE)[['...']]))
    rnms = sapply(pls[selempts], deparse)
    if (is.function(.replacefn)) # Used to select the element name when not present. The pattern is function(element, name)
    {
      rfn = .replacefn
      fnargs = length(formals(.replacefn))
      if (fnargs == 1L) rfn = function(e, name) .replacefn(e)
      else if (fnargs == 0L) rfn = function(e, name) .replacefn()
      indempts = which(selempts)
      for (i in 1:length(indempts)) rnms[i] = rfn(argls[[indempts[i]]], rnms[i])
    }
    if (.trimquotes) rnms = trimws(rnms, whitespace = '[\\s\'\"]')
    
    argnms[selempts] = rnms
    argls = setNames(argls, argnms)
  }
  argls
}

# Gets a character vector with the argument names used in dots. Empty names will be replaced by the actual calls 
call.dots.namesorargs <- function(..., .trimquotes=TRUE)
{
  if (...length() == 0L) return(character(0))
  pargs = call.dots.args(..., .prevfns=1L)
  nargs = length(pargs)
  if (nargs == 0L) return(character(0))
  argnms = names(pargs)
  if (is.null(argnms))
  {
    selunamed = rep(T, nargs)
    argnms = rep('', nargs)
  } else selunamed = argnms %in% ''
  if (any(selunamed))
  {
    rexprs = sapply(pargs[selunamed], deparse)
    if (.trimquotes) rexprs = trimws(rexprs, whitespace = '[\\s\'\"]')
    argnms[selunamed] = rexprs
  }
  argnms
}

# Get the possible characters for argument names, removing possible calls
call.arg.characters <- function(callarg)
{
  argnm = deparse(substitute(callarg))
  argsub = eval.parent(parse(text=sprintf("substitute(%s)", argnm)))
  if (is.call(argsub))
  {
    retls = new.env()
    recdecall = function(e)
    {
      if (is.call(e))
      {
        subls = as.list(e)[-1]
        for (el in subls)
        {
          recdecall(el)
        }
      }
      else retls[[sprintf('%d', length(retls))]] = e
      T
    }
    recdecall(argsub)
    #argsub = rapply(as.list(argsub), function(e) if (is.call(e)) as.list(e)[-1] else NULL, how='replace')
    argsub = unlist(as.list(retls))
  }
  argsub = unique(as.character(argsub))
  argsub
}

# Gets an argument by its name inside the dots of a parent funtion. If a default value is provided, this is used if the argument is missing
...arg <- function(name, default)
{
  argnm = trimws(deparse(substitute(name)), whitespace = '[\'\"\t\n]')
  dotexpr = substitute(call.dots.args(...))
  argls = eval(dotexpr, envir = sys.frame(1L))
  nodef = missing(default)
  ret = if (argnm %in% names(argls)) argls[[argnm]] else if (!nodef) default else stop(sprintf("object '%s' not found", argnm), call. = F)
  ret
}

# Merges the arguments inside a function with the passed arguments in dots. The dots arguments in called function are preferred when present 
...merge <- function(...)
{
  argls = list(...)
  pargls = eval.parent(quote(list(...)))
  newarginds = which(!(names(argls) %in% names(pargls)))
  for (newi in newarginds)
  {
    pargls[[names(argls)[newi]]] = argls[[newi]]
  }
  pargls
  #as.list(pargls)
}

# Uses the second argument if the first is null
`%ifnull%` <- function(x, replacement)
{
  if (!is.null(x)) return(x)
  return(replacement)
}
