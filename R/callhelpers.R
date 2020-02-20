

##########################
# CALL HELPER FUNCTIONS
# -----------------------
# 
# Helper methods for function calling at run-time
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

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

call.default.arg <- function(obj)
{
  objname = call.objname(obj, 1)
  fms = eval.parent(formals(sys.function(sys.parent())))
  if (!(objname %in% names(fms))) return(NULL)
  argres = fms[[objname]]
  if (missing(argres)) argres = NULL
  if (is.call(argres)) argres = eval.parent(argres)
  return(argres)
}