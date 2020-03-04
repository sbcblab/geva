
##########################
# STRING HELPER FUNCTIONS
# -----------------------
# 
# Helper methods to manipulate strings
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

# Override of plus function to support string concatenation
`+` <- function(x, y)
{
  if(is.character(x) || is.character(y))
  {
    return(paste(x, y, sep=""))
  }
  .Primitive("+")(x, y)
}

# More succint way to concatenate a vector of strings
strjoin <- function(strvec, sep="") paste0(strvec, collapse = sep)

# Concatenates a string vector using conjunctions when needed
strconjunct <- function(strvec, sep=', ', conj=' and ')
{
  len = length(strvec)
  if (len == 0L) return('')
  if (len == 1L) return(strvec)
  return(strjoin(strvec[1L:(len-1L)], sep) + conj + strvec[len])
}
