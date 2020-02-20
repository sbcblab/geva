
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

strjoin <- function(strvec, sep="") paste0(strvec, collapse = sep)
