

##########################
# USE CASE CHECK FUNCTIONS
# -----------------------
# 
# Functions to deal with specific use cases, usually warning the user when something occurred
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0


# GEVAInput
check.suitable.factors <- function(fs)
{
  if (length(levels(fs)) == 1)
  {
    warning("'factors' has only one level. All analyses using factors will be ignored")
    return(invisible(F))
  }
  invisible(T)
}
