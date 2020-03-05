
##########################
# VECTOR HELPER FUNCTIONS
# -----------------------
# 
# Helper methods to manipulate vectors
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

# Gets the initial and final indexes within a limit
indexes.headtail <- function(length, n=6L, start=1L, ...)
{
  length = as.integer(length)
  if (is.na(length)) stop("Invalid 'length' argument")
  if (length <= 0) return(integer(0))
  if (n < length)
  {
    halflimit = n / 2
    lowerlimit = as.integer(ceiling(halflimit))
    lowerinds = 1:lowerlimit
    upperlimit = as.integer(floor(halflimit))
    if (upperlimit == 0L) return(lowerinds)
    upperinds = (length - upperlimit + 1):length
    inds = c(lowerinds, upperinds)
  } else {
    inds = 1:length
  }
  inds = inds + (start - 1)
  inds
}

# Methods to get both head and tail from arrays and data.frames
headtail <- function(x, ...) UseMethod('headtail')

headtail.default <- function(x, n = 6L, ...)
{
  stopifnot(length(n) == 1L)
  inds = indexes.headtail(length(x), n, ...)
  x[inds]
}

headtail.matrix <- function(x, n = 6L, ...)
{
  stopifnot(length(n) == 1L)
  inds = indexes.headtail(nrow(x), n, ...)
  x[inds, , drop = FALSE]
}

headtail.data.frame <- function(x, n = 6L, ...) headtail.matrix(x, n, ...)


# Checks if a vector is a named vector
is.named <- function(x) !is.null(names(x))
