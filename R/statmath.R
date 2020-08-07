
##########################
# STATISTICS & MATH CALCULATION METHODS
# -----------------------
# 
# Helper methods for math and statistical calculations
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

#' @include matrixhelpers.R

# Normalizes the weights so that the values are proportionally distributed between 0 and 1
normalize.weights <- function(w, na.val=0)
{
  n = length(w)
  if (n == 0L) return(numeric(0))
  w[is.na(w)] = na.val
  if (!any(w != 0)) return(rep(1, n))
  maxval = max(w)
  w = w / maxval
  w
}

# Restores the p.values from weight values if they are normalized
restore.weights.pvals <- function(w)
{
  n = length(w)
  if (n == 0L) return(numeric(0))
  w[is.na(w)] = 0
  if (all(w < 1)) return(w)
  maxval = max(w)
  w = w / maxval
  w
}

# Gets the row indexes where all values are above a cutoff
which.rows.outside.cutoff <- function(mat, cutoff=0.05, na.val=0)
{
  mat = as.matrix(mat)
  if (anyNA(mat))
    mat[is.na(mat)] = na.val
  mat = 1 - mat
  vinds = which(all.apply(mat, `>`, e2=cutoff))
  vinds
}