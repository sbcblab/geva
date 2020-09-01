
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
normalize.weights <- function(w, by.min=FALSE, na.val=0)
{
  n = length(w)
  if (n == 0L) return(numeric(0))
  w[is.na(w)] = na.val
  if (by.min)
  {
    wsel = w != 0
    if (!any(wsel)) return(rep(1.0, n))
    w = w / min(w[wsel])
  }
  else
  {
    wsum = sum(w)
    if (wsum == 0) return(rep(1/n, n))
    w = w / wsum
  }
  w
}

# Normalizes the weights for all rows so that the values are proportionally distributed between 0 and 1
rows.normalize.weights <- function(mw, g = NULL)
{
  n = ncol(mw)
  if (!is.null(g))
  {
    if (length(g) != n) stop("invalid length for 'g' parameter: ncol(mw) != ", length(g))
    if (!is.factor(g)) g = as.factor(g)
    lvls = levels(g)
    if (length(lvls) > 1L)
    {
      for (lvl in lvls)
      {
        msel = which(lvl == g)
        if (length(msel) == 0L) next
        mw[,msel] = rows.normalize.weights(mw[,msel,drop=FALSE])
      }
      return(mw)
    }
  }
  vsums = base::rowSums(mw, na.rm = TRUE)
  selzeros = vsums == 0
  if (any(selzeros))
  {
    vsums[selzeros] = n
    mw[selzeros, ] = 1
  }
  mw = mw / vsums
  mw
}

# Scales the values between their minimum and maximum so that they are proportionally distributed within a range
normalize.scale.numeric <- function(x, min=0, max=1, default=mean(c(min, max)), ...)
{
  if (length(na.replace(x, NULL)) == 0L) return(x)
  if (min == max) return(x * 0 + default)
  vrng = range(x, na.rm = TRUE)
  if (vrng[1] == vrng[2])
    return(x * 0 + default)
  
  x = (x - vrng[1]) / (vrng[2] - vrng[1])
  if (min > max)
  {
    tmp = max
    max = min
    min = tmp
  }
  if (min != 0 || max != 1)
    x = x * (max - min) + min
  x
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

# Gets the first index where the numeric value is nearest to the slice value
# Using slice=0.5, normalized=TRUE and sorted=TRUE gives the median
which.slice <- function(x, slice, normalized=TRUE)
{
  n = length(x)
  if (n == 0L) return(integer(0))
  if (n == 1L) return(1L)
  if (normalized)
  {
    x = normalize.scale(x)
  }
  voffsets = abs(x - slice)
  ind.min = which.min(voffsets)
  ind.min
}
