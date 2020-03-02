
##########################
# INPUT METHODS
# -----------------------
# 
# Functions to read and process data for GEVA
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

#' @include c_GEVAInput.R

# Processes data for GEVA using table files as input
#' @export
geva.read.tables <- function(filenames=NULL, dirname=".", col.values="logFC", col.pvals="adj.P.val", col.other=NULL, ...)
{
  if (is.null(filenames)) filenames = list.files(dirname, full.names = T, all.files = F, recursive = F, include.dirs = F)
  assert.notempty(filenames)
  fcount = length(filenames)
  if (fcount == 1L) stop("GEVA requires at least two tables of comparison results")
  df = NULL
  dw = NULL
  da = NULL
  for (fnm in filenames)
  {
    basenm = sub('(.*)\\..*$', '\\1', basename(fnm))
    dfTmp = read.delim(fnm, row.names=1, check.names = F)
    dfcnms = colnames(dfTmp)
    if (col.values %in% dfcnms)
    {
      if (is.null(df))
      {
        df = data.frame(row.names = rownames(dfTmp))
      } else assert.dim(dfTmp, nrow = nrow(df))
      df[, basenm] = dfTmp[, col.values]
    } else stop(sprintf("No column named '%s' was found in %s", col.values, basename(fnm)))
    if (is.null(dw)) dw = data.frame(row.names = rownames(df))
    if (is.null(da)) da = data.frame(row.names = rownames(df))
    if (!(is.null(col.pvals) || is.na(col.pvals)))
    {
      if (col.pvals %in% dfcnms)
      {
        dw[, basenm] = 1 - dfTmp[, col.pvals]
      } else {
        dw[, basenm] = rep(NA, nrow(df))
        warning(sprintf("No column named '%s' was found in %s. NAs used instead.", col.pvals, basename(fnm)))
      }
    } else dw[, col.pvals] = rep(1.0, nrow(df))
    
    if (!(is.null(col.other) || any(is.na(col.other))))
    {
      for (conm in setdiff(intersect(col.other, dfcnms), colnames(da))) da[, conm] = dfTmp[, conm]
    }
  }
  info = list(filenames = filenames, values.column = values.column, pvalues.column = pvalues.column)
  ginput = new('GEVAInput', values=as.matrix(df), weights=as.matrix(dw), probeattrs = da, info = info)
  ginput
}