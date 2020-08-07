
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
#' @include statmath.R
#' @include callhelpers.R

# Processes data for GEVA using table files as input
#' @export
geva.read.tables <- function(filenames=NULL, dirname=".", col.values="logFC", col.pvals="adj.P.Val", col.other=NULL, ..., files.pattern = "\\.txt$", p.value.cutoff=0.05)
{
  if (length(filenames) == 0L || dir.exists(filenames[1]) ) filenames = list.files(dirname, full.names = T, all.files = F, recursive = F, include.dirs = F, pattern = files.pattern)
  assert.notempty(filenames)
  fcount = length(filenames)
  if (fcount == 1L) stop("GEVA requires at least two tables of comparison results")
  df = NULL
  dw = NULL
  da = NULL
  for (fnm in filenames)
  {
    vprint("Reading '%s' ...", basename(fnm))
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
      }
      else
      {
        dw[, basenm] = rep(NA, nrow(df))
        warning(sprintf("No column named '%s' was found in %s. NAs used instead.", col.pvals, basename(fnm)))
      }
    } else dw[, col.pvals] = rep(1.0, nrow(df))
    if (!(is.null(col.other) || any(is.na(col.other))))
    {
      for (conm in setdiff(intersect(col.other, dfcnms), colnames(da))) da[, conm] = dfTmp[, conm]
    }
  }
  na.val = ...arg(na.val, 1)
  if (!is.null(p.value.cutoff) && !is.na(p.value.cutoff))
  {
    # Removes the rows where all p-values are above the cutoff
    vinvrinds = which.rows.outside.cutoff(dw, cutoff=p.value.cutoff, na.val=na.val)
    remcount = nrow(dw) - length(vinvrinds)
    if (remcount != 0L)
    {
      dw = dw[vinvrinds,,drop=FALSE]
      df = df[vinvrinds,,drop=FALSE]
      da = da[vinvrinds,,drop=FALSE]
      catline("Removed %d rows where all p-values were above the cutoff (%s)", remcount, as.character(p.value.cutoff))
    }
  }
  
  dw = t(apply(dw, 1, normalize.weights, na.val=na.val)) # p.values are normalized so that weighted calculations can be made
  info = list(filenames = filenames, values.column = col.values, pvalues.column = col.pvals)
  ginput = new('GEVAInput', values=as.matrix(df), weights=as.matrix(dw), ftable = da, info = info)
  vprint("Read %d columns with %d probes", ncol(df), nrow(df))
  ginput
}


# Corrects the input data, removing rows containing invalid values (NA, Inf)
#' @export
geva.input.correct <- function(ginput, na.rm=TRUE, inf.rm=TRUE, invalid.col.rm=TRUE)
{
  mv = inputvalues(ginput)
  mw = inputweights(ginput)
  sel.valid.cols = rep(TRUE, ncol(mv))
  if (invalid.col.rm)
  {
    if (na.rm)
    {
      sel.valid.cols = sel.valid.cols & !(all.apply(mv, is.na, 2)) & !(all.apply(mw, is.na, 2))
    }
    if (inf.rm)
    {
      sel.valid.cols = sel.valid.cols & !(all.apply(mv, is.infinite, 2)) & !(all.apply(mw, is.infinite, 2))
    }
    if (all(!sel.valid.cols)) stop("All columns are invalid. Perhaps the source data was corrupted?")
    mv = mv[,sel.valid.cols,drop=FALSE]
    mw = mw[,sel.valid.cols,drop=FALSE]
  }
  sel.valid.rows = rep(TRUE, nrow(mv))
  if (na.rm)
  {
    sel.valid.rows = sel.valid.rows & !(apply(mv, 1, anyNA)) & !(apply(mw, 1, anyNA))
  }
  if (inf.rm)
  {
    sel.valid.rows = sel.valid.rows & !(any.apply(mv, is.infinite)) & !(any.apply(mw, is.infinite))
  }
  ginput = ginput[sel.valid.rows,sel.valid.cols,drop=FALSE]
  ginput
}

# Replaces the row.names from a GEVAInput based on a attribute column or a character vector
#' @export
geva.input.rename.rows <- function(ginput, attr.column, dupl.rm.method = c("least.p.vals", "order") )
{
  dupl.rm.method = match.arg(dupl.rm.method)
  errmsg = sprintf("%s must be a vector (length %d) or a valid attribute column name", "%s", nrow(ginput))
  assert.notempty(attr.column, .posmsg = errmsg)
  if (is.character(attr.column) && length(attr.column) == 1L)
  {
    if (attr.column %in% colnames(featureTable(ginput)))
    {
      attr.column = featureTable(ginput)[,attr.column]
    }
    else stop(sprintf("could not find an attribute column named '%s'", attr.column))
  }
  if (!is.character(attr.column))
  {
    if (is.vector(attr.column) || is.factor(attr.column))
      attr.column = as.character(attr.column)
    else stop(sprintf(errmsg, as.character(quote(attr.column))))
  }
  assert.dim(attr.column, length=nrow(ginput))
  attr.column[is.na(attr.column)] = ''
  sel.empty.vals = nchar(attr.column) == 0L
  if (all(sel.empty.vals)) stop("The selected column must contain at least one non-empty character")
  inds.match = match(attr.column, rownames(ginput))
  if (!anyNA(inds.match)) return(ginput[inds.match,,drop=FALSE])
  sel.dupls = duplicated(attr.column)
  if (any(sel.dupls) && dupl.rm.method == "least.p.vals")
  {
    # Removes the duplicated rows where the sum of p.values is
    sel.dupls = sel.dupls | duplicated(attr.column, fromLast = TRUE)
    vdinds = which(sel.dupls)
    fdgroups = as.factor(attr.column[vdinds])
    mw = inputweights(ginput)
    vsumpvals = rowSums(t(apply(mw[vdinds,,drop=FALSE], 1, restore.weights.pvals)))
    sel.dupls[vdinds] = gtapply(vsumpvals, fdgroups, function(x) 1:length(x) != which.min(x))
  }
  sel.valid = !sel.dupls & (nchar(attr.column) != 0L)
  assign("sel.valid", sel.valid, envir=globalenv())
  ginput = ginput[sel.valid,,drop=FALSE]
  featureTable(ginput)[,'renamed_id'] = rownames(ginput)
  rownames(ginput) = attr.column[sel.valid]
  catline("Row names renamed")
  if (any(!sel.valid))
    catline("Removed %d duplicated lines", sum(!sel.valid))
  ginput
}
