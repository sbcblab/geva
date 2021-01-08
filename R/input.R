
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

# Processes any compatible data as input to GEVA, including data.frames, lists and 
#' @export
geva.merge.input <- function(..., col.values="logFC", col.pvals="adj.P.Val", col.other=NULL)
{
  if (...length() == 0L) return(NULL)
  argls = list(...)
  names(argls) = call.dots.namesorargs(...)
  argls = list.flatten(argls, ignore.class = 'geva.promise.read.path')
  df = NULL
  dw = NULL
  da = NULL
  lsfacts = list()
  na.val = ...arg(na.val, 0)
  nms = names(argls)
  if (is.null(nms))
    nms = paste0("arg", as.character(1L:length(argls)))
  nms[nchar(nms) == 0L] = "arg"
  nms = make.unique(nms, sep="_")
  for (i in 1L:length(argls))
  {
    arg = argls[[i]]
    arg.nm = nms[i]
    argcall = arg.nm
    arg.values = NULL
    arg.weights = NULL
    arg.attrs = NULL
    vfacts = character(0)
    if (is(arg, 'geva.promise.read.path'))
    {
      fnm = arg$path
      read.args = arg$read.args
      vprint("Reading '%s' ...", basename(fnm))
      basenm = sub('(.*)\\..*$', '\\1', basename(fnm))
      rdargs = list.merge(read.args, list(file = fnm, row.names=1, check.names = F))
      arg = do.call(read.delim, rdargs)
      argcall = basename(fnm)
    }
    if (is.matrix(arg))
      arg = as.data.frame(arg)
    else if (is.vector(arg) && length(arg) > 1L && (if (is.null(df)) length(arg) else nrow(df)) == length(arg))
    {
      if (is.numeric(arg))
      {
        arg = data.frame(v=arg, w=rep(0, length(arg)), row.names = names(arg))
        colnames(arg) = c(c(col.values, 'v')[1], c(col.pvals, 'w')[1])
      }
      #else if (is.character(arg))
      #{
      #  ...
      #}
    }
    if (is.data.frame(arg))
    {
      sel.vals = colnames(arg) %in% col.values
      if (!any(sel.vals))
      {
        warning(sprintf("No column named '%s' was found in %s", strconjunct(col.values, strvec = "', '", conj="' or '"), argcall))
        next
      }
      sel.ws = colnames(arg) %in% col.pvals
      if (!any(sel.ws))
      {
        warning(sprintf("No column named '%s' was found in %s. Using %s as replacement for weights", strconjunct(col.values, strvec = "', '", conj="' or '"), argcall, na.val))
      }
      else if (sum(sel.ws) != sum(sel.vals))
      {
        sel.vals = seq(1L, length.out = length(sel.vals)) %in% which(sel.vals)[1]
        sel.ws = seq(1L, length.out = length(sel.ws)) %in% which(sel.ws)[1]
      }
      sel.attrs = col.other %in% (if (is.null(da)) colnames(arg) else setdiff(colnames(arg), colnames(da)))
      arg.values = arg[, sel.vals, drop=FALSE]
      if (!(is.numeric(arg.values) || all(sapply(1:ncol(arg.values), is.numeric))))
      {
        warning(sprintf("Input '%s' is not numeric. Argument ignored", argcall))
        next
      }
      if (ncol(arg.values) == 1L)
        colnames(arg.values) = arg.nm
      if (any(sel.ws))
        arg.weights = 1 - arg[, sel.ws, drop=FALSE]
      if (any(sel.attrs))
        arg.attrs = arg[, sel.attrs, drop=FALSE]
    }
    else if (hasMethod('inputdata', class(arg)))
    {
      arg = inputdata(arg)
      if (inherits(arg, 'GEVAInput'))
      {
        arg.values = inputvalues(arg)
        arg.weights = inputweights(arg)
        vfacts = factors(arg)
      }
    }
    if (is.null(arg.values))
      next
    if (is.null(arg.weights))
      arg.weights = (arg.values * 0) + na.val
    if (is.null(df))
    {
      df = data.frame(row.names = rownames(arg.values))
      dw = data.frame(row.names = rownames(arg.values))
      da = data.frame(row.names = rownames(arg.values))
    }
    else
    {
      ind.matchrows = match(rownames(df), rownames(arg.values))
      if (anyNA(ind.matchrows))
      {
        sel.valids = !is.na(ind.matchrows)
        if (any(sel.valids))
        {
          df = df[sel.valids,,drop=FALSE]
          dw = dw[sel.valids,,drop=FALSE]
          da = da[sel.valids,,drop=FALSE]
          ind.matchrows = ind.matchrows[sel.valids]
          vprint("Removed %s rows upon merging with '%s'", sum(!sel.valids), argcall)
        }
        else
        {
          if (nrow(df) == nrow(arg.values))
            ind.matchrows = 1L:nrow(arg.values)
          else
          {
            warning(sprintf("'%s' ignored: no matching rows or dimensions (%s != %s)", argcall, nrow(arg.values), nrow(df)))
            next
          }
        }
      }
      arg.values = arg.values[ind.matchrows,,drop=FALSE]
      arg.weights = arg.weights[ind.matchrows,,drop=FALSE]
      if (!is.null(arg.attrs))
        arg.attrs = arg.attrs[ind.matchrows,,drop=FALSE]
    }
    if (length(vfacts) != ncol(arg.values))
      vfacts = rep(NA_character_, ncol(arg.values))
    lsfacts[[length(lsfacts)+1L]] = as.factor(vfacts)
    colnms = setdiff(make.unique(c(colnames(df), colnames(arg.values)), sep = '_'), colnames(df))
    df[, colnms] = arg.values
    dw[, colnms] = arg.weights
    if (!is.null(arg.attrs))
      da[,colnms] = arg.attrs
    
  }
  #if (ncol(df) == 0L) return(NULL)
  facts = as.factor(unlist(lsfacts))
  info = list(values.column = col.values, pvalues.column = col.pvals)
  ginput = new('GEVAInput', values=as.matrix(df), weights=as.matrix(dw), ftable = da, info = info, factors=facts)
  ginput
}

# Processes data for GEVA using table files as input
#' @export
geva.read.tables <- function(filenames=NULL, dirname=".", col.values="logFC", col.pvals="adj.P.Val", col.other=NULL, ..., files.pattern = "\\.txt$", p.value.cutoff=0.05, read.args = list())
{
  if (is.null(p.value.cutoff) || is.na(p.value.cutoff[1]))
    p.value.cutoff = 1
  assert.operator(p.value.cutoff, `>=` = 0, `<=` = 1)
  if (length(filenames) == 0L || dir.exists(filenames[1]) ) filenames = list.files(dirname, full.names = T, all.files = F, recursive = F, include.dirs = F, pattern = files.pattern)
  assert.notempty(filenames)
  fcount = length(filenames)
  if (fcount <= 1L) stop("GEVA requires at least two tables of comparison results")
  lsinput = lapply(setNames(filenames, basename(filenames)), function(fnm)
  {
    fls = list(path=fnm, read.args=read.args)
    class(fls) = 'geva.promise.read.path'
    fls
  })
  ginput = geva.merge.input(lsinput)
  vprint("Read %d columns with %d probes", ncol(ginput), nrow(ginput))
  if (p.value.cutoff < 1)
    ginput = geva.input.filter(ginput, p.value.cutoff, ...)
  ginput
}

# Filters the input data, removing rows containing p.values (1 - weights) below a threshold
#' @export
geva.input.filter <- function(ginput, p.value.cutoff=0.05, na.val=0, ...)
{
  # Removes the rows where all p-values are above the cutoff
  dw = inputweights(ginput)
  sel.remove = 1L:nrow(ginput) %in% which.rows.outside.cutoff(1 - dw, cutoff=1-p.value.cutoff, na.val=na.val)
  remcount = nrow(dw) - sum(sel.remove)
  if (remcount != 0L)
  {
    ginput = ginput[!sel.remove,,drop=FALSE]
    vprint("Removed %s rows where all p-values were above the cutoff (%s)", remcount, as.character(p.value.cutoff))
  }
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
