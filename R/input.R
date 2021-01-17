
##########################
# INPUT METHODS
# -----------------------
# 
# Functions to read and process data for GEVA
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al

#' @include c_GEVAInput.R
#' @include statmath.R
#' @include callhelpers.R
NULL

# Processes any compatible data as input to GEVA, including data.frames, lists and 
#' @title GEVA Input Processing and Merge
#' @description Functions to read, load, and concatenate the experimental comparisons from the data input. This is the initial step to proceed with any GEVA analysis.
#' 
#' @details The `geva.merge.input` function takes multiple tables as arguments (*e.g.*, `matrix` or `data.frame` objects), extracts the *logFC* columns from each table and merges them into a single [`GEVAInput-class`] dataset.
#' 
#' The column names are specified in the `col.values` and `col.pvals` arguments (`character`) and must correctly match the column names for *logFC* and p-value columns, respectively, in the inputs to be extracted.
#' Multiple values for column names can also be specified as valid name possibilities if they differ among the tables.
#' 
#' @note The inclusion of p-value columns is not technically required, but strongly recommended as they improve the statistical accuracy in the summarization steps. If the p-value (or adjusted p-value) columns are present, their values are converted to weights by applying `1 - pvalue` for each `pvalue` element, otherwise an optional `na.val` optional argument can specified as replacement to the absent values (default is `NA`). Weights are used to accomodate the central *logFC* values towards the most significant observations and penalize potential statistical innacuracies.
#' 
#' @param ... multiple `matrix` or `data.frame` objects. At least two arguments are required for `geva.merge.input`, but it's optional for `geva.read.tables`. The optional arguments in `geva.read.tables` are also passed to its internal call to `geva.merge.input` and [`geva.input.filter`].
#' \cr In addition, the following optional arguments are accepted:
#' \itemize{
#' \item{`na.val`  : (`numeric`) value between `0` and `1` used as replacement when a p-value column is not present (default is `NA`)}
#' \item{`use.regex` : (`logical`) whether to match the column names using [regular expressions][base::regex] (default is `FALSE`) }
#' \item{`verbose` : (`logical`) whether to print the current loading and merge progress (default is `TRUE`)}
#' }
#' @param col.values `character` vector, possible name(s) to match the *logFC* column(s) from each table
#' @param col.pvals `character` vector, possible name(s) to match the p-value column(s) from each table
#' @param col.other `character` vector, name(s) to match additional columns (*e.g.*, gene symbols). Ignored if `NULL`
#' 
#' @return A [`GEVAInput-class`] object
#' 
#' @examples
#' ### EXAMPLE 1
#' ## geva.merge.input example with three randomly generated tables
#' ## (For demonstration purposes only)
#' 
#' # Number of rows
#' n <- 10000
#' 
#' # Random row (probe) names
#' probnms <- sprintf("PROBE_%s", 1:n)
#' 
#' # Random gene names (optional)
#' genenms <- paste0(sprintf("GENE_%s", 1:n), LETTERS[1:n %% (length(LETTERS)+1)])
#' 
#' # Random table 1
#' dt1 <- data.frame(row.names=probnms,
#'                   logfc=(rnorm(n, 0, sd=2) * rnorm(n, 0, sd=0.5)),
#'                   pvalues = runif(n, max=0.08),
#'                   genesymbol = genenms)
#' # Random table 2
#' dt2 <- data.frame(row.names=probnms,
#'                   logfc=(rnorm(n, 0, sd=2) * rnorm(n, 0, sd=0.5)),
#'                   pvalues = runif(n, max=0.08),
#'                   genesymbol = genenms)
#' # Random table 3
#' dt3 <- data.frame(row.names=probnms,
#'                   logfc=(rnorm(n, 0, sd=2) * rnorm(n, 0, sd=0.5)),
#'                   pvalues = runif(n, max=0.08),
#'                   genesymbol = genenms)
#' 
#' # Merges the three tables
#' ginput <- geva.merge.input(exp1=dt1, exp2=dt2, exp3=dt3,
#'                            col.values="logfc",
#'                            col.pvals="pvalues",
#'                            col.other="genesymbol")
#' 
#' # Prints the first rows from the merged table
#' print(head(ginput))               # values
#' print(head(inputweights(ginput))) # weights
#' 
#' # ---
#' \dontrun{
#' 
#' ### EXAMPLE 2
#' ## geva.read.tables example with three tab-delimited files
#' 
#' # Table file examples. Each one has 3 columns: "logfc", "pvalues", and "genesymbol"
#' # Replace it with your tab-delimited files (e.g. exported from limma's topTable)
#' fnames <- c("dt1.txt", "dt2.txt", "dt3.txt")
#' 
#' ginput <- geva.read.tables(fnames,
#'                            col.values="logfc",
#'                            col.pvals="pvalues",
#'                            col.other="genesymbol")
#' 
#' # Prints the first rows from the merged table
#' print(head(ginput))               # values
#' print(head(inputweights(ginput))) # weights
#' 
#' 
#' # ---
#' 
#' ### EXAMPLE 3
#' ## geva.read.tables example with tab-delimited files in a directory
#' 
#' # Directory name (replace it with a directory containing the table files)
#' dirnm <- "C:/User/robertplant123/Documents/R/gevaexamples"
#' 
#' # In this example, table files contain 3 columns: "logfc", "pvalues", and "genesymbol"
#' # Reads all txt files in the directory
#' ginput <- geva.read.tables(dirname=dirnm,
#'                            col.values="logfc",
#'                            col.pvals="pvalues",
#'                            col.other="genesymbol")
#' 
#' # (Optional step)
#' # Let's assume that all table file names start with "dt" and ends with the ".txt" extension,
#' # such as dt1.txt, dt2.txt and so on...
#' fname_pattern <- c("^dt.+?\\.txt$")  # Defines a RegEx pattern to find the files
#' # Loads only files that match the file name pattern
#' ginput <- geva.read.tables(dirname=dirnm,
#'                            files.pattern=fname_pattern,
#'                            col.values="logfc",
#'                            col.pvals="pvalues",
#'                            col.other="genesymbol")
#' 
#' # Prints the first rows from the merged table
#' print(head(ginput))               # values
#' print(head(inputweights(ginput))) # weights
#' }
#' 
#' @name geva.merge.input
#' @rdname geva.merge.input
#' @export
geva.merge.input <- function(..., col.values="logFC", col.pvals="adj.P.Val", col.other=NULL)
{
  if (...length() == 0L) return(NULL)
  argls = list(...)
  names(argls) = call.dots.namesorargs(...)
  argls = list.flatten(argls, ignore.class = c('geva.promise.read.path', 'data.frame'))
  df = NULL
  dw = NULL
  da = NULL
  verbose = ...arg(verbose, TRUE)
  lsfacts = list()
  na.val = ...arg(na.val, 0)
  if (!is.na(na.val))
    assert.operator(na.val, `>=` = 0, `<=` = 1)
  use.regex = ...arg(use.regex, FALSE)
  if (!identical(use.regex, TRUE))
    use.regex = FALSE
  nms = names(argls)
  if (is.null(nms))
    nms = paste0("arg", as.character(seq_along(argls)))
  nms[nchar(nms) == 0L] = "arg"
  nms = make.unique(nms, sep="_")
  for (i in seq_along(argls))
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
    }
    if (is.data.frame(arg))
    {
      sel.vals = if (use.regex) {rowAnys(sapply(col.values, grepl, x=colnames(arg), perl=TRUE))} else {colnames(arg) %in% col.values}
      if (!any(sel.vals))
      {
        warning(sprintf("No column %s '%s' was found in %s",
                        ifelse(use.regex, "matching", "named"),
                        strconjunct(col.values, strvec = "', '", conj="' or '"), argcall))
        next
      }
      sel.ws = if (use.regex) {rowAnys(sapply(col.pvals, grepl, x=colnames(arg), perl=TRUE))} else {colnames(arg) %in% col.pvals}
      if (!any(sel.ws))
      {
        warning(sprintf("No column %s '%s' was found in %s. Using %s as replacement for weights",
                        ifelse(use.regex, "matching", "named"),
                        strconjunct(col.values, strvec = "', '", conj="' or '"), argcall, na.val))
      }
      else if (sum(sel.ws) != sum(sel.vals))
      {
        sel.vals = seq(1L, length.out = length(sel.vals)) %in% which(sel.vals)[1]
        sel.ws = seq(1L, length.out = length(sel.ws)) %in% which(sel.ws)[1]
      }
      sel.attrs = if (use.regex) {rowAnys(sapply(col.other, grepl, x=colnames(arg), perl=TRUE))} else {colnames(arg) %in% col.other}
      if (!is.null(da))
        sel.attrs[colnames(arg) %in% colnames(da)] = FALSE
      arg.values = arg[, sel.vals, drop=FALSE]
      if (!(is.numeric(arg.values) || all(sapply(seq.int(ncol(arg.values)), is.numeric))))
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
  facts = as.factor(unlist(lsfacts))
  info = list(values.column = col.values, pvalues.column = col.pvals)
  ginput = new('GEVAInput', values=as.matrix(df), weights=as.matrix(dw), ftable = da, info = info, factors=facts)
  ginput
}

# Processes data for GEVA using table files as input
#' 
#' 
#' @details The function `geva.merge.input` reads multiple tab-delimited text files containing, extracts the *logFC* columns from each table and merges into a single [`GEVAInput-class`] dataset.
#' 
#' @param filenames `character` vector with two or more file paths
#' @param dirname single `character`, base directory containing the input files. Ignored if `filenames` is specified
#' @param files.pattern single `character`, pattern used to filter the files inside `dirname`. Ignored if `filenames` is specified
#' @param p.value.cutoff `numeric` (`0` to `1`), initial p-value threshold. Rows entirely composed by p-values above this cutoff (*i.e.*, no significant *logFC*) are removed after the final merge. Ignored if `NA` or `NULL`
#' @param read.args `list` of additional arguments passed to [utils::read.table]
#' 
#' @aliases geva.read.tables
#' @rdname geva.merge.input
#' @export
geva.read.tables <- function(filenames=NULL, dirname=".", col.values="logFC", col.pvals="adj.P.Val", col.other=NULL, ..., files.pattern = "\\.txt$", p.value.cutoff=0.05, read.args = list())
{
  if (is.null(p.value.cutoff) || is.na(p.value.cutoff[1]))
    p.value.cutoff = 1
  assert.operator(p.value.cutoff, `>=` = 0, `<=` = 1)
  if (length(filenames) == 0L || dir.exists(filenames[1]) ) filenames = list.files(dirname, full.names = T, all.files = F, recursive = F, include.dirs = F, pattern = files.pattern)
  assert.notempty(filenames)
  verbose = ...arg(verbose, TRUE)
  fcount = length(filenames)
  if (fcount <= 1L) stop("GEVA requires at least two tables of comparison results")
  lsinput = lapply(setNames(filenames, basename(filenames)), function(fnm)
  {
    fls = list(path=fnm, read.args=read.args)
    class(fls) = 'geva.promise.read.path'
    fls
  })
  ginput = geva.merge.input(lsinput, ..., col.values=col.values, col.pvals=col.pvals, col.other=col.other)
  vprint("Read %d columns with %d probes", ncol(ginput), nrow(ginput))
  if (p.value.cutoff < 1)
    ginput = geva.input.filter(ginput, p.value.cutoff=p.value.cutoff, ...)
  ginput
}


# Corrects the input data, removing rows containing invalid values (NA, Inf)
#' @title GEVA Input Post-processing
#' 
#' @description Helper functions used to edit the contents from a [`GEVAInput-class`].
#' 
#' @param ginput A [`GEVAInput-class`] object
#' @param na.rm `logical`; if `TRUE`, removes all rows containing `NA`
#' @param inf.rm `logical`; if `TRUE`, removes all rows containing infinite values (`Inf` or `-Inf`)
#' @param invalid.col.rm `logical`; if `TRUE`, searches for any column that is entirely composed by invalid values (according to the other arguments) and removes it before checking the rows
#' 
#' @return A modified [`GEVAInput-class`] object
#' 
#' @details `geva.input.correct` corrects the numeric input data (values and weights), removing rows that include invalid values such as NA or infinite.
#' 
#' @examples
#' ## geva.input.correct example
#' colexample1 <- runif(1000, -1, 1)        # Random column 1
#' colexample2 <- runif(1000, -1, 1)        # Random column 2
#' colexample3 <- runif(1000, -1, 1)        # Random column 3
#' colexample3[runif(1000, -1, 1) < 0] = NA # Random NA's
#' ginput = geva.merge.input(col1=colexample1,
#'                           col2=colexample2,
#'                           col3=colexample3)
#' # Before the correction:
#' print(nrow(ginput))    # Returns 1000
#' # Applies the correction (removes rows with NA's)
#' ginput <- geva.input.correct(ginput)
#' # After the correction:
#' print(nrow(ginput))    # Returns less than 1000
#' 
#' @rdname geva.input.correct
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

# Filters the input data, removing rows containing p.values (1 - weights) below a threshold
#' 
#' @param p.value.cutoff `numeric` (`0` to `1`), the p-value cutoff. Rows containing values above this threshold are removed
#' @param by.any `logical`, set to `TRUE` to delete the rows with at least one occurrence above the cutoff; or `FALSE` to delete only those rows in which all values are above the specified threshold
#' @param na.val `numeric`, the replacement for `NA` values
#' 
#' @details `geva.input.filter` attempts to select the most relevant part of the input data, removing rows containing p.values (1 - weights) above a specific threshold.
#' 
#' @examples
#' ## ---
#' ## geva.input.filter example
#' ginput <- geva.ideal.example(1000)  # Generates a random input
#' # Before the filter:
#' print(nrow(ginput))    # Returns 1000
#' # Applies the filter
#' ginput <- geva.input.filter(ginput)
#' # After the filter:
#' print(nrow(ginput))    # Returns less than 1000
#' 
#' @aliases geva.input.filter
#' @rdname geva.input.correct
#' @export
geva.input.filter <- function(ginput, p.value.cutoff=0.05, by.any=FALSE, na.val=0, ...)
{
  # Removes the rows where all p-values are above the cutoff
  dw = inputweights(ginput)
  sel.remove = 1L:nrow(ginput) %in% which.rows.outside.cutoff(1 - dw,
                                                              cutoff=1-p.value.cutoff,
                                                              na.val=na.val,
                                                              by.any=identical(by.any, TRUE))
  remcount = sum(sel.remove)
  if (remcount != 0L)
  {
    ginput = ginput[!sel.remove,,drop=FALSE]
    vprint("Removed %s rows where all p-values were above the cutoff (%s)", remcount, as.character(p.value.cutoff))
  }
  ginput
}

# Replaces the row.names from a GEVAInput based on a attribute column or a character vector
#' @param attr.column `character`, target column with the values that will replace the current row names
#' @param dupl.rm.method `character`, method to remove duplicate names. The possible options are:
#' \itemize{
#' \item{`"least.p.vals"` : Keeps the duplicate that contains the least sum of p-values}
#' \item{`"order"` : Keeps the first occurrence of the duplicate in the current row order}
#' }
#' 
#' @details `geva.input.rename.rows` replaces the row names with a column from the feature table (see [`GEVAInput-class`]). The column name specified for `attr.column` must be included in the `names(featureTable(ginput))`. Any duplicates are removed according to the `dupl.rm.method`, and the selected duplicates are stored as a new column named `"renamed_id"` inside the feature table from the returned object.
#' 
#' @examples
#' ## ---
#' ## geva.input.rename.rows example
#' ginput <- geva.ideal.example()  # Generates a random input
#' # Renames to 'Symbol'
#' ginput <- geva.input.rename.rows(ginput,
#'                                  attr.column = "Symbol")
#' print(head(ginput))             # The row names are set now as the gene symbols
#' 
#' @aliases geva.input.rename.rows
#' @rdname geva.input.correct
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
    sel.dupls[vdinds] = gtapply(vsumpvals, fdgroups, function(x) seq_along(x) != which.min(x))
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
