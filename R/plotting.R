
##########################
# PLOTTING HELPER FUNCTIONS
# -----------------------
# 
# Helper methods for data plotting
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al

#' @include vectorhelpers.R
NULL

# Generates random colors distributed by hue
generate.colors <- function(n=1L, h.range = c(0, 1) , s.range = c(0.8, 1), v.range = c(0.8, 1))
{
  if (n <= 0L) return(character(0))
  h.range = h.range * c(1, 1)
  s.range = s.range * c(1, 1)
  v.range = v.range * c(1, 1)
  hsize = max(h.range) - min(h.range)
  h = (seq(from=0, to = hsize, length.out = n + 1L)[-1] + runif(n, 0, hsize / (n * 2) )) %% hsize
  h = h + min(h.range)
  h = sample(h, length(h))
  s = runif(n, s.range[1], s.range[2])
  v = runif(n, v.range[1], v.range[2])
  hsv(h, s, v)
}


# Generates random colors based on a factor
generate.colors.byfactor <- function(x, ..., clrs=NULL)
{
  if (!is.factor(x))
    stop("'x' must be a factor")
  lvls = levels(x)
  if (is.null(clrs) || length(clrs) == 0L)
    clrs = generate.colors(length(lvls))[x]
  else if (length(clrs) != length(x))
  {
    if (length(clrs) > length(lvls) && length(unique(clrs)) == length(lvls))
      clrs = unique(clrs)[x]
    else if (length(clrs) >= length(lvls))
      clrs = clrs[x]
    else if (length(clrs) == 1L)
      clrs = rep(clrs, length(x))
    else
      clrs = rep(clrs, ceiling(length(lvls) / length(clrs)))[x]
  }
  clrs = color.adjust(clrs=clrs, ...)
  clrs
}

color.adjust <- function(clrs, na.color="#666666", alpha=1, ...)
{
  if (length(clrs) == 0L) return(character(0))
  clrs = na.replace(clrs, na.color)
  clrs = sub("(?i)^\\#([0-9a-f])([0-9a-f])([0-9a-f])$", "#\\1\\1\\2\\2\\3\\3", trimws(clrs), perl=TRUE)
  if (any(alpha != 1, na.rm = TRUE))
  {
    if (length(alpha) == 1L)
      alpha = rep(alpha, length(clrs))
    else
      assert.dim(alpha, length=length(clrs))
    alpha = clamp(alpha, min.value = 0, max.value = 1)
    clrpatt = "(?i)^(\\#(?:[0-9a-f]{6}))(?:[0-9a-f]{2})?$"
    if (length(unique(alpha)) == 1L)
    {
      clrs = sub(clrpatt, sprintf("\\1%02X", round(alpha[1] * 255)), clrs, perl = TRUE)
    }
    else
    {
      alpha.reps = sprintf("\\1%02X", round(alpha * 255))
      clrmat = matrix(c(clrs, alpha.reps), ncol=2)
      clrs = apply(clrmat, 1, function(r)
        sub(pattern=clrpatt,
            replacement=r[2L], x = r[1L], perl = TRUE))
    }
  }
  
  clrs
}

# Filter the plot arguments, accepting only the supported arguments in plot function
filter.plotargs <- function(plotargs, accept=character(0))
{
  if (length(plotargs) == 0 || !is.named(plotargs)) return(list())
  validargs = c(names(graphics::par(no.readonly = TRUE)),
                names(formals(plot.default)),
                accept)
  argnms = names(plotargs)
  plotargs = plotargs[!duplicated(argnms) & (argnms %in% validargs)]
  plotargs
}

# Calls the plot method by merging multiple argument lists
call.plot <- function(x, ..., defargs=list(), plotfn=plot)
{
  argls = list.merge(list(x=x), list(...), defargs)
  argls = filter.plotargs(argls, accept = unique(c(names(defargs), names(formals(plotfn)))))
  do.call(plotfn, argls)
}

# Gets the default arguments used to plot proportional SV data
plotargs.sv.proportional <- function(x)
{
  if (inherits(x, 'SVTable'))
  {
    vsum = summary(x)
    vvar = variation(x)
  }
  else
  {
    vsum = x[,1,drop=TRUE]
    vvar = x[,2,drop=TRUE]
  }
  rngs = max(abs(vsum)) * c(-1, 1)
  rngv = max(abs(vvar)) * c(0, 1)
  defargs = list(xlim = rngs,
                 ylim = rngv)
  defargs
}



# Creates the convex polygons for grouped points
hull.lines <- function(x, cl, col = NULL, hull_lwd = 1, hull_lty = 1, alpha = 0.2, border.visible=TRUE, na.color="#666666", ...)
{
  if (!is.factor(cl))
  {
    cl = as.factor(cl)
  }
  lvls = levels(cl)
  if (is.null(col))
    col = generate.colors.byfactor(cl, na.color)
  col_poly = adjustcolor(col, alpha.f = alpha)
  border = col
  if (is.null(hull_lwd) || is.na(hull_lwd) || hull_lwd == 0)
  {
    hull_lwd = 1
    border = NA
  }
  for (lvl in lvls)
  {
    sel.cl = cl %in% lvl
    d = x[sel.cl, , drop=F]
    ch = chull(d)
    ch = c(ch, ch[1])
    borderv = if (border.visible) border[sel.cl] else NA
    polygon(d[ch,,drop=F ], col = col_poly[sel.cl], lwd = hull_lwd, lty = hull_lty, border = borderv)
  }
  invisible()
}

# Creates a hull plot of the current data
hull.plot <- function (x, cl, col = NULL, ...)
{
  if (is.null(dim(x)) || ncol(x) < 2)
    stop("the 'x' parameter must be a bidimensional array with at least two columns")
  if (is.list(cl))
  {
    cl = cl$cluster
  }
  if (!is.factor(cl))
  {
    cl = as.factor(cl)
  }
  if (!is.factor(cl))
    stop("clusters must be specified as a factor or an integer vector in cl.")
  assert.dim(cl, length=nrow(x))
  if (is.null(col))
    col = generate.colors.byfactor(cl)
  call.plot(x[, 1:2], col = col, ...)
  hull.lines(x, cl, col, ...)
  invisible()
}

# S3 Methods
color.values.vector <- function(x, ...) generate.colors.byfactor(as.factor(x), ...)
color.values.factor <- function(x, alpha=1, ...) generate.colors.byfactor(x, alpha=alpha, ...)
