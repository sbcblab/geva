
##########################
# PLOTTING HELPER FUNCTIONS
# -----------------------
# 
# Helper methods for data plotting
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

#' @include vectorhelpers.R

# Generates random colors distributed by hue
generate.colors <- function(n=1L, h.range = c(0, 1) , s.range = c(0.8, 1), v.range = c(0.8, 1))
{
  h.range = h.range * c(1, 1)
  s.range = s.range * c(1, 1)
  v.range = v.range * c(1, 1)
  hsize = max(h.range) - min(h.range)
  h = (seq(from=0, to = hsize, length.out = n + 1L)[-1] + runif(n, 0, hsize / (n * 2) )) %% hsize
  h = h + min(h.range)
  s = runif(n, s.range[1], s.range[2])
  v = runif(n, v.range[1], v.range[2])
  hsv(h, s, v)
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
  rngs = max(abs(summary(x))) * c(-1, 1)
  rngv = max(abs(variation(x))) * c(0, 1)
  defargs = list(xlim = rngs,
                 ylim = rngv)
  defargs
}

# Creates a hull plot of the current data
hull.plot <- function (x, cl, col = NULL, hull_lwd = 1, hull_lty = 1, alpha = 0.2, ...)
{
  if (is.list(cl)) 
  {
    cl = cl$cluster
  }
  if (is.factor(cl))
  {
    cl = as.integer(cl)
  }
  if (!is.numeric(cl))
    stop("clusters must be specified as a factor or an integer vector in cl.")
  assert.dim(cl, length=nrow(x))
  if (is.null(col))
    col = generate.colors(max(cl))[cl]
  call.plot(x[, 1:2], col = col, ...)
  col_poly = adjustcolor(col, alpha.f = alpha)
  border = col
  if (is.null(hull_lwd) || is.na(hull_lwd) || hull_lwd == 0)
  {
    hull_lwd = 1
    border = NA
  }
  ci_order = 1:max(cl)
  for (i in 1:length(ci_order))
  {
    d = x[cl == i, , drop=F]
    ch = chull(d)
    ch = c(ch, ch[1])
    polygon(d[ch,,drop=F ], col = col_poly[cl == i], lwd = hull_lwd, lty = hull_lty, border = border[cl == i])
  }
}
