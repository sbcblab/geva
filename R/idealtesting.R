
##########################
# IDEAL TESTING METHODS
# -----------------------
# 
# Functions to generate ideal data to perform tests with GEVA
# The ideal data is for testing only and has no biological meaning
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al

# Gauss helper function for internal use
rgauss <- function(n, sigma=n/5, peak=0.5)
{
  n = as.integer(n)
  if (n <= 2) return(rep(0.5, n))
  if (peak < 0) peak = 0 else if (peak > 1) peak = 1
  mu = (n - 1) * peak
  x = 0L:(n - 1L)
  vres = vapply(x, function(xi) exp(-0.5*((xi-mu)/sigma)^2), NA_real_)
  rng = range(vres)
  if (rng[1] == rng[2]) return(0.5)
  vres = (vres - rng[1]) / (rng[2] - rng[1])
  vres
}

#' @title GEVA ``Ideal'' Example for Package Testing
#' 
#' @description Generates a random example of GEVAInput object that simulates an ideal analysis dataset. Used for testing purposes only.
#' 
#' @param probecount `integer`, number of probes (*i.e.*, table rows)
#' @param nfactors `integer`, number of factors (*e.g.*, experimental groups)
#' @param colsperfactor `integer`, number of columns (*e.g.*, experiments) per factor
#' 
#' @return A [`GEVAInput-class`] object. The included tables are composed by `probecount` rows and `nfactors` * `colsperfactor` columns
#' 
#' @examples
#' ## "Ideal" input example
#' ginput <- geva.ideal.example()     # Generates a random example
#' gsummary <- geva.summarize(ginput) # Summarizes the generated data
#' plot(gsummary)                     # Plots the summarized data
#' 
#' @seealso [`geva.summarize`]
#' 
#' @export
geva.ideal.example <- function(probecount=10000, nfactors=3, colsperfactor=3)
{
  if (probecount <= 0) stop("probecount must be a positive value")
  probnms = paste0("probe_", seq_len(probecount))
  dtls = list()
  dtlfc = data.frame(row.names = probnms)
  dtpval = data.frame(row.names = probnms)
  vi = seq(from=0, to=1 + 1 / probecount, by = 1 / (probecount - 1))[seq_len(probecount)]
  wi = (rgauss(probecount, probecount/1, 0.18) * 0.02 + rgauss(probecount, probecount/64, 0.12)) / 1.02
  condnms = rep("", nfactors * colsperfactor)
  smpcntrng = as.integer(ceiling(c(probecount / 1000, probecount / 20)))
  for(ci in seq_len(nfactors))
  {
    vlfcbase = rnorm(probecount, 0, 1) #runif(probecount, -0.5, 0.5)
    zi = sample(vi, probecount, replace = TRUE, prob = wi) * runif(1, 6, 12)
    vpvalbase = runif(probecount, 0, 1)^8
    vlfcchange = rnorm(probecount, 0, 1) * zi  #(runif(probecount, 0, 1)^500 + (-1 * runif(probecount, 0, 1)^500))
    smpinds = sample(seq.int(probecount), sample(smpcntrng[1]:smpcntrng[2], 1), replace = FALSE)
    condname = sprintf("Cond_%d", ci)
    for (ri in seq_len(colsperfactor))
    {
      vlfc = (vlfcbase + vlfcchange * runif(1, 0.1, 1)) * 0.5
      vlfc[smpinds] = vlfc[smpinds] + rnorm(length(smpinds), 0, 0.5)
      vpval = vpvalbase * runif(probecount, 0.8, 1)
      currcondname = sprintf("%s%s", condname, LETTERS[ri])
      dtlfc[, currcondname] = vlfc
      dtpval[, currcondname] = vpval
      condnms[ncol(dtlfc)] = condname
    }
  }
  infols = list(column.weight='adj.P.Val')
  dtfeats = data.frame(row.names=probnms, Symbol=paste0("GENE_", LETTERS, seq_len(probecount)))
  ginput = new('GEVAInput', values=as.matrix(dtlfc),
               weights=as.matrix(dtpval), factors=as.factor(condnms),
               ftable=dtfeats, info=infols)
  ginput
}