
##########################
# IDEAL TESTING METHODS
# -----------------------
# 
# Functions to generate ideal data to perform tests with GEVA
# 
# ########################
# Nunes et al, 2020
# Last updated version: 0.1.0

rgauss = function(n, sigma=n/5, peak=0.5)
{
  n = as.integer(n)
  if (n <= 2) return(rep(0.5, n))
  if (peak < 0) peak = 0 else if (peak > 1) peak = 1
  mu = (n - 1) * peak
  x = 0:(n - 1)
  vres = sapply(x, function(xi) exp(-0.5*((xi-mu)/sigma)^2))
  rng = range(vres)
  if (rng[1] == rng[2]) return(0.5)
  vres = (vres - rng[1]) / (rng[2] - rng[1])
  vres
}

# Generates a ideal example of GEVAInput. For testing only
#' @export
geva.ideal.example <- function(probecount=10000, condcount=3, respercond=3, seed=NA_integer_)
{
  if (!is.na(seed)) set.seed(seed)
  probnms = paste0("probe_", 1:probecount)
  dtls = list()
  dtlfc = data.frame(row.names = probnms)
  dtpval = data.frame(row.names = probnms)
  vi = seq(from=0, to=1 + 1 / probecount, by = 1 / (probecount - 1))[1:probecount]
  wi = (rgauss(probecount, probecount/1, 0.18) * 0.02 + rgauss(probecount, probecount/64, 0.12)) / 1.02
  condnms = rep("", condcount * respercond)
  smpcntrng = as.integer(ceiling(c(probecount / 1000, probecount / 20)))
  for(ci in 1:condcount)
  {
    vlfcbase = rnorm(probecount, 0, 1) #runif(probecount, -0.5, 0.5)
    zi = sample(vi, probecount, replace = T, prob = wi) * runif(1, 6, 12)
    vpvalbase = runif(probecount, 0, 1)^8
    vlfcchange = rnorm(probecount, 0, 1) * zi  #(runif(probecount, 0, 1)^500 + (-1 * runif(probecount, 0, 1)^500))
    smpinds = sample(1:probecount, sample(smpcntrng[1]:smpcntrng[2], 1), replace = F)
    condname = sprintf("Cond_%d", ci)
    for (ri in 1:respercond)
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
  ginput = new('GEVAInput', values=as.matrix(dtlfc), weights=as.matrix(dtpval), factors=as.factor(condnms), info=infols)
  ginput
}