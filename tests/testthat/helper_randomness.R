

geva.ideal.example.with.seed <- function(probecount=10000, nfactors=3, colsperfactor=3, seed=1L)
{
  if (!exists('.Random.seed'))
  {
    set.seed(seed)
    return(geva.ideal.example(probecount = probecount, nfactors = nfactors, colsperfactor = colsperfactor))
  }
  orig.seed = .Random.seed
  on.exit(.Random.seed <<- orig.seed)
  set.seed(seed)
  geva.ideal.example(probecount = probecount, nfactors = nfactors, colsperfactor = colsperfactor)
}
