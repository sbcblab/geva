
test_that(paste("geva.cluster and geva.quantiles are correctly",
                 "grouping the input with all the parameters"),
          {
            ginput = geva.ideal.example.with.seed(seed = 1L)
            gsummary = geva.summarize(ginput)
            for (param in setdiff(options.cluster.method, 'quantiles'))
            {
              catline("Testing with cluster.method = '%s'", param)
              gcluster = geva.cluster(gsummary, cluster.method = param,
                                      grouped.return = FALSE)
              expect_s4_class(gcluster, 'GEVACluster')
            }
            catline("Testing quantiles grouping")
            for (param in options.quantiles)
            {
              catline("Testing with quantile.method = '%s'", param)
              if (identical(param, "custom"))
                gquants = geva.quantiles(gsummary,
                                         initial.thresholds = c(S=0.5, V=0.5))
              else
                gquants = geva.quantiles(gsummary, quantile.method = param)
              expect_s4_class(gquants, 'GEVAQuantiles')
            }
            catline("Testing plot with GEVACluster and GEVAQuantiles")
            plot(gcluster)
            plot(gquants)
            plot(gcluster, gquants)
            plot(gquants, gcluster)
            TRUE
          })

