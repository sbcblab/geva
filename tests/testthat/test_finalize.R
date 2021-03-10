
test_that(paste("geva.finalize and geva.quick are correctly",
                "finishing the analysis"),
          {
            ginput = geva.ideal.example.with.seed(seed = 1L)
            gsummary = geva.summarize(ginput)
            gcluster = geva.hcluster(gsummary)
            gquants = geva.quantiles(gsummary, quantile.method = 'density')
            gres = geva.finalize(gsummary, gcluster, gquants)
            expect_s4_class(gres, 'GEVAResults')
            gres = geva.quick(gres, quantile.method = 'range.slice')
            expect_s4_class(gres, 'GEVAResults')
            catline("Testing plot with GEVAResults")
            plot(gres)
            TRUE
          })

