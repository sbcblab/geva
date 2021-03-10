
test_that("geva.summarize is correctly summarizing the input with all the parameters",
          {
            ginput = geva.ideal.example.with.seed(seed = 1L)
            for (param in options.summary)
            {
              catline("Testing with summary.method = '%s'", param)
              gsummary = geva.summarize(ginput, summary.method = param)
              expect_s4_class(gsummary, 'GEVASummary')
            }
            for (param in options.variation)
            {
              catline("Testing with variation.method = '%s'", param)
              gsummary = geva.summarize(ginput, variation.method = param)
              expect_s4_class(gsummary, 'GEVASummary')
            }
            catline("Testing plot with GEVASummary")
            plot(gsummary)
            TRUE
          })
