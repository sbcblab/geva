
test_that("geva.read.tables is correctly reading input text files",
         {
           ginput = geva.ideal.example.with.seed(seed = 1L)
           expect_s4_class(ginput, "GEVAInput")
           tmppaths = character(ncol(ginput))
           for(i in seq_len(ncol(ginput)))
           {
             tmppath = tempfile(pattern = 'gevainput', fileext = '.txt')
             tmppaths[i] = tmppath
             df = data.frame(ID=rownames(ginput),
                             foldchange = inputvalues(ginput)[,i],
                             pvalues=inputweights(ginput)[,i],
                             Symbol=featureTable(ginput)$Symbol)
             write.table(df, file = tmppath, sep = '\t',
                         row.names = FALSE, quote = FALSE)
           }
           ginput2 = geva.read.tables(filenames = tmppaths,
                                      col.values = 'foldchange',
                                      col.pvals = 'pvalues',
                                      col.other = 'Symbol')
           expect_equal(ncol(featureTable(ginput2)), 1L)
           TRUE
         })
