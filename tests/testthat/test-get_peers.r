# Load data
frontier41 <- readRDS('../testdata/frontier41.RDS')
f41_x <- as.matrix(frontier41[c('capital', 'labour')])
f41_y <- as.matrix(frontier41[c('output')])

norCourts2018 <- readRDS('../testdata/norCourts2018.RDS')
nc_x <- as.matrix(norCourts2018[c('judges', 'case_workers', 'costs')])
nc_y <- as.matrix(norCourts2018[c(
  'civil_cases', 'criminal_case_single',
  'criminal_case_full_bench',
  'other_civil_cases')])


test_that('get_peers() gives the correct results', {

  # --- Basic Frontier 4.1 model --- #

  res <- compute_efficiency(f41_x, f41_y, type = 'crs', orientation = 'out', digits = NULL)
  df <- get_peers(res$lambda, ids = frontier41$firm)
  res2 <- Benchmarking::dea(f41_x, f41_y, RTS = 'crs', ORIENTATION = 'out')
  df2 <- Benchmarking::peers(res2)
  expect_identical(df$peer1, df2[,1])
  expect_identical(df$peer2, df2[,2])

  # --- Full district court model --- #

  res <- compute_efficiency(nc_x, nc_y, type = 'vrs', orientation = 'out', digits = NULL)
  df <- get_peers(res$lambda, ids = norCourts2018$district_court)
  res2 <- Benchmarking::dea(nc_x, nc_y, RTS = 'vrs', ORIENTATION = 'out')
  colnames(res2$lambda) <- norCourts2018$district_court
  df2 <- Benchmarking::peers(res2, NAMES = TRUE)
  expect_identical(df$peer1, df2[,1])
  expect_identical(df$peer2, df2[,2])
  expect_identical(df$peer3, df2[,3])
  expect_identical(df$peer4, df2[,4])
  expect_identical(df$peer4, df2[,4])
  expect_identical(df$peer5, df2[,5])
  expect_identical(df$peer6, df2[,6])
  expect_identical(df$peer7, df2[,7])
})
