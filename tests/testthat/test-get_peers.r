# Benchmarking results
benchmarking_results <- readRDS('../testdata/benchmarking-results.RDS')

# Frontier 4.1
frontier41 <- readRDS('../testdata/frontier41.RDS')
f41_x <- as.matrix(frontier41[c('capital', 'labour')])
f41_y <- as.matrix(frontier41[c('output')])

# Norwegian District Courts
norCourts2018 <- readRDS('../testdata/norCourts2018.RDS')
nc_x <- as.matrix(norCourts2018[c('judges', 'case_workers', 'costs')])
nc_y <- as.matrix(norCourts2018[c(
  'civil_cases', 'criminal_case_single',
  'criminal_case_full_bench',
  'other_civil_cases')])

# Electric Plants
electricPlants <- readRDS('../testdata/electricPlants.RDS')
ecp_x <- as.matrix(electricPlants[c('labor', 'fuel', 'capital')])
ecp_y <- as.matrix(electricPlants[c('output')])

# Hospitals
hospitals <- readRDS('../testdata/hospitals.RDS')
hp_x <- as.matrix(hospitals[c('labor', 'capital')])
hp_y <- as.matrix(hospitals[c('inpatients', 'outpatients')])

test_that('get_peers() gives the correct results', {

  # --- Frontier 4.1 data --- #

  bench_res <- benchmarking_results$frontier41$out_crs$efficiency$peers
  res <- compute_efficiency(f41_x, f41_y, rts = 'crs', orientation = 'out')
  df <- get_peers(res$lambda, ids = as.character(frontier41$firm))
  expect_identical(df$peer1, bench_res[,1])
  expect_identical(df$peer2, bench_res[,2])

  # --- Norwegian District Courts --- #

  bench_res <- benchmarking_results$norCourts2018$out_vrs$efficiency$peers
  res <- compute_efficiency(nc_x, nc_y, rts = 'vrs', orientation = 'out')
  df <- get_peers(res$lambda, ids = norCourts2018$district_court)
  expect_identical(df$peer1, bench_res[,1])
  expect_identical(df$peer2, bench_res[,2])
  expect_identical(df$peer3, bench_res[,3])
  expect_identical(df$peer4, bench_res[,4])
  expect_identical(df$peer5, bench_res[,5])
  expect_identical(df$peer6, bench_res[,6])
  expect_identical(df$peer7, bench_res[,7])

  # --- Hospitals --- #

  bench_res <- benchmarking_results$hospitals$out_vrs$efficiency$peers
  res <- compute_efficiency(hp_x, hp_y, rts = 'vrs', orientation = 'out')
  df <- get_peers(res$lambda, ids = as.character(hospitals$firm_id))
  expect_identical(df$peer1, bench_res[,1])
  expect_identical(df$peer2, bench_res[,2])
  expect_identical(df$peer3, bench_res[,3])
  expect_identical(df$peer4, bench_res[,4])

  # --- Electric Plants --- #

  bench_res <- benchmarking_results$electricPlants$out_drs$efficiency$peers
  res <- compute_efficiency(ecp_x, ecp_y, rts = 'drs', orientation = 'out')
  df <- get_peers(res$lambda, ids = electricPlants$plant)
  expect_identical(df$peer1, bench_res[,1])
  expect_identical(df$peer2, bench_res[,2])

})
