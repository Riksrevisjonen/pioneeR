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

test_that('compute_super_efficiency() works for CRS', {

  # --- Frontier 4.1 data --- #

  # orientation in
  res <- compute_super_efficiency(f41_x, f41_y, type = 'crs', orientation = 'in', digits = NULL)
  res2 <- Benchmarking::sdea(f41_x, f41_y, RTS = 'crs', ORIENTATION = 'in')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation out
  res <- compute_super_efficiency(f41_x, f41_y, type = 'crs', orientation = 'out', digits = NULL)
  res2 <- Benchmarking::sdea(f41_x, f41_y, RTS = 'crs', ORIENTATION = 'out')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

  # --- Norwegian District Courts --- #

  # orientation in
  res <- compute_super_efficiency(nc_x, nc_y, type = 'crs', orientation = 'in', digits = NULL)
  res2 <- Benchmarking::sdea(nc_x, nc_y, RTS = 'crs', ORIENTATION = 'in')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation out
  res <- compute_super_efficiency(nc_x, nc_y, type = 'crs', orientation = 'out', digits = NULL)
  res2 <- Benchmarking::sdea(nc_x, nc_y, RTS = 'crs', ORIENTATION = 'out')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

})

test_that('compute_super_efficiency() works for VRS', {

  # orientation in
  res <- compute_super_efficiency(f41_x, f41_y, type = 'vrs', orientation = 'in', digits = NULL)
  res2 <- Benchmarking::sdea(f41_x, f41_y, RTS = 'vrs', ORIENTATION = 'in')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation out
  res <- compute_super_efficiency(f41_x, f41_y, type = 'vrs', orientation = 'out', digits = NULL)
  res2 <- Benchmarking::sdea(f41_x, f41_y, RTS = 'vrs', ORIENTATION = 'out')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

  # Real data

  # orientation in
  res <- compute_super_efficiency(nc_x, nc_y, type = 'vrs', orientation = 'in', digits = NULL)
  res2 <- Benchmarking::sdea(nc_x, nc_y, RTS = 'vrs', ORIENTATION = 'in')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation out
  res <- compute_super_efficiency(nc_x, nc_y, type = 'vrs', orientation = 'out', digits = NULL)
  res2 <- Benchmarking::sdea(nc_x, nc_y, RTS = 'vrs', ORIENTATION = 'out')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

})

test_that('compute_super_efficiency() works for IRS', {

  # orientation in
  res <- compute_super_efficiency(f41_x, f41_y, type = 'irs', orientation = 'in', digits = NULL)
  res2 <- Benchmarking::sdea(f41_x, f41_y, RTS = 'irs', ORIENTATION = 'in')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation out
  res <- compute_super_efficiency(f41_x, f41_y, type = 'irs', orientation = 'out', digits = NULL)
  res2 <- Benchmarking::sdea(f41_x, f41_y, RTS = 'irs', ORIENTATION = 'out')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

  # Real data

  # orientation in
  res <- compute_super_efficiency(nc_x, nc_y, type = 'irs', orientation = 'in', digits = NULL)
  res2 <- Benchmarking::sdea(nc_x, nc_y, RTS = 'irs', ORIENTATION = 'in')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation out
  res <- compute_super_efficiency(nc_x, nc_y, type = 'irs', orientation = 'out', digits = NULL)
  res2 <- Benchmarking::sdea(nc_x, nc_y, RTS = 'irs', ORIENTATION = 'out')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

})

test_that('compute_super_efficiency() works for DRS', {

  # orientation in
  res <- compute_super_efficiency(f41_x, f41_y, type = 'drs', orientation = 'in', digits = NULL)
  res2 <- Benchmarking::sdea(f41_x, f41_y, RTS = 'drs', ORIENTATION = 'in')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation out
  res <- compute_super_efficiency(f41_x, f41_y, type = 'drs', orientation = 'out', digits = NULL)
  res2 <- Benchmarking::sdea(f41_x, f41_y, RTS = 'drs', ORIENTATION = 'out')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

  # Real data

  # orientation in
  res <- compute_super_efficiency(nc_x, nc_y, type = 'drs', orientation = 'in', digits = NULL)
  res2 <- Benchmarking::sdea(nc_x, nc_y, RTS = 'drs', ORIENTATION = 'in')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation out
  res <- compute_super_efficiency(nc_x, nc_y, type = 'drs', orientation = 'out', digits = NULL)
  res2 <- Benchmarking::sdea(nc_x, nc_y, RTS = 'drs', ORIENTATION = 'out')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

})
