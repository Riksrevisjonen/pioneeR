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

test_that('compute_efficiency() works for CRS', {

  # orientation in
  res <- compute_efficiency(f41_x, f41_y, type = 'crs', orientation = 'in', digits = NULL)
  res2 <- Benchmarking::dea(f41_x, f41_y, RTS = 'crs', ORIENTATION = 'in')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$unadj_values, res2$objval) # unadjusted efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation out
  res <- compute_efficiency(f41_x, f41_y, type = 'crs', orientation = 'out', digits = NULL)
  res2 <- Benchmarking::dea(f41_x, f41_y, RTS = 'crs', ORIENTATION = 'out')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$unadj_values, res2$objval) # unadjusted efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation in
  res <- compute_efficiency(nc_x, nc_y, type = 'crs', orientation = 'in', digits = NULL)
  res2 <- Benchmarking::dea(nc_x, nc_y, RTS = 'crs', ORIENTATION = 'in')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$unadj_values, res2$objval) # unadjusted efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation out
  res <- compute_efficiency(nc_x, nc_y, type = 'crs', orientation = 'out', digits = NULL)
  res2 <- Benchmarking::dea(nc_x, nc_y, RTS = 'crs', ORIENTATION = 'out')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$unadj_values, res2$objval) # unadjusted efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

})

test_that('compute_efficiency() works for VRS', {

  # orientation in
  res <- compute_efficiency(f41_x, f41_y, type = 'vrs', orientation = 'in', digits = NULL)
  res2 <- Benchmarking::dea(f41_x, f41_y, RTS = 'vrs', ORIENTATION = 'in')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$unadj_values, res2$objval) # unadjusted efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation out
  res <- compute_efficiency(f41_x, f41_y, type = 'vrs', orientation = 'out', digits = NULL)
  res2 <- Benchmarking::dea(f41_x, f41_y, RTS = 'vrs', ORIENTATION = 'out')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$unadj_values, res2$objval) # unadjusted efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

  # Real data

  # orientation in
  res <- compute_efficiency(nc_x, nc_y, type = 'vrs', orientation = 'in', digits = NULL)
  res2 <- Benchmarking::dea(nc_x, nc_y, RTS = 'vrs', ORIENTATION = 'in')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$unadj_values, res2$objval) # unadjusted efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation out
  res <- compute_efficiency(nc_x, nc_y, type = 'vrs', orientation = 'out', digits = NULL)
  res2 <- Benchmarking::dea(nc_x, nc_y, RTS = 'vrs', ORIENTATION = 'out')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$unadj_values, res2$objval) # unadjusted efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

})

test_that('compute_efficiency() works for IRS', {

  # orientation in
  res <- compute_efficiency(f41_x, f41_y, type = 'irs', orientation = 'in', digits = NULL)
  res2 <- Benchmarking::dea(f41_x, f41_y, RTS = 'irs', ORIENTATION = 'in')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$unadj_values, res2$objval) # unadjusted efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation out
  res <- compute_efficiency(f41_x, f41_y, type = 'irs', orientation = 'out', digits = NULL)
  res2 <- Benchmarking::dea(f41_x, f41_y, RTS = 'irs', ORIENTATION = 'out')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$unadj_values, res2$objval) # unadjusted efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation in
  res <- compute_efficiency(nc_x, nc_y, type = 'irs', orientation = 'in', digits = NULL)
  res2 <- Benchmarking::dea(nc_x, nc_y, RTS = 'irs', ORIENTATION = 'in')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$unadj_values, res2$objval) # unadjusted efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation out
  res <- compute_efficiency(nc_x, nc_y, type = 'irs', orientation = 'out', digits = NULL)
  res2 <- Benchmarking::dea(nc_x, nc_y, RTS = 'irs', ORIENTATION = 'out')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$unadj_values, res2$objval) # unadjusted efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

})

test_that('compute_efficiency() works for DRS', {

  # orientation in
  res <- compute_efficiency(f41_x, f41_y, type = 'drs', orientation = 'in', digits = NULL)
  res2 <- Benchmarking::dea(f41_x, f41_y, RTS = 'drs', ORIENTATION = 'in')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$unadj_values, res2$objval) # unadjusted efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation out
  res <- compute_efficiency(f41_x, f41_y, type = 'drs', orientation = 'out', digits = NULL)
  res2 <- Benchmarking::dea(f41_x, f41_y, RTS = 'drs', ORIENTATION = 'out')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$unadj_values, res2$objval) # unadjusted efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation in
  res <- compute_efficiency(nc_x, nc_y, type = 'drs', orientation = 'in', digits = NULL)
  res2 <- Benchmarking::dea(nc_x, nc_y, RTS = 'drs', ORIENTATION = 'in')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$unadj_values, res2$objval) # unadjusted efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation out
  res <- compute_efficiency(nc_x, nc_y, type = 'drs', orientation = 'out', digits = NULL)
  res2 <- Benchmarking::dea(nc_x, nc_y, RTS = 'drs', ORIENTATION = 'out')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$unadj_values, res2$objval) # unadjusted efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

})

test_that('compute_efficiency() works when a reference techology set is used', {

  # Simple example (taken from https://pystoned.readthedocs.io/en/latest/examples/DEA/dea_ref.html)
  x <- as.matrix(c(100,200,300,500,100,200,600,400,550,600))
  y <- as.matrix(c(75,100,300,400,25,50,400,260,180,240))
  xref <- as.matrix(c(100,300,500,100,600))
  yref <- as.matrix(c(75,300,400,25,400))

  res <- compute_efficiency(x, y, xref, yref, type = 'vrs', orientation = 'out', digits = NULL)
  res2 <- Benchmarking::dea(x, y, XREF = xref, YREF = yref, RTS = 'vrs', ORIENTATION = 'out')
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$eff) # efficiency
  expect_equal(res$unadj_values, res2$objval) # unadjusted efficiency
  expect_equal(res$lambda, res2$lambda) # lambda

})
