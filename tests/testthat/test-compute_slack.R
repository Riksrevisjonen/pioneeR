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

test_that('compute_slack() works for CRS', {

  # orientation in
  eff <- Benchmarking::dea(f41_x, f41_y, RTS = 'crs', ORIENTATION = 'in')
  res <- compute_slack(f41_x, f41_y, eff$objval, type = 'crs', orientation = 'in', digits = NULL)
  res2 <- Benchmarking::slack(f41_x, f41_y, eff)
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$sum) # slack sum
  expect_equal(res$unadj_values, res2$objval) # unadjusted slack sum
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation out
  eff <- Benchmarking::dea(f41_x, f41_y, RTS = 'crs', ORIENTATION = 'out')
  res <- compute_slack(f41_x, f41_y, eff$objval, type = 'crs', orientation = 'out', digits = NULL)
  res2 <- Benchmarking::slack(f41_x, f41_y, eff)
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$sum) # slack sum
  expect_equal(res$unadj_values, res2$objval) # unadjusted slack sum
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation in
  eff <- Benchmarking::dea(nc_x, nc_y, RTS = 'crs', ORIENTATION = 'in')
  res <- compute_slack(nc_x, nc_y, eff$objval, type = 'crs', orientation = 'in', digits = NULL)
  res2 <- Benchmarking::slack(nc_x, nc_y, eff)
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$sum) # slack sum
  expect_equal(res$unadj_values, res2$objval) # unadjusted slack sum
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation out
  eff <- Benchmarking::dea(nc_x, nc_y, RTS = 'crs', ORIENTATION = 'out')
  res <- compute_slack(nc_x, nc_y, eff$objval, type = 'crs', orientation = 'out', digits = NULL)
  res2 <- Benchmarking::slack(nc_x, nc_y, eff)
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$sum) # slack sum
  expect_equal(res$unadj_values, res2$objval) # unadjusted slack sum
  expect_equal(res$lambda, res2$lambda) # lambda

})

test_that('compute_slack() works for VRS', {

  # orientation in
  eff <- Benchmarking::dea(f41_x, f41_y, RTS = 'vrs', ORIENTATION = 'in')
  res <- compute_slack(f41_x, f41_y, eff$objval, type = 'vrs', orientation = 'in', digits = NULL)
  res2 <- Benchmarking::slack(f41_x, f41_y, eff)
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$sum) # slack sum
  expect_equal(res$unadj_values, res2$objval) # unadjusted slack sum
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation out
  eff <- Benchmarking::dea(f41_x, f41_y, RTS = 'vrs', ORIENTATION = 'out')
  res <- compute_slack(f41_x, f41_y, eff$objval, type = 'vrs', orientation = 'out', digits = NULL)
  res2 <- Benchmarking::slack(f41_x, f41_y, eff)
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$sum) # slack sum
  expect_equal(res$unadj_values, res2$objval) # unadjusted slack sum
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation in
  eff <- Benchmarking::dea(nc_x, nc_y, RTS = 'vrs', ORIENTATION = 'in')
  res <- compute_slack(nc_x, nc_y, eff$objval, type = 'vrs', orientation = 'in', digits = NULL)
  res2 <- Benchmarking::slack(nc_x, nc_y, eff)
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$sum) # slack sum
  expect_equal(res$unadj_values, res2$objval) # unadjusted slack sum
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation out
  eff <- Benchmarking::dea(nc_x, nc_y, RTS = 'vrs', ORIENTATION = 'out')
  res <- compute_slack(nc_x, nc_y, eff$objval, type = 'vrs', orientation = 'out', digits = NULL)
  res2 <- Benchmarking::slack(nc_x, nc_y, eff)
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$sum) # slack sum
  expect_equal(res$unadj_values, res2$objval) # unadjusted slack sum
  expect_equal(res$lambda, res2$lambda) # lambda

})

test_that('compute_slack() works for IRS', {

  # orientation in
  eff <- Benchmarking::dea(f41_x, f41_y, RTS = 'irs', ORIENTATION = 'in')
  res <- compute_slack(f41_x, f41_y, eff$objval, type = 'irs', orientation = 'in', digits = NULL)
  res2 <- Benchmarking::slack(f41_x, f41_y, eff)
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$sum) # slack sum
  expect_equal(res$unadj_values, res2$objval) # unadjusted slack sum
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation out
  eff <- Benchmarking::dea(f41_x, f41_y, RTS = 'irs', ORIENTATION = 'out')
  res <- compute_slack(f41_x, f41_y, eff$objval, type = 'irs', orientation = 'out', digits = NULL)
  res2 <- Benchmarking::slack(f41_x, f41_y, eff)
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$sum) # slack sum
  expect_equal(res$unadj_values, res2$objval) # unadjusted slack sum
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation in
  eff <- Benchmarking::dea(nc_x, nc_y, RTS = 'irs', ORIENTATION = 'in')
  res <- compute_slack(nc_x, nc_y, eff$objval, type = 'irs', orientation = 'in', digits = NULL)
  res2 <- Benchmarking::slack(nc_x, nc_y, eff)
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$sum) # slack sum
  expect_equal(res$unadj_values, res2$objval) # unadjusted slack sum
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation out
  eff <- Benchmarking::dea(nc_x, nc_y, RTS = 'irs', ORIENTATION = 'out')
  res <- compute_slack(nc_x, nc_y, eff$objval, type = 'irs', orientation = 'out', digits = NULL)
  res2 <- Benchmarking::slack(nc_x, nc_y, eff)
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$sum) # slack sum
  expect_equal(res$unadj_values, res2$objval) # unadjusted slack sum
  expect_equal(res$lambda, res2$lambda) # lambda

})

test_that('compute_slack() works for DRS', {

  # orientation in
  eff <- Benchmarking::dea(f41_x, f41_y, RTS = 'drs', ORIENTATION = 'in')
  res <- compute_slack(f41_x, f41_y, eff$objval, type = 'drs', orientation = 'in', digits = NULL)
  res2 <- Benchmarking::slack(f41_x, f41_y, eff)
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$sum) # slack sum
  expect_equal(res$unadj_values, res2$objval) # unadjusted slack sum
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation out
  eff <- Benchmarking::dea(f41_x, f41_y, RTS = 'drs', ORIENTATION = 'out')
  res <- compute_slack(f41_x, f41_y, eff$objval, type = 'drs', orientation = 'out', digits = NULL)
  res2 <- Benchmarking::slack(f41_x, f41_y, eff)
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$sum) # slack sum
  expect_equal(res$unadj_values, res2$objval) # unadjusted slack sum
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation in
  eff <- Benchmarking::dea(nc_x, nc_y, RTS = 'drs', ORIENTATION = 'in')
  res <- compute_slack(nc_x, nc_y, eff$objval, type = 'drs', orientation = 'in', digits = NULL)
  res2 <- Benchmarking::slack(nc_x, nc_y, eff)
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$sum) # slack sum
  expect_equal(res$unadj_values, res2$objval) # unadjusted slack sum
  expect_equal(res$lambda, res2$lambda) # lambda

  # orientation out
  eff <- Benchmarking::dea(nc_x, nc_y, RTS = 'drs', ORIENTATION = 'out')
  res <- compute_slack(nc_x, nc_y, eff$objval, type = 'drs', orientation = 'out', digits = NULL)
  res2 <- Benchmarking::slack(nc_x, nc_y, eff)
  dimnames(res2$lambda) <- NULL
  expect_equal(res$values, res2$sum) # slack sum
  expect_equal(res$unadj_values, res2$objval) # unadjusted slack sum
  expect_equal(res$lambda, res2$lambda) # lambda

})
