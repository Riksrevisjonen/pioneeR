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


test_that('compute_slack() returns the correct structure', {
  res <- compute_efficiency(f41_x, f41_y, type = 'vrs', orientation = 'out')
  res <- compute_slack(f41_x, f41_y, model = res)
  # class
  expect_identical(class(res), 'list')
  expect_identical(class(res$data), 'data.frame')
  # object names
  expect_identical(names(res), c('values', 'unadj_values', 'lambda', 'data', 'info'))
  expect_identical(names(res$data), c('sum', 'is_slack', 'sx1', 'sx2', 'sy1'))
  expect_identical(names(res$info), c('type', 'orientation', 'dims'))
  expect_identical(names(res$info$dims), c('n_inputs', 'n_outputs', 'n_units', 'n_constraints', 'n_vars', 'n_lambda'))
  expect_identical(res$info$type, 'vrs')
  expect_identical(res$info$orientation, 'out')
  # dimensions (dim object)
  expect_equal(res$info$dims$n_units, nrow(f41_x))
  expect_equal(res$info$dims$n_inputs, ncol(f41_x))
  expect_equal(res$info$dims$n_outputs, ncol(f41_y))
  # dimensions (values)
  expect_equal(length(res$values), nrow(f41_x))
  expect_equal(length(res$unadj_values), nrow(f41_x))
  expect_equal(nrow(res$lambda), nrow(f41_x))
  expect_equal(ncol(res$lambda), nrow(f41_x))

})

test_that('compute_slack() works for CRS', {

  # --- Frontier 4.1 data --- #

  # orientation in
  bench_res <- benchmarking_results$frontier41$in_crs$slack
  res <- compute_efficiency(f41_x, f41_y, type = 'crs', orientation = 'in')
  res <- compute_slack(f41_x, f41_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # orientation out
  bench_res <- benchmarking_results$frontier41$out_crs$slack
  res <- compute_efficiency(f41_x, f41_y, type = 'crs', orientation = 'out')
  res <- compute_slack(f41_x, f41_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # --- Norwegian District Courts --- #

  # orientation in
  bench_res <- benchmarking_results$norCourts2018$in_crs$slack
  res <- compute_efficiency(nc_x, nc_y, type = 'crs', orientation = 'in')
  res <- compute_slack(nc_x, nc_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # orientation out
  bench_res <- benchmarking_results$norCourts2018$out_crs$slack
  res <- compute_efficiency(nc_x, nc_y, type = 'crs', orientation = 'out')
  res <- compute_slack(nc_x, nc_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # --- Hospitals --- #

  # orientation in
  bench_res <- benchmarking_results$hospitals$in_crs$slack
  res <- compute_efficiency(hp_x, hp_y, type = 'crs', orientation = 'in')
  res <- compute_slack(hp_x, hp_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # orientation out
  bench_res <- benchmarking_results$hospitals$out_crs$slack
  res <- compute_efficiency(hp_x, hp_y, type = 'crs', orientation = 'out')
  res <- compute_slack(hp_x, hp_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # --- Electric Plants --- #

  # orientation in
  bench_res <- benchmarking_results$electricPlants$in_crs$slack
  res <- compute_efficiency(ecp_x, ecp_y, type = 'crs', orientation = 'in')
  res <- compute_slack(ecp_x, ecp_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # orientation out
  bench_res <- benchmarking_results$electricPlants$out_crs$slack
  res <- compute_efficiency(ecp_x, ecp_y, type = 'crs', orientation = 'out')
  res <- compute_slack(ecp_x, ecp_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

})

test_that('compute_slack() works for VRS', {

  # --- Frontier 4.1 data --- #

  # orientation in
  bench_res <- benchmarking_results$frontier41$in_vrs$slack
  res <- compute_efficiency(f41_x, f41_y, type = 'vrs', orientation = 'in')
  res <- compute_slack(f41_x, f41_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # orientation out
  bench_res <- benchmarking_results$frontier41$out_vrs$slack
  res <- compute_efficiency(f41_x, f41_y, type = 'vrs', orientation = 'out')
  res <- compute_slack(f41_x, f41_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # --- Norwegian District Courts --- #

  # orientation in
  bench_res <- benchmarking_results$norCourts2018$in_vrs$slack
  res <- compute_efficiency(nc_x, nc_y, type = 'vrs', orientation = 'in')
  res <- compute_slack(nc_x, nc_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # orientation out
  bench_res <- benchmarking_results$norCourts2018$out_vrs$slack
  res <- compute_efficiency(nc_x, nc_y, type = 'vrs', orientation = 'out')
  res <- compute_slack(nc_x, nc_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # --- Hospitals --- #

  # orientation in
  bench_res <- benchmarking_results$hospitals$in_vrs$slack
  res <- compute_efficiency(hp_x, hp_y, type = 'vrs', orientation = 'in')
  res <- compute_slack(hp_x, hp_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # orientation out
  bench_res <- benchmarking_results$hospitals$out_vrs$slack
  res <- compute_efficiency(hp_x, hp_y, type = 'vrs', orientation = 'out')
  res <- compute_slack(hp_x, hp_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # --- Electric Plants --- #

  # orientation in
  bench_res <- benchmarking_results$electricPlants$in_vrs$slack
  res <- compute_efficiency(ecp_x, ecp_y, type = 'vrs', orientation = 'in')
  res <- compute_slack(ecp_x, ecp_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # orientation out
  bench_res <- benchmarking_results$electricPlants$out_vrs$slack
  res <- compute_efficiency(ecp_x, ecp_y, type = 'vrs', orientation = 'out')
  res <- compute_slack(ecp_x, ecp_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

})

test_that('compute_slack() works for IRS', {

  # --- Frontier 4.1 data --- #

  # orientation in
  bench_res <- benchmarking_results$frontier41$in_irs$slack
  res <- compute_efficiency(f41_x, f41_y, type = 'irs', orientation = 'in')
  res <- compute_slack(f41_x, f41_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # orientation out
  bench_res <- benchmarking_results$frontier41$out_irs$slack
  res <- compute_efficiency(f41_x, f41_y, type = 'irs', orientation = 'out')
  res <- compute_slack(f41_x, f41_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # --- Norwegian District Courts --- #

  # orientation in
  bench_res <- benchmarking_results$norCourts2018$in_irs$slack
  res <- compute_efficiency(nc_x, nc_y, type = 'irs', orientation = 'in')
  res <- compute_slack(nc_x, nc_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # orientation out
  bench_res <- benchmarking_results$norCourts2018$out_irs$slack
  res <- compute_efficiency(nc_x, nc_y, type = 'irs', orientation = 'out')
  res <- compute_slack(nc_x, nc_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # --- Hospitals --- #

  # orientation in
  bench_res <- benchmarking_results$hospitals$in_irs$slack
  res <- compute_efficiency(hp_x, hp_y, type = 'irs', orientation = 'in')
  res <- compute_slack(hp_x, hp_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # orientation out
  bench_res <- benchmarking_results$hospitals$out_irs$slack
  res <- compute_efficiency(hp_x, hp_y, type = 'irs', orientation = 'out')
  res <- compute_slack(hp_x, hp_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # --- Electric Plants --- #

  # orientation in
  bench_res <- benchmarking_results$electricPlants$in_irs$slack
  res <- compute_efficiency(ecp_x, ecp_y, type = 'irs', orientation = 'in')
  res <- compute_slack(ecp_x, ecp_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # orientation out
  bench_res <- benchmarking_results$electricPlants$out_irs$slack
  res <- compute_efficiency(ecp_x, ecp_y, type = 'irs', orientation = 'out')
  res <- compute_slack(ecp_x, ecp_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda


})

test_that('compute_slack() works for DRS', {

  # --- Frontier 4.1 data --- #

  # orientation in
  bench_res <- benchmarking_results$frontier41$in_drs$slack
  res <- compute_efficiency(f41_x, f41_y, type = 'drs', orientation = 'in')
  res <- compute_slack(f41_x, f41_y, model = res )
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # orientation out
  bench_res <- benchmarking_results$frontier41$out_drs$slack
  res <- compute_efficiency(f41_x, f41_y, type = 'drs', orientation = 'out')
  res <- compute_slack(f41_x, f41_y, model = res )
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # --- Norwegian District Courts --- #

  # orientation in
  bench_res <- benchmarking_results$norCourts2018$in_drs$slack
  res <- compute_efficiency(nc_x, nc_y, type = 'drs', orientation = 'in')
  res <- compute_slack(nc_x, nc_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # orientation out
  bench_res <- benchmarking_results$norCourts2018$out_drs$slack
  res <- compute_efficiency(nc_x, nc_y, type = 'drs', orientation = 'out')
  res <- compute_slack(nc_x, nc_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # --- Hospitals --- #

  # orientation in
  bench_res <- benchmarking_results$hospitals$in_drs$slack
  res <- compute_efficiency(hp_x, hp_y, type = 'drs', orientation = 'in')
  res <- compute_slack(hp_x, hp_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # orientation out
  bench_res <- benchmarking_results$hospitals$out_drs$slack
  res <- compute_efficiency(hp_x, hp_y, type = 'drs', orientation = 'out')
  res <- compute_slack(hp_x, hp_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # --- Electric Plants --- #

  # orientation in
  bench_res <- benchmarking_results$electricPlants$in_drs$slack
  res <- compute_efficiency(ecp_x, ecp_y, type = 'drs', orientation = 'in')
  res <- compute_slack(ecp_x, ecp_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

  # orientation out
  bench_res <- benchmarking_results$electricPlants$out_drs$slack
  res <- compute_efficiency(ecp_x, ecp_y, type = 'drs', orientation = 'out')
  res <- compute_slack(ecp_x, ecp_y, model = res)
  expect_equal(res$values, bench_res$sum) # slack sum
  expect_equal(res$unadj_values, bench_res$objval) # unadjusted slack sum
  expect_equal(res$lambda, bench_res$lambda) # lambda

})

