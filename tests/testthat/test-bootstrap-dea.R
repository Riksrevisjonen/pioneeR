# Frontier 4.1
frontier41 <- readRDS('../testdata/frontier41.RDS')

mod_vrs_in <- compute_dea(frontier41, id = 'firm', input = c('labour', 'capital'), output = 'output', rts = 'vrs', orientation = 'in')
mod_vrs_out <- compute_dea(frontier41, id = 'firm',input = c('labour', 'capital'), output = 'output', rts = 'vrs', orientation = 'out')
mod_crs_in <- compute_dea(frontier41, id = 'firm', input = c('labour', 'capital'), output ='output', rts = 'crs', orientation = 'in')
mod_crs_out <- compute_dea(frontier41, id = 'firm', input = c('labour', 'capital'), output = 'output', rts = 'crs', orientation = 'out')
mod_irs_in <- compute_dea(frontier41, id = 'firm', input = c('labour', 'capital'), output = 'output', rts = 'irs', orientation = 'in')

test_that('bootstrap_dea() returns error as expected', {
  expect_error(bootstrap_dea(data.frame(x = 1:10)))
  expect_error(bootstrap_dea(mod_irs))
  expect_error(bootstrap_dea(mod_vrs, bw_rule = 'abc'))
})

test_that('bootstrap_dea() will perform a bootstrap for all supported technologies', {
  boot_vrs_in <- bootstrap_dea(mod_vrs_in, iterations = 2)
  boot_vrs_out <- bootstrap_dea(mod_vrs_out, iterations = 2)
  boot_crs_in <- bootstrap_dea(mod_crs_in, iterations = 2)
  boot_crs_out <- bootstrap_dea(mod_crs_out, iterations = 2)
  expect_equal(class(boot_vrs_in), 'pioneer_bootstrap')
  expect_equal(attr(boot_vrs_in$bootstrap, 'rts'), 'vrs')
  expect_equal(attr(boot_vrs_in$bootstrap, 'orientation'), 'in')
  expect_equal(class(boot_vrs_out), 'pioneer_bootstrap')
  expect_equal(attr(boot_vrs_out$bootstrap, 'rts'), 'vrs')
  expect_equal(attr(boot_vrs_out$bootstrap, 'orientation'), 'out')
  expect_equal(class(boot_crs_in), 'pioneer_bootstrap')
  expect_equal(attr(boot_crs_in$bootstrap, 'rts'), 'crs')
  expect_equal(attr(boot_crs_in$bootstrap, 'orientation'), 'in')
  expect_equal(class(boot_crs_out), 'pioneer_bootstrap')
  expect_equal(attr(boot_crs_out$bootstrap, 'rts'), 'crs')
  expect_equal(attr(boot_crs_out$bootstrap, 'orientation'), 'out')
})

test_that('bootstrap_sample() returns a vector in the interval [0, 1]', {
  # We create a vector of random floats in the interval [0, 1]
  # We do not set a seed, as we do not want (fully) deterministic values
  rand_eff <- runif(20)
  h <- bw_rule(rand_eff)
  b_sample <- bootstrap_sample(rand_eff, h)
  expect_equal(class(b_sample), 'numeric')
  expect_equal(typeof(b_sample), 'double')
  expect_true(max(b_sample) <= 1)
  expect_true(min(b_sample) >= 0)
  # Output oriented scores should still produce sample values [0, 1]
  b_sample <- bootstrap_sample(1/rand_eff, h)
  expect_true(max(b_sample) <= 1)
  expect_true(min(b_sample) >= 0)
})

test_that('bw_rule can be set as expected', {
  boot_ucv <- bootstrap_dea(mod_vrs_in, bw_rule = 'ucv', iterations = 2)
  expect_equal(attr(boot_ucv$bootstrap, 'bandwidth')$bw_rule, 'ucv')
  boot_silverman <- bootstrap_dea(mod_vrs_in, bw_rule = 'silverman', iterations = 2)
  expect_equal(attr(boot_silverman$bootstrap, 'bandwidth')$bw_rule, 'silverman')
  boot_scott <- bootstrap_dea(mod_vrs_in, bw_rule = 'scott', iterations = 2)
  expect_equal(attr(boot_scott$bootstrap, 'bandwidth')$bw_rule, 'scott')
  boot_sw <- bootstrap_dea(mod_vrs_in, bw_rule = 0.014, iterations = 2)
  expect_equal(attr(boot_sw$bootstrap, 'bandwidth')$h, 0.014)
  boot_null <- bootstrap_dea(mod_vrs_in, bw_rule = NULL, iterations = 2)
  expect_true(attr(boot_sw$bootstrap, 'bandwidth')$h > 0)
})
