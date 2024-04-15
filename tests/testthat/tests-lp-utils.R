test_that('adjust_efficiency() works correctly', {
  eps <- 0.02
  x <- c(0.99, 1.01, 0.5)
  expected <- c(1, 1, 0.5)
  result <- adjust_efficiency(x, eps)
  expect_equal(result, expected)

  eps <- 1e-06
  x <- c(0.9999999, 1.000001, 0.5)
  expected <- c(1, 1, 0.5)
  result <- adjust_efficiency(x, eps)
  expect_equal(result, expected)
})

test_that('adjust_lambda() works correctly', {
  eps <- 0.02
  x <- c(0.99, 1.01, 0.5, 0.009)
  expected <- c(1, 1, 0.5, 0)
  result <- adjust_lambda(x, eps)
  expect_equal(result, expected)

  eps <- 1e-06
  x <- c(0.9999999, 1.000001, 0.5, 0.0000009)
  expected <- c(1, 1, 0.5, 0)
  result <- adjust_lambda(x, eps)
  expect_equal(result, expected)
})

test_that('get_dims() works correctly', {
  x <- matrix(1:6, nrow=3, ncol=2)
  y <- matrix(7:9, nrow=3, ncol=1)
  xref <- matrix(1:4, nrow=2, ncol=2)
  yref <- matrix(5:6, nrow=2, ncol=1)

  # crs
  result <- get_dims(x, y, rts = 'crs')
  expect_equal(result$n_inputs, 2L)
  expect_equal(result$n_outputs, 1L)
  expect_equal(result$n_units, 3L)
  expect_equal(result$n_constraints, 3L)
  expect_equal(result$n_lambda, 0L)
  expect_equal(result$n_vars, 4L)

  # irs/drs
  result <- get_dims(x, y, rts = 'irs')
  expect_equal(result$n_inputs, 2L)
  expect_equal(result$n_outputs, 1L)
  expect_equal(result$n_units, 3L)
  expect_equal(result$n_constraints, 4L)
  expect_equal(result$n_lambda, 1L)
  expect_equal(result$n_vars, 4L)

  # vrs
  result <- get_dims(x, y, rts = 'vrs')
  expect_equal(result$n_inputs, 2L)
  expect_equal(result$n_outputs, 1L)
  expect_equal(result$n_units, 3L)
  expect_equal(result$n_constraints, 5L)
  expect_equal(result$n_lambda, 2L)
  expect_equal(result$n_vars, 4L)

  # xref/yref (only two units here)
  result <- get_dims(x, y, xref, yref, rts = 'crs')
  expect_equal(result$n_units, 2L)
  expect_equal(result$n_vars, 3L)

  # slack (LP matrix is transposed in this calculation, hence n_vars = n_inputs + n_outputs + n_units)
  result <- get_dims(x, y, rts = 'crs', slack = TRUE)
  expect_equal(result$n_vars, 6L)

})

test_that('get_invalid_objective() works correctly', {
  expect_equal(get_invalid_objective(0, 'in'), NULL)
  expect_equal(get_invalid_objective(2, 'in'), Inf)
  expect_equal(get_invalid_objective(3, 'in'), Inf)
  expect_equal(get_invalid_objective(2, 'out'), -Inf)
  expect_equal(get_invalid_objective(3, 'out'), -Inf)
  expect_equal(get_invalid_objective(1, 'in'), NA_real_)
  expect_equal(get_invalid_objective(1, 'out'), NA_real_)
})
