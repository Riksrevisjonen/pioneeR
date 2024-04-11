df <- data.frame(
  id = letters[1:20],
  a = 1:20,
  b = 21:40,
  c = 41:60,
  d = 61:80
)

test_that('compute_scale_efficiency returns expected results', {
  withr::local_options(lifecycle_verbosity = 'quiet')
  x <- create_matrix(df, id = 'id', columns = c('a', 'b', 'c'))
  y <- create_matrix(df, id = 'id', columns = 'd')
  res <- compute_scale_efficiency(x, y, orientation = 'in')
  expect_true(is.data.frame(res))
  res <- compute_scale_efficiency(x, y, orientation = 'in', digits = 4L)
  expect_true(is.data.frame(res))
})

test_that('compute_scale_efficiency returns errors as expected', {
  withr::local_options(lifecycle_verbosity = 'quiet')
  x <- create_matrix(df, id = 'id', columns = c('a', 'b', 'c'))
  y <- create_matrix(df, id = 'id', columns = 'd')
  expect_error(compute_scale_efficiency(x, 1:3))
  expect_error(compute_scale_efficiency(1:3, x))
  expect_error(compute_scale_efficiency(matrix(1:4, ncol = 2), matrix(1:6, ncol = 2)))
  expect_warning(compute_scale_efficiency(x, y, digits = 'a'))
})
