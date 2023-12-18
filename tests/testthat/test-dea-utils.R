df <- data.frame(
  id = letters[1:20],
  a = 1:20,
  b = 21:40,
  c = 41:60,
  d = 61:80
)

test_that('create_matrix returns expected results', {
  x <- create_matrix(df, id = 'id', columns = c('a', 'b', 'c'))
  expect_true(is.matrix(x))
  expect_equal(c('a', 'b', 'c'), colnames(x))
  expect_equal(letters[1:20], rownames(x))
})

test_that('normalization works in create_matrix', {
  x <- create_matrix(df, id = 'id', columns = c('a', 'b', 'c'), normalize = TRUE)
  expect_equal(mean(x), 1)
})

test_that('create_matrix returns errors as expected', {
  expect_error(create_matrix(1:10, 1, 1))
  expect_error(create_matrix(df, id = 'id', columns = 'x'))
  expect_error(create_matrix(df, id = 'x', columns = 'd'))
})

test_that('compute_scale_efficiency returns expected results', {
  x <- create_matrix(df, id = 'id', columns = c('a', 'b', 'c'))
  y <- create_matrix(df, id = 'id', columns = 'd')
  res <- compute_scale_efficiency(x, y, orientation = 'in')
  expect_true(is.data.frame(res))
  res <- compute_scale_efficiency(x, y, orientation = 'in', digits = 4L)
  expect_true(is.data.frame(res))
})

test_that('compute_scale_efficiency returns errors as expected', {
  x <- create_matrix(df, id = 'id', columns = c('a', 'b', 'c'))
  y <- create_matrix(df, id = 'id', columns = 'd')
  expect_error(compute_scale_efficiency(x, 1:3))
  expect_error(compute_scale_efficiency(1:3, x))
  expect_error(compute_scale_efficiency(matrix(1:4, ncol = 2), matrix(1:6, ncol = 2)))
  expect_warning(compute_scale_efficiency(x, y, digits = 'a'))
})
