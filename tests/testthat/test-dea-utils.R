df <- data.frame(
  id = letters[1:20],
  a = 1:20,
  b = 21:40,
  c = 41:60,
  d = 61:80
)

test_that('create_matrix() returns expected results', {
  withr::local_options(lifecycle_verbosity = 'quiet')
  x <- create_matrix(df, id = 'id', columns = c('a', 'b', 'c'))
  expect_true(is.matrix(x))
  expect_equal(c('a', 'b', 'c'), colnames(x))
  expect_equal(letters[1:20], rownames(x))
})

test_that('normalization works in create_matrix()', {
  withr::local_options(lifecycle_verbosity = 'quiet')
  x <- create_matrix(df, id = 'id', columns = c('a', 'b', 'c'), normalize = TRUE)
  expect_equal(mean(x), 1)
})

test_that('create_matrix() returns errors as expected', {
  withr::local_options(lifecycle_verbosity = 'quiet')
  expect_error(create_matrix(1:10, 1, 1))
  expect_error(create_matrix(df, id = 'id', columns = 'x'))
  expect_error(create_matrix(df, id = 'x', columns = 'd'))
})


test_that('check_numeric() works correctly', {
  # numeric data
  numeric_x <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
  numeric_y <- data.frame(c = c(7, 8, 9), d = c(10, 11, 12))
  expect_silent(check_numeric(numeric_x, numeric_y))

  # non-numeric data
  non_numeric_x <- data.frame(a = c(1, 2, 3), b = c('a', 'b', 'c'))
  numeric_y <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
  expect_error(check_numeric(non_numeric_x, numeric_y), 'All variables must be numeric.')
  expect_error(check_numeric(numeric_y, non_numeric_x), 'All variables must be numeric.')
})

test_that('check_nunits() works correctly', {
  # correct
  x <- data.frame(a = 1:12, b = 13:24)
  y <- data.frame(c = 25:36, d = 37:48)
  expect_silent(check_nunits(x, y))

  # incorrect
  x <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
  y <- data.frame(c = c(7, 8, 9, 10), d = c(11, 12, 13, 14))
  expect_error(check_nunits(x, y))

  # to few DMUs
  x <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
  y <- data.frame(c = c(7, 8, 9), d = c(10, 11, 12))
  expect_warning(check_nunits(x, y))
})

