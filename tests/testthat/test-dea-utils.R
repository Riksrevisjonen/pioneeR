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
