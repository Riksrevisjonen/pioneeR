Sys.unsetenv('PIONEER_DATA')

test_that('set_local_data() sets environment variable', {
  x <- data.frame(a = 1:10)
  set_local_data(x)
  expect_true(nzchar(Sys.getenv('PIONEER_DATA')))
  Sys.unsetenv('PIONEER_DATA')
})

test_that('set_local_data() writes a temp file', {
  x <- data.frame(a = 1:10)
  set_local_data(x)
  expect_true(file.exists(Sys.getenv('PIONEER_DATA')))
  Sys.unsetenv('PIONEER_DATA')
})

test_that('set_lcoal_data() fails if PIONEER_DATA is already set', {
  Sys.setenv(PIONEER_DATA = 'hello world')
  x <- data.frame(a = 1:10)
  expect_error(set_local_data(x))
  Sys.unsetenv('PIONEER_DATA')
})
