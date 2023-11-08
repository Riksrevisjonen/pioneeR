Sys.unsetenv('PIONEER_DATA')

test_that('unsetting environment variables works', {
  Sys.setenv(PIONEER_DATA = 'hello world')
  unset_env_vars()
  expect_false(nzchar(Sys.getenv('PIONEER_DATA')))
})
