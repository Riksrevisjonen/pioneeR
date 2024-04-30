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

test_that('valid ports are set as expected', {
  expect_equal(check_for_unsafe_port(7700), 7700L)
  expect_equal(check_for_unsafe_port('7700'), 7700L)
})

test_that('unsafe ports issue a warning and returns NULL', {
  expect_warning(check_for_unsafe_port(4045))
  expect_warning(check_for_unsafe_port(6566))
  expect_warning(check_for_unsafe_port(6666))
  expect_warning(check_for_unsafe_port(6697))
  expect_null(suppressWarnings(check_for_unsafe_port(4045)))
  expect_null(suppressWarnings(check_for_unsafe_port(6566)))
  expect_null(suppressWarnings(check_for_unsafe_port(6666)))
  expect_null(suppressWarnings(check_for_unsafe_port(6697)))
  expect_null(suppressWarnings(check_for_unsafe_port('abc')))
  expect_equal(suppressWarnings(check_for_unsafe_port(4743)), 4743)
})

test_that('ports outside valid range issue a warning and returns NULL', {
  expect_warning(check_for_unsafe_port(22))
  expect_warning(check_for_unsafe_port(99999))
  expect_null(suppressWarnings(check_for_unsafe_port(22)))
  expect_null(suppressWarnings(check_for_unsafe_port(99999)))
})
