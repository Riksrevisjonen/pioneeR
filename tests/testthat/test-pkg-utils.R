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

test_that('unsafe ports issue a warning and returns NULL', {
  expect_warning(check_for_unsafe_port(4045))
  expect_warning(check_for_unsafe_port(6566))
  expect_warning(check_for_unsafe_port(6666))
  expect_warning(check_for_unsafe_port(6697))
  expect_true(suppressWarnings(check_for_unsafe_port(4045)) != 4045)
  expect_true(suppressWarnings(check_for_unsafe_port(6566)) != 6566)
  expect_true(suppressWarnings(check_for_unsafe_port(6666)) != 6666)
  expect_true(suppressWarnings(check_for_unsafe_port(6697)) != 6697)
  expect_equal(suppressWarnings(check_for_unsafe_port(4743)), 4743)
})

test_that('ports outside valid range issue a warning and returns a valid port', {
  # Our safe port function does not use port numbers above 9999
  port_range <- 3000L:9999L
  safe_ports <- port_range[!port_range %in% unsafe_ports()]
  expect_warning(check_for_unsafe_port(22))
  expect_warning(check_for_unsafe_port(99999))
  expect_true(suppressWarnings(check_for_unsafe_port(22)) %in% safe_ports)
  expect_true(suppressWarnings(check_for_unsafe_port(99999)) %in% safe_ports)
})
