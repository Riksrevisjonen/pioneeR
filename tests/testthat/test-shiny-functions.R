Sys.unsetenv("PIONEER_DATA")

test_that("unsetting environment variables works", {
  Sys.setenv(PIONEER_DATA = "hello world")
  unset_env_vars()
  expect_false(nzchar(Sys.getenv("PIONEER_DATA")))
})

test_that("check_balance() works as expected", {
  # balanced
  df <- data.frame(id = rep(LETTERS[1:6], 2), time = c(rep.int(1984, 6), rep.int(2001, 6)))
  expect_false(check_balance(df, "id", "time")$listwise)

  # unbalanced
  df <- data.frame(id = c(LETTERS[1:6], LETTERS[1:5]), time = c(rep.int(1984, 6), rep.int(2001, 5)))
  expect_true(check_balance(df, "id", "time")$listwise)
})

test_that("set_message() returns a div", {
  div <- set_message("warning", "A warning")
  expect_equal(class(div), "shiny.tag")
  expect_equal(div$attribs$class, "alert alert-warning")
  div <- set_message("info", "An ordinary message")
  expect_equal(class(div), "shiny.tag")
  expect_equal(div$attribs$class, "alert alert-info")
})
