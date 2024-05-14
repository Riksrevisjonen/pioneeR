test_that('input oriented models are summarised correctly', {
  x <- runif(100)
  s <- summary_tbl_dea(x)
  expect_equal(class(s), 'data.frame')
  expect_true(all(grepl('E', s$Range)))
  expect_equal(length(x), sum(s$Frequency))
})

test_that('output oriented models are summarised correctly', {
  x <- runif(100, 1, 10)
  s <- summary_tbl_dea(x)
  expect_equal(class(s), 'data.frame')
  expect_true(all(grepl('F', s$Range)))
  expect_equal(length(x), sum(s$Frequency))
})
