x <- c(1, 2, 3, 4, 5)
y <- c(1, 2, 3, 4, NA)
z <- c(TRUE, FALSE, TRUE)
w <- letters[1:5]

source("functions.R")



test_that("range works as expected", {
  x <- c(1,2,3,4,5)
  expect_equal(stat_range(x), 4)
  expect_length(stat_range(x))
  expect_type(stat_range(x), 'double')
})


test_that("range works as expected with NA", {
  y <- c(1,2,3,4,NA)
  expect_equal(stat_range(y), NA_real_)
  expect_length(stat_range(y), 1)
})

test_that("range works as expected with booleans", {
  z <- c(TRUE,FALSE, TRUE)
  expect_equal(stat_range(z), 1L)
  expect_length(stat_range(z), 1)
  expect_type(stat_range(z), 'integer')
})

test_that("does not work with characters",{
  w <- letters[1:5]
  expect_error()
})

test_file("tests.R")
