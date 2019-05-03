library(testthat)

#Load source functions
source("/Users/yusheng/Desktop/Stat133/Workout_03/binomial/R")

context("Test Binomial functions")

test_that("bin_choose returns accurate value", {
  expect_length(bin_choose(5, 1), 1)
  expect_error(bin_choose(5, 6))
  expect_equal(bin_choose(3, 2), 3)
})

test_that("bin_probability returns accurate value", {
  expect_length(bin_probability(1:3, 3, .5), 3)
  expect_error(bin_probability(3, 1, .4))
  expect_error(bin_probability(1, 3, 2))
  expect_equal(bin_probability(1, 1, .5), .5)
})

test_that("bin_distribution returns accurate values", {
  expect_s3_class(bin_distribution(4, .5), "bindis")
  expect_length(bin_distribution(2, .5), 2)
  expect_equal(sum(bin_distribution(2, .5)$probability), 1)
})

test_that("bin_cumulative returns accurate values", {
  expect_s3_class(bin_cumulative(4, .5), "bincum")
  expect_length(bin_cumulative(4, .5), 3)
  expect_equal(sum(bin_cumulative(4, .5)$probability), 1)
})