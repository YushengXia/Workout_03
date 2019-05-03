library(testthat)

#Load source functions
source("/Users/yusheng/Desktop/Stat133/Workout_03/binomial/R")

context("Test summary functions")

test_that("aux_mean returns accurate value", {
  expect_length(aux_mean(4, .5), 1)
  expect_error(aux_mean(3, "bird"))
  expect_equal(aux_mean(2, .5), 1)
})

test_that("aux_variance returns accurate value", {
  expect_length(aux_variance(2, .5), 1)
  expect_error(aux_variance(2, "bird"))
  expect_equal(aux_variance(4, .5), 1)
})

test_that("aux_mode returns accurate value", {
  expect_length(aux_mode(2, .5), 1)
  expect_error(aux_mode(2, "bird"))
  expect_equal(aux_mode(2, .5), 1)
})

test_that("aux_skewness returns accurate value", {
  expect_length(aux_skewness(2, .5), 1)
  expect_error(aux_skewness(2, "bird"))
  expect_equal(aux_skewness(2, .4), (1 - 2*.4)/(2 * .4 * (1-.4))^(1/2))
})

test_that("aux_kurtosis returns accurate value", {
  expect_length(aux_kurtosis(2, .5), 1)
  expect_error(aux_kurtosis(2, "bird"))
  expect_equal(aux_kurtosis(2, .5), (1 - 6 * .5 * (1 - .5))/(2 * .5 * (1-.5)))
})