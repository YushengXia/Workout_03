library(testthat)

#Load source functions
source("/Users/yusheng/Desktop/Stat133/Workout_03/binomial/R")

context("Test checker functions")

test_that("check_prob returns accurate values", {
  expect_true(check_prob(0.5))
  expect_error(check_prob(1.5))
  expect_error(check_prob("one"))
  expect_error(check_prob(-1))
})

test_that("check_trials returns accurate values", {
  expect_true(check_trials(4))
  expect_error(check_trials(4.4))
  expect_error(check_trials("one"))
  expect_error(check_trials(-5))
})

test_that("check_success returns accurate values", {
  expect_true(check_success(4, 5))
  expect_error(check_success(5, 4))
  expect_error(check_success("one", "two"))
  expect_true(check_success(5, 5))
  expect_error(check_success(3.3, 3.4))
})


