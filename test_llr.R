context("Check local linear regression function")
source("llr_functions.R")
library(testthat)

n = 15
## a very simple regression model
x = rnorm(n)
y = rnorm(x + rnorm(n))
z = seq(-1, 1, length.out = 100)
test_that("llr output has correct length", {
  expect_equal(length(llr(x, y, z, omega = 1)), length(z))
})


test_that("make_weight_matrix works on simple cases", {
  ## check that the output is a diagonal matrix, that all the elements are positive, that the weights are correct in simple cases where you know what the output shuold be
  z=0 
  omega=1
  Wz = make_weight_matrix(z, x, omega)
  expect_equal(dim(Wz), c(n, n))
  expect_true(all(diag(Wz) >= 0))
  expect_equal((Wz[upper.tri(Wz)]), rep(0,n*(n-1)/2))
  expect_equal((Wz[lower.tri(Wz)]), rep(0,n*(n-1)/2))
})

test_that("make_predictor_matrix works on simple cases", {
  ## write tests to check that the dimensions are correct, the first column is all 1's, etc.
  X= make_predictor_matrix(x)
  expect_equal(dim(X), c(n, 2))
  expect_equal(X[, 1], rep(1, n))
  expect_equal(X[, 2], x)
})