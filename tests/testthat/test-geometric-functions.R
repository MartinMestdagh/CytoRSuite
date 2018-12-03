context("Geometric Functions")

## --------------------------------------------------------------------------------
# linesIntercept -

test_that("linesIntercept", {
  
  expect_equal(linesIntercept(c(0, 0), c(0, 1), c(1, 0), c(1, 1)), c(Inf,Inf))
  expect_equal(linesIntercept(c(0, 0), c(1, 1), c(0, 1), c(1, 0), interior.only = TRUE), c(0.5,0.5))
  
})