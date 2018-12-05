context("computeSpillover")

## ---------------------------------------------------------------------------------------
# computeSpillover -

test_that("computeSpillover GatingSet method", {
  
  computeSpillover(gs4, parent = "Single Cells", spfile = "Test Spillover Matrix.csv")
  
  expect_true(checkFile("Test Spillover Matrix.csv"))
  
  sp <- read.csv("Test Spillover Matrix.csv", header = TRUE, row.names = 1)
  colnames(sp) <- rownames(sp)
  expect_equal(sp , spill)
  
})

unlink("Test Spillover Matrix.csv")