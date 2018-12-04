context("plotCytoComp")

## --------------------------------------------------------------------------------------
# plotCytoComp flowFrame Method -

test_that("plotCytoComp flowFrame method", {
  
  p <- function() plotCytoComp(getData(gs4, "Single Cells")[[1]], channel = "7-AAD-A", compensate = TRUE, transList = trans2, spfile = "Spillover Matrix.csv")
  expect_doppelganger("plotCytoComp-fr1", p)
  
})

## --------------------------------------------------------------------------------------
# plotCytoComp flowSet Method -

test_that("plotCytoComp flowSet method", {
  
  p <- function() plotCytoComp(getData(gs4, "Single Cells")[c(4,5)], cmfile = "Compensation Channels.csv", compensate = TRUE, transList = trans2, spfile = "Spillover Matrix.csv")
  expect_doppelganger("plotCytoComp-fs1", p)
  
})

## --------------------------------------------------------------------------------------
# plotCytoComp GatingSet Method -

test_that("plotCytoComp GatingSet method", {
  
  p <- function() plotCytoComp(gs4[c(4,5)], parent = "Single Cells", cmfile = "Compensation Channels.csv", compensate = TRUE)
  expect_doppelganger("plotCytoComp-gs1", p)
  
})