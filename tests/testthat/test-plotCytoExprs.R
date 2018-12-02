context("plotCytoExprs")

## --------------------------------------------------------------------------
# flowFrame Method -

test_that("plotCytoExprs flowFrame method", {
  
  p <- function() plotCytoExprs(Va2[[4]], channels = c("CD4","CD8","CD44","CD69"), transList = trans)
  expect_doppelganger("plotcytoexprs-fr1", p)
  
  p <- function() plotCytoExprs(Va2[[4]], transList = trans, mfrow = c(6,3))
  expect_doppelganger("plotcytoexprs-fr2", p)
  
})

## --------------------------------------------------------------------------
# flowSet Method -

test_that("plotCytoExprs flowSet method", {
  
  p <- function() plotCytoExprs(Va2, transList = trans, merge = TRUE)
  expect_doppelganger("plotcytoexprs-fs1", p)
  
  p <- function() plotCytoExprs(Va2, transList = trans, merge = FALSE)
  expect_doppelganger("plotcytoexprs-fs2", p)
  
})

## --------------------------------------------------------------------------
# GatingHierarchy Method - 

test_that("plotCytoExprs GatingHierarchy method", {
  
  p <- function() plotCytoExprs(gs[[4]], parent = "T Cells", transList = trans)
  expect_doppelganger("plotcytoexprs-gh1", p)
  
  p <- function() plotCytoExprs(gs[[4]], parent = "T Cells", channels = c("CD4","CD8","CD44","CD69"))
  expect_doppelganger("plotcytoexprs-gh2", p)
  
  expect_error(plotCytoExprs(gs[[4]]), "Please supply the name of the parent population to plot.")
  
})

## --------------------------------------------------------------------------
# GatingSet Method -

test_that("plotCytoExprs GatingSet method", {
  
  p <- function() plotCytoExprs(gs, parent = "T Cells", transList = trans)
  expect_doppelganger("plotcytoexprs-gs1", p)
  
  p <- function() plotCytoExprs(gs, parent = "T Cells", channels = c("CD4","CD8","CD44","CD69"))
  expect_doppelganger("plotcytoexprs-gs2", p)
  
  expect_error(plotCytoExprs(gs), "Please supply the name of the parent population to plot.")
  
  
})