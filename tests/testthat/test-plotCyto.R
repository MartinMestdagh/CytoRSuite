context("plotCyto")

## ------------------------------------------------------------------------------------------------
# plotCyto Gatinghierarchy Method - 

test_that("plotCyto gatingHierarchy method", {
  
  expect_error(plotCyto(gs[[1]], alias = "Cells", channels = c("FSC-A","SSC-A")), "Please supply the name of the parent population to plot.")
  expect_error(plotCyto(gs[[1]], parent = "Cells", alias = "Test", channels = c("FSC-A","SSC-A")), "Supplied alias does not exist in the GatingSet.")
  
  p <- function() plotCyto(gs[[1]], parent = "Live Cells", alias = "CD4 T Cells", channels = c("CD4","CD8"), overlay = "CD8 T Cells")
  expect_doppelganger("plotCyto-gh1", p)
  
})

## ------------------------------------------------------------------------------------------------
# plotCyto GatingSet Method -

test_that("plotCyto GatingSet method", {
  
  expect_error(plotCyto(gs, alias = "Cells", channels = c("FSC-A","SSC-A")), "Please supply the name of the parent population to plot.")
  expect_error(plotCyto(gs, parent = "Cells", alias = "Test", channels = c("FSC-A","SSC-A")), "Supplied alias does not exist in the GatingSet.")
  
  p <- function() plotCyto(gs, parent = "Live Cells", alias = "CD4 T Cells", channels = c("CD4","CD8"), overlay = "CD8 T Cells")
  expect_doppelganger("plotCyto-gs1", p)
  
  p <- function() plotCyto(gs, parent = "root", alias = "Cells", channels = c("FSC-A","SSC-A"), overlay = "CD8 T Cells")
  expect_doppelganger("plotCyto-gs2", p)
  
})