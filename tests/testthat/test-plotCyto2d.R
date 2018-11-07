context("plotCyto2d")

gt <- system.file("extdata", "Example-gatingTemplate.csv", package = "cytoRSuite")
gt <- gatingTemplate(gt)
gating(gt,gs)

Va2 <- getData(gs, "T Cells")

intGate <- rectangleGate("PE-A"=c(2.6,4.2))

## ----------------------------------------------------------------------------------
# flowFrame -

test_that("",{
  
  p1 <- function() plotCyto1d(Va2[[1]], channel = "FSC-A", main = "Control")
  vdiffr::expect_doppelganger("plotCyto2d fr", p1)
  
  })
