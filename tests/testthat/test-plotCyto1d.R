context("plotCyto1d")

library(vdiffr)
library(grDevices)

gt <- system.file("extdata", "Example-gatingTemplate.csv", package = "cytoRSuite")
gt <- gatingTemplate(gt)
gating(gt,gs)

Va2 <- getData(gs, "T Cells")

intGate <- rectangleGate("PE-A"=c(2.6,4.2))

# Remeber to run manage_cases in console to verify and generate reference svg files #

## --------------------------------------------------------------------------------
# flowFrame method -

test_that("plotCyto1d flowFrame method return appropriate plots",{
  
  plotCyto1d(Va2[[1]], channel = "FSC-A", main = "Control")
  p1 <- recordPlot()
  expect_doppelganger("plotCyto1d fr", p1)
  
  plotCyto1d(Va2[[1]], channel = "PE-A")
  p2 <- recordPlot()
  expect_doppelganger("plotCyto1d fluor", p2)
  
  plotCyto1d(Va2[[1]], channel = "PE-A", transList = trans, xlab = "Va2", ylab = "Density")
  p3 <- recordPlot()
  expect_doppelganger("plotCyto1d trans", p3)
  
  plotCyto1d(Va2[[1]], channel = "PE-A", overlay = Va2[[2]], transList = trans, offset = 0.6)
  p4 <- recordPlot()
  expect_doppelganger("plotCyto1d fr overlay fr", p4)
  
  plotCyto1d(Va2[[1]], channel = "PE-A", overlay = Va2, transList = trans, fill = c("green","red","blue"), gates = intGate, col.gate = "black")
  p5 <- recordPlot()
  expect_doppelganger("plotCyto1d fr overlay fs", p5)
  
  plotCyto1d(Va2[[1]], channel = "PE-A", overlay = Va2, transList = trans, fill = c("green","red","blue"), gates = intGate, col.gate = "black", text.labels = "T Cells")
  p6 <- recordPlot()
  expect_doppelganger("plotCyto1d labels", p6)
  
  })

## --------------------------------------------------------------------------------
# flowSet method -

test_that("",{
  
  plotCyto1d(Va2, channel = "FSC-A", main = c("Control","Activated"), fill = "red", ylab = "Density")
  p7 <- recordPlot()
  expect_doppelganger("plotCyto1d fs", p7)
  
  plotCyto1d(Va2, channel = "FSC-A", main = "Control", merge = TRUE)
  p8 <- recordPlot()
  expect_doppelganger("plotCyto1d fs merge", p8)
  
  plotCyto1d(Va2, channel = "FSC-A", main = "Control", overlay = Va2[[1]])
  p9 <- recordPlot()
  expect_doppelganger("plotCyto1d fs overlay fr", p9)
  
  plotCyto1d(Va2, channel = "CD4", main = "Control", overlay = Va2, transList = trans)
  p10 <- recordPlot()
  expect_doppelganger("plotCyto1d fs overlay fs", p10)
  
  plotCyto1d(Va2, channel = "FSC-A", main = "Control", overlay = Va2[[1]], merge = TRUE, col = c("blue","red"))
  p11 <- recordPlot()
  expect_doppelganger("plotCyto1d merge overlay fr", p11)
  
  plotCyto1d(Va2, channel = "FSC-A", main = "Control", overlay = Va2, merge = TRUE, fill = c("blue","purple"))
  p12 <- recordPlot()
  expect_doppelganger("plotCyto1d merge overlay fs", p12)
  
  plotCyto1d(Va2, channel = "PE-A", main = "Control", stack = 0.5, transList = trans)
  p13 <- recordPlot()
  expect_doppelganger("plotCyto1d fs stack", p13)
  
  })
