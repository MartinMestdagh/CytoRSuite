context("plotCyto1d")

# vdiffr::manage_cases() to validate and generate reference images

Va2 <- getData(gs, "T Cells")

intGate <- rectangleGate("PE-A"=c(2.6,4.2))

# Remeber to run manage_cases in console to verify and generate reference svg files #

## --------------------------------------------------------------------------------
# flowFrame method -

test_that("plotCyto1d flowFrame method return appropriate plots",{
  
  p1 <- function() plotCyto1d(Va2[[1]], channel = "FSC-A", main = "Control")
  expect_doppelganger("plotCyto1d fr", p1)
  
  p2 <- function() plotCyto1d(Va2[[1]], channel = "PE-A")
  expect_doppelganger("plotCyto1d fluor", p2)
  
  p3 <- function() plotCyto1d(Va2[[1]], channel = "PE-A", transList = trans, xlab = "Va2", ylab = "Density")
  expect_doppelganger("plotCyto1d trans", p3)
  
  p4 <- function() plotCyto1d(Va2[[1]], channel = "PE-A", overlay = Va2[[2]], transList = trans, offset = 0.6)
  expect_doppelganger("plotCyto1d fr overlay fr", p4)
  
  p5 <- function() plotCyto1d(Va2[[1]], channel = "PE-A", overlay = Va2, transList = trans, fill = c("green","red","blue"), gates = intGate, col.gate = "black")
  expect_doppelganger("plotCyto1d fr overlay fs", p5)
  
  p6 <- function() plotCyto1d(Va2[[1]], channel = "PE-A", overlay = Va2, transList = trans, fill = c("green","red","blue"), gates = intGate, col.gate = "black", text.labels = "T Cells")
  expect_doppelganger("plotCyto1d labels", p6)
  
  })

## --------------------------------------------------------------------------------
# flowSet method -

test_that("",{
  
  p7 <- function() plotCyto1d(Va2, channel = "FSC-A", main = c("Control","Activated"), fill = "red", ylab = "Density")
  expect_doppelganger("plotCyto1d fs", p7)
  
  p8 <- function() plotCyto1d(Va2, channel = "FSC-A", main = "Control", merge = TRUE)
  expect_doppelganger("plotCyto1d fs merge", p8)
  
  p9 <- function() plotCyto1d(Va2, channel = "FSC-A", main = "Control", overlay = Va2[[1]])
  expect_doppelganger("plotCyto1d fs overlay fr", p9)
  
  p10 <- function() plotCyto1d(Va2, channel = "CD4", main = "Control", overlay = Va2, transList = trans)
  expect_doppelganger("plotCyto1d fs overlay fs", p10)
  
  p11 <- function() plotCyto1d(Va2, channel = "FSC-A", main = "Control", overlay = Va2[[1]], merge = TRUE, col = c("blue","red"))
  expect_doppelganger("plotCyto1d merge overlay fr", p11)
  
  p12 <- function() plotCyto1d(Va2, channel = "FSC-A", main = "Control", overlay = Va2, merge = TRUE, fill = c("blue","purple"))
  expect_doppelganger("plotCyto1d merge overlay fs", p12)
  
  p13 <- function() plotCyto1d(Va2, channel = "PE-A", main = "Control", stack = 0.5, transList = trans)
  expect_doppelganger("plotCyto1d fs stack", p13)
  
  })
