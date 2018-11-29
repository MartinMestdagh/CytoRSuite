context("plotCyto2d")

# vdiffr::manage_cases() to validate and generate reference images

mat <- matrix(c(2,3.5,-0.5,2.5), ncol=2, dimnames=list(c("min", "max"),
                                                         c("Alexa Fluor 700-A", "Alexa Fluor 488-A")))
rectGate <- rectangleGate(.gate=mat)

sqrcut <- matrix(c(-0.5, -0.3, 0.2, 1.2, 1.5, 1.7, 4.3, 3.5, 2.5, 2.5, 3.5, 4.3),ncol=2,nrow=6)
colnames(sqrcut) <- c("Alexa Fluor 700-A","Alexa Fluor 488-A")
polyGate <- polygonGate(.gate = sqrcut)

## ----------------------------------------------------------------------------------
# flowFrame -

test_that("plotCyto2d flowFrame method returns appropriate plots", {
  
  p1 <- function() plotCyto2d(Va2[[1]], channels = c("FSC-A","SSC-A"), main = "Control")
  vdiffr::expect_doppelganger("plotCyto2d fr", p1)
  
  p2 <- function() plotCyto2d(Va2[[1]], channels = c("CD4","CD8"), xlab = "CD4", ylab = "CD8")
  expect_doppelganger("plotCyto2d fr markers", p2)
  
  p3 <- function() plotCyto2d(Va2[[1]], channels = c("CD4","CD8"), transList = trans, contours = 15)
  expect_doppelganger("plotCyto2d fr trans", p3)
  
  p4 <- function() plotCyto2d(Va2[[2]], channels = c("CD4","CD8"), transList = trans, overlay = Va2[[1]], col = c("black","magenta"))
  expect_doppelganger("plotCyto2d fr overlay fr", p4)
  
  p5 <- function() plotCyto2d(Va2[[2]], channels = c("CD4","CD8"), transList = trans, overlay = Va2, col = c("black","magenta", "blue"))
  expect_doppelganger("plotCyto2d fr overlay fs", p5)
  
  p6 <- function() plotCyto2d(Va2[[1]], channels = c("Alexa Fluor 700-A","Alexa Fluor 488-A"), gates = polyGate)
  expect_doppelganger("plotCyto2d fr gate", p6)
  
  p7 <- function() plotCyto2d(Va2[[1]], channels = c("Alexa Fluor 700-A","Alexa Fluor 488-A"), gates = filters(list(rectGate,polyGate)), transList = trans)
  expect_doppelganger("plotCyto2d fr gates", p7)
  
  })

## ----------------------------------------------------------------------------------
# flowSet -

test_that("plotCyto2d flowSet method returns appropriate plots", {
  
  p8 <- function() plotCyto2d(Va2, channels = c("FSC-A","SSC-A"), main = c("Control","Activated"))
  vdiffr::expect_doppelganger("plotCyto2d fs", p8)
  
  p9 <- function() plotCyto2d(Va2, channels = c("CD4","CD8"), xlab = "CD4", ylab = "CD8")
  expect_doppelganger("plotCyto2d fs markers", p9)
  
  p10 <- function() plotCyto2d(Va2, channels = c("CD4","CD8"), transList = trans, contours = 15)
  expect_doppelganger("plotCyto2d fs trans", p10)
  
  p11 <- function() plotCyto2d(Va2, channels = c("CD4","CD8"), transList = trans, overlay = Va2[[1]], col = c("black","magenta"))
  expect_doppelganger("plotCyto2d fs overlay fr", p11)
  
  p12 <- function() plotCyto2d(Va2, channels = c("CD4","CD8"), transList = trans, overlay = Va2, col = c("black","magenta", "blue"))
  expect_doppelganger("plotCyto2d fs overlay fs", p12)
  
  p13 <- function() plotCyto2d(Va2, channels = c("Alexa Fluor 700-A","Alexa Fluor 488-A"), gates = polyGate)
  expect_doppelganger("plotCyto2d fs gate", p13)
  
  p14 <- function() plotCyto2d(Va2, channels = c("Alexa Fluor 700-A","Alexa Fluor 488-A"), gates = filters(list(rectGate,polyGate)), transList = trans, col.gate = c("purple","green4"), text.labels = c("CD4+","CD8+"))
  expect_doppelganger("plotCyto2d fs gates", p14)
  
})