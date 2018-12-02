context("plotCytoGates")

smp <- flowSet(lapply(1:4, function(x){
  
  Activation[[x]][1:1000,]
  
}))

gs2 <- GatingSet(smp)
gs2 <- compensate(gs2, smp[[1]]@description$SPILL)
gs2 <- transform(gs2, trans)

gating(gt, gs2)

## --------------------------------------------------------------------------
# GatingHierarchy Method -

test_that("plotCytoGates GatingHierachy method", {
  
  p1 <- function() plotCytoGates(gs2[[4]], overlay = c("CD4 T Cells", "CD8 T Cells"), col = c("black","red","green"))
  expect_doppelganger("plotcytogates-gh1", p1)
  
})