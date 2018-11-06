context("ComputeStats")

fs <- Activation
pData(fs)$Treatment <- c("Control","Activated")

chnls <- c("Alexa Fluor 405-A","Alexa Fluor 430-A","APC-Cy7-A", "PE-A", "Alexa Fluor 488-A", "Alexa Fluor 700-A", "Alexa Fluor 647-A", "7-AAD-A") 
markers <- c("Hoechst-405", "Hoechst-430", "CD11c", "Va2", "CD8", "CD4", "CD44", "CD69")
names(markers) <- chnls
markernames(fs) <- markers

gs <- GatingSet(fs)
gs <- compensate(gs, fs[[1]]@description$SPILL)

trans <- estimateLogicle(gs[[2]], getChannels(gs))
gs <- transform(gs, trans)

gt <- gatingTemplate("Example-gatingTemplate.csv")
gating(gt, gs)

medFI <- computeStats(gs, alias = c("Live Cells", "T Cells", "CD8 T Cells"), stat = "median", save = FALSE)
gMFI <- computeStats(gs, alias = c("Live Cells", "T Cells", "CD8 T Cells"), stat = "geo mean", save = FALSE)
ModFI <- computeStats(gs, alias = c("Live Cells", "T Cells", "CD8 T Cells"), stat = "mode", save = FALSE)
Freq <- computeStats(gs, alias = c("T Cells", "CD8 T Cells", "CD4 T Cells"), parent = c("Live Cells", "T Cells"), stat = "freq", save = FALSE)
Cnt <- computeStats(gs, alias = c("T Cells", "CD8 T Cells", "CD4 T Cells"), parent = c("Live Cells", "T Cells"), stat = "count", save = FALSE)

## -------------------------------------------------------------------
# computeStats flowFrame method -
Va2 <- getData(gs, "T Cells")

test_that("computeStats flowFrame method returns the same statistics as GatingSet method",{
  
  expect_equal(computeStats(Va2[[1]], stat = "median", transList = trans), as.matrix(medFI[[2]][1,-1]))
  expect_equal(computeStats(Va2[[1]], stat = "geo mean", transList = trans), as.matrix(gMFI[[2]][1,-1]))
  expect_equal(computeStats(Va2[[1]], stat = "mode", transList = trans), as.matrix(ModFI[[2]][1,-1]))
  expect_equal(computeStats(Va2[[1]], stat = "count")[,1], Cnt[[1]][1,"count"])
  
  })

## -------------------------------------------------------------------
# computeStats flowSet method -

test_that("computeStats flowSet method returns the same statistics as GatingSet method",{
  
  expect_equal(computeStats(Va2, stat = "median", transList = trans)[[1]], medFI[[2]])
  expect_equal(computeStats(Va2, stat = "geo mean", transList = trans)[[1]], gMFI[[2]])
  expect_equal(computeStats(Va2, stat = "mode", transList = trans)[[1]], ModFI[[2]])
  expect_equal(computeStats(Va2, stat = "count")[[1]], Cnt[[1]])

  })

## -------------------------------------------------------------------
# computeStats GatingSet method -

test_that("computeStats work swith marker names",{
  
  expect_equal(computeStats(gs, channels = c("CD4","CD8"), alias = c("T Cells","CD8 T Cells"), stat = "median", save = FALSE),
               computeStats(gs, channels = c("Alexa Fluor 700-A","Alexa Fluor 488-A"), alias = c("T Cells","CD8 T Cells"), stat = "median", save = FALSE))
  
})