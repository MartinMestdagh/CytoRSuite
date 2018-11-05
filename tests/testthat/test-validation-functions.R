context("Validation Functions")

## ----------------------------------------------------------------------
# checkChannels -

fs <- Activation

chnls <- c("PE-A", "Alexa Fluor 488-A", "Alexa Fluor 700-A", "Alexa Fluor 647-A", "7-AAD-A") 
markers <- c("Va2", "CD8", "CD4", "CD44", "CD69")
names(markers) <- chnls
markernames(fs) <- markers

gs <- GatingSet(fs)

test_that("checkChannels flowFrame method returns appropriate channels",{
  
  expect_equal(checkChannels(fs[[1]],"CD4", plot = TRUE), "Alexa Fluor 700-A")
  expect_equal(checkChannels(fs[[1]],c("CD4","Alexa Fluor 488-A"), plot = TRUE), c("Alexa Fluor 700-A","Alexa Fluor 488-A"))
  expect_equal(checkChannels(fs[[1]],c("CD4","CD8"), plot = TRUE), c("Alexa Fluor 700-A","Alexa Fluor 488-A"))
  expect_error(checkChannels(fs[[1]],c("CD4","CD8","CD69"), plot = TRUE), "Invalid number of supplied channels.", fixed = TRUE)
  expect_equal(checkChannels(fs[[1]],c("CD4","CD8","CD69"), plot = FALSE), c("Alexa Fluor 700-A","Alexa Fluor 488-A","7-AAD-A"))
  expect_equal(checkChannels(fs[[1]],c("CD4","Alexa Fluor 488-A","CD69"), plot = FALSE), c("Alexa Fluor 700-A","Alexa Fluor 488-A","7-AAD-A"))
  
  })

test_that("checkChannels flowSet method returns appropriate channels",{
  
  expect_equal(checkChannels(fs,"CD4", plot = TRUE), "Alexa Fluor 700-A")
  expect_equal(checkChannels(fs,c("CD4","Alexa Fluor 488-A"), plot = TRUE), c("Alexa Fluor 700-A","Alexa Fluor 488-A"))
  expect_equal(checkChannels(fs,c("CD4","CD8"), plot = TRUE), c("Alexa Fluor 700-A","Alexa Fluor 488-A"))
  expect_error(checkChannels(fs,c("CD4","CD8","CD69"), plot = TRUE), "Invalid number of supplied channels.", fixed = TRUE)
  expect_equal(checkChannels(fs,c("CD4","CD8","CD69"), plot = FALSE), c("Alexa Fluor 700-A","Alexa Fluor 488-A","7-AAD-A"))
  expect_equal(checkChannels(fs,c("CD4","Alexa Fluor 488-A","CD69"), plot = FALSE), c("Alexa Fluor 700-A","Alexa Fluor 488-A","7-AAD-A"))
  
  })

test_that("checkChannels GatingSet method returns appropriate channels",{
  
  expect_equal(checkChannels(gs,"CD4", plot = TRUE), "Alexa Fluor 700-A")
  expect_equal(checkChannels(gs,c("CD4","Alexa Fluor 488-A"), plot = TRUE), c("Alexa Fluor 700-A","Alexa Fluor 488-A"))
  expect_equal(checkChannels(gs,c("CD4","CD8"), plot = TRUE), c("Alexa Fluor 700-A","Alexa Fluor 488-A"))
  expect_error(checkChannels(gs,c("CD4","CD8","CD69"), plot = TRUE), "Invalid number of supplied channels.", fixed = TRUE)
  expect_equal(checkChannels(gs,c("CD4","CD8","CD69"), plot = FALSE), c("Alexa Fluor 700-A","Alexa Fluor 488-A","7-AAD-A"))
  expect_equal(checkChannels(gs,c("CD4","Alexa Fluor 488-A","CD69"), plot = FALSE), c("Alexa Fluor 700-A","Alexa Fluor 488-A","7-AAD-A"))
  
  })

## ------------------------------------------------------------------
# checkGateType -

test_that("checkGateType returns a vector of gate types of expected length",{
  
  expect_equal(checkGateType(type = "r", alias = c("A","B","C")), c("rectangle","rectangle","rectangle"))
  expect_equal(checkGateType(type = "p", alias = c("A","B","C")), c("polygon","polygon","polygon"))
  expect_equal(checkGateType(type = "q", alias = c("A","B","C","D")), "quadrant")
  expect_error(checkGateType(type = "q", alias = c("A","B","C")), "Supply the names of 4 poulations to alias for quadrant gates.", fixed = TRUE)
  
  })

## ------------------------------------------------------------------
# checkOverlay -

test_that("checkOverlay flowFrame method returns list of flowFrames",{
  
  expect_equal(checkOverlay(fs[[1]], overlay = fs[[2]]), list(fs[[2]]))
  expect_equal(checkOverlay(fs[[1]], overlay = list(fs[[2]])), list(fs[[2]]))
  expect_equal(checkOverlay(fs[[1]], overlay = fs), list(fs[[1]],fs[[2]]))
  expect_equal(checkOverlay(fs[[1]], overlay = list(fs)), list(fs[[1]],fs[[2]]))
  
  })

test_that("checkOverlay flowFrame method returns list of flowFrames",{
  
  expect_equal(checkOverlay(fs, overlay = fs[[2]]), list(list(fs[[2]]),list(fs[[2]])))
  expect_equal(checkOverlay(fs, overlay = list(fs[[2]])), list(list(fs[[2]]),list(fs[[2]])))
  expect_equal(checkOverlay(fs, overlay = fs), list(list(fs[[1]]),list(fs[[2]])))
  expect_equal(checkOverlay(fs, overlay = list(fs)), list(list(fs[[1]]),list(fs[[2]])))
  expect_equal(checkOverlay(fs, overlay = list(fs,fs)), list(list(fs[[1]],fs[[1]]),list(fs[[2]],fs[[2]])))
  
  })