context("Validation Functions")

## ----------------------------------------------------------------------
# checkChannels -

test_that("checkChannels flowFrame method returns appropriate channels",{
  
  expect_equal(checkChannels(fs[[1]],"CD4", plot = TRUE), "Alexa Fluor 700-A")
  expect_equal(checkChannels(fs[[1]],c("CD4","Alexa Fluor 488-A"), plot = TRUE), c("Alexa Fluor 700-A","Alexa Fluor 488-A"))
  expect_equal(checkChannels(fs[[1]],c("CD4","CD8"), plot = TRUE), c("Alexa Fluor 700-A","Alexa Fluor 488-A"))
  expect_error(checkChannels(fs[[1]],c("CD4","CD8","CD69"), plot = TRUE), "Invalid number of supplied channels.", fixed = TRUE)
  expect_equal(checkChannels(fs[[1]],c("CD4","CD8","CD69"), plot = FALSE), c("Alexa Fluor 700-A","Alexa Fluor 488-A","7-AAD-A"))
  expect_equal(checkChannels(fs[[1]],c("CD4","Alexa Fluor 488-A","CD69"), plot = FALSE), c("Alexa Fluor 700-A","Alexa Fluor 488-A","7-AAD-A"))
  expect_error(checkChannels(fs[[1]],c("Alexa Fluor 48-A","CD69"), plot = TRUE), "Alexa Fluor 48-A is not a valid channel for this flowFrame.")
  expect_equal(checkChannels(fs[[1]], channels = c(1,2), plot = TRUE), colnames(fs[[1]])[1:2])
  
})

test_that("checkChannels flowSet method returns appropriate channels",{
  
  expect_equal(checkChannels(fs,"CD4", plot = TRUE), "Alexa Fluor 700-A")
  expect_equal(checkChannels(fs,c("CD4","Alexa Fluor 488-A"), plot = TRUE), c("Alexa Fluor 700-A","Alexa Fluor 488-A"))
  expect_equal(checkChannels(fs,c("CD4","CD8"), plot = TRUE), c("Alexa Fluor 700-A","Alexa Fluor 488-A"))
  expect_error(checkChannels(fs,c("CD4","CD8","CD69"), plot = TRUE), "Invalid number of supplied channels.", fixed = TRUE)
  expect_equal(checkChannels(fs,c("CD4","CD8","CD69"), plot = FALSE), c("Alexa Fluor 700-A","Alexa Fluor 488-A","7-AAD-A"))
  expect_equal(checkChannels(fs,c("CD4","Alexa Fluor 488-A","CD69"), plot = FALSE), c("Alexa Fluor 700-A","Alexa Fluor 488-A","7-AAD-A"))
  expect_error(checkChannels(fs,c("Alexa Fluor 48-A","CD69"), plot = TRUE), "Alexa Fluor 48-A is not a valid channel for this flowFrame.")
  expect_equal(checkChannels(fs, channels = c(1,2), plot = TRUE), colnames(fs[[1]])[1:2])
  
})

test_that("checkChannels GatingSet method returns appropriate channels",{
  
  expect_equal(checkChannels(gs,"CD4", plot = TRUE), "Alexa Fluor 700-A")
  expect_equal(checkChannels(gs,c("CD4","Alexa Fluor 488-A"), plot = TRUE), c("Alexa Fluor 700-A","Alexa Fluor 488-A"))
  expect_equal(checkChannels(gs,c("CD4","CD8"), plot = TRUE), c("Alexa Fluor 700-A","Alexa Fluor 488-A"))
  expect_error(checkChannels(gs,c("CD4","CD8","CD69"), plot = TRUE), "Invalid number of supplied channels.", fixed = TRUE)
  expect_equal(checkChannels(gs,c("CD4","CD8","CD69"), plot = FALSE), c("Alexa Fluor 700-A","Alexa Fluor 488-A","7-AAD-A"))
  expect_equal(checkChannels(gs,c("CD4","Alexa Fluor 488-A","CD69"), plot = FALSE), c("Alexa Fluor 700-A","Alexa Fluor 488-A","7-AAD-A"))
  expect_error(checkChannels(gs,c("Alexa Fluor 48-A","CD69"), plot = TRUE), "Alexa Fluor 48-A is not a valid channel for this flowFrame.")
  expect_equal(checkChannels(gs, channels = c(1,2), plot = TRUE), colnames(fs[[1]])[1:2])
  
})

## ------------------------------------------------------------------
# checkGateType -

test_that("checkGateType returns a vector of gate types of expected length",{
  
  expect_equal(checkGateType(type = "r", alias = c("A","B","C")), c("rectangle","rectangle","rectangle"))
  expect_equal(checkGateType(type = "p", alias = c("A","B","C")), c("polygon","polygon","polygon"))
  expect_equal(checkGateType(type = "q", alias = c("A","B","C","D")), "quadrant")
  expect_error(checkGateType(type = "q", alias = c("A","B","C")), "Supply the names of 4 poulations to alias for quadrant gates.", fixed = TRUE)
  expect_error(checkGateType(type = c("v","j"), alias = c("A","B","C")), "v & j are not valid gate types for drawGate!", fixed = TRUE)
  expect_error(checkGateType(type = "z", alias = c("A","B","C")), "z is not a valid gate type for drawGate!", fixed = TRUE)
  
  })

## ------------------------------------------------------------------
# checkAlias -

test_that("checkAlias stops gating process if alias is incorrect", {
  
  expect_error(checkAlias(type = "web"), "The name(s) of the population(s) to be gated must be supplied as the alias argument.", fixed = TRUE)
  expect_error(checkAlias(type = "quadrant", alias = "Cells"), "Supply 4 population names to alias argument to construct quadrant gates.", fixed = TRUE)
  expect_error(checkAlias(type = c("rectangle","polygon"), alias = "Cells"), "Length of alias must be the same length as type for multi-gates.", fixed = TRUE)
  
})

## ------------------------------------------------------------------
# checkOSGD -

test_that("checkOSGD opens a windows graphics device", {
  
  checkOSGD()
  expect_equal(names(dev.cur()), "windows")
  dev.off()
  
})

## ------------------------------------------------------------------
# checkTemplate -

test_that("checkTemplate throws an error if population already exists in gatingTemplate", {
  
  expect_error(checkTemplate(parent = "Cells", alias = "Single Cells", gtfile = "Compensation-gatingTemplate.csv"), "Please supply a different gtfile name or edit the existing gate(s) using editGate.", fixed = TRUE)
  
})

## ------------------------------------------------------------------
# checkTransList -

test_that("checkTransList throws an error if the transList is not valid", {
  
  expect_error(checkTransList(transList = "Test"), "Supplied transList should be of class transformList or transformerList.")
  
})

## ------------------------------------------------------------------
# checkOverlay -

test_that("checkOverlay flowFrame method returns list of flowFrames",{
  
  expect_equal(checkOverlay(fs[[1]], overlay = fs[[2]]), list(fs[[2]]))
  expect_equal(checkOverlay(fs[[1]], overlay = list(fs[[2]])), list(fs[[2]]))
  expect_equal(checkOverlay(fs[[1]], overlay = fs), list(fs[[1]],fs[[2]], fs[[3]], fs[[4]]))
  expect_equal(checkOverlay(fs[[1]], overlay = list(fs)), list(fs[[1]],fs[[2]], fs[[3]], fs[[4]]))
  
  expect_error(checkOverlay(fs[[1]], overlay = list(fs,fs)), "Overlay should be either a flowFrame, flowSet, list of flowFrames or a list containing a flowSet.")
  
  expect_equal(nrow(checkOverlay(fs[[1]], overlay = fs[[2]], subSample = 100)[[1]]), 100)
  expect_equal(nrow(checkOverlay(fs[[1]], overlay = list(fs[[2]]), subSample = 100)[[1]]), 100)
  expect_equal(nrow(checkOverlay(fs[[1]], overlay = fs, subSample = 100)[[1]]), 100)
  expect_equal(nrow(checkOverlay(fs[[1]], overlay = list(fs), subSample = 100)[[1]]), 100)
  
})

test_that("checkOverlay flowSet method returns list of flowFrame lists",{
  
  expect_equal(checkOverlay(fs, overlay = fs[[2]]), list(list(fs[[2]]),list(fs[[2]]), list(fs[[2]]), list(fs[[2]])))
  expect_equal(checkOverlay(fs, overlay = list(fs[[2]])), list(list(fs[[2]]),list(fs[[2]]),list(fs[[2]]),list(fs[[2]])))
  expect_equal(checkOverlay(fs, overlay = fs), list(list(fs[[1]]),list(fs[[2]]), list(fs[[3]]), list(fs[[4]])))
  expect_equal(checkOverlay(fs, overlay = list(fs)), list(list(fs[[1]]),list(fs[[2]]), list(fs[[3]]), list(fs[[4]])))
  expect_equal(checkOverlay(fs, overlay = list(fs,fs)), list(list(fs[[1]],fs[[1]]),list(fs[[2]],fs[[2]]), list(fs[[3]],fs[[3]]), list(fs[[4]],fs[[4]])))
  expect_equal(checkOverlay(fs, overlay = list(fs[[1]],fs[[2]],fs[[3]],fs[[4]])), list(list(fs[[1]]),list(fs[[2]]),list(fs[[3]]),list(fs[[4]])))

  expect_error(checkOverlay(fs, overlay = exprs(fs[[1]])), "Overlay should be either a flowFrame, flowSet, list of flowFrames or a list of flowSets.")
  
  expect_equal(nrow(checkOverlay(fs, overlay = fs[[2]], subSample = 100)[[1]][[1]]), 100)
  expect_equal(nrow(checkOverlay(fs, overlay = list(fs[[2]]), subSample = 100)[[1]][[1]]), 100)
  expect_equal(nrow(checkOverlay(fs, overlay = fs, subSample = 100)[[1]][[1]]), 100)
  expect_equal(nrow(checkOverlay(fs, overlay = list(fs), subSample = 100)[[1]][[1]]), 100)
  expect_equal(nrow(checkOverlay(fs, overlay = list(fs,fs), subSample = 100)[[1]][[1]]), 100)
  expect_equal(nrow(checkOverlay(fs, overlay = list(fs[[1]],fs[[2]],fs[[3]],fs[[4]]), subSample = 100)[[1]][[1]]), 100)

  expect_error(checkOverlay(fs, overlay = list(fs[1:3])), "Each flowSet in supplied list should be of the same length as the supplied flowSet.")
  expect_error(checkOverlay(fs, overlay = list(fs[[1]],fs[[2]])), "Supplied list of flowFrames should be of the same length as the flowSet.")
  expect_error(checkOverlay(fs, overlay = list(list(fs[[1]]),list(fs[[2]]))), "Each list of flowFrames should be the same length as the flowSet.")
  
})

## ---------------------------------------------------------------------
# checkStat -

test_that("checkStat stops computeStats if supplied stat is not supported", {
  
  expect_error(checkStat("average"), "Supplied statistic not supported.")
  
})