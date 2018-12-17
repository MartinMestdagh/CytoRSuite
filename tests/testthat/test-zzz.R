context("zzz")

# zzz ---------------------------------------------------------------

test_that("CytoRSuite loading", {
  
  CytoRSuite:::.onLoad()
  expect_true(getOption("CytoRSuite_interact") == interactive())
  expect_true(openCyto:::.isRegistered("ppmanualGate"))
  expect_true(openCyto:::.isRegistered("manualGate"))
  expect_true(openCyto:::.isRegistered("drawGate"))
  
})