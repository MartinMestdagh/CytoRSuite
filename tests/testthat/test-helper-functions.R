context("Helper Functions")

## ------------------------------------------------------------------
# getChannels -
test_that("getChannels returns the correct channels for flowFrames, flowSets and GatingSets", {
  
  expect_equal(getChannels(fs[[1]]), c("Alexa Fluor 488-A", "PE-A", "PE-Texas Red-A", "7-AAD-A", "PE-Cy7-A", "Alexa Fluor 405-A", "Alexa Fluor 430-A", "Qdot 605-A", "Alexa Fluor 647-A", "Alexa Fluor 700-A", "APC-Cy7-A"))
  expect_equal(getChannels(fs), c("Alexa Fluor 488-A", "PE-A", "PE-Texas Red-A", "7-AAD-A", "PE-Cy7-A", "Alexa Fluor 405-A", "Alexa Fluor 430-A", "Qdot 605-A", "Alexa Fluor 647-A", "Alexa Fluor 700-A", "APC-Cy7-A"))
  expect_equal(getChannels(gs), c("Alexa Fluor 488-A", "PE-A", "PE-Texas Red-A", "7-AAD-A", "PE-Cy7-A", "Alexa Fluor 405-A", "Alexa Fluor 430-A", "Qdot 605-A", "Alexa Fluor 647-A", "Alexa Fluor 700-A", "APC-Cy7-A"))

})

## ------------------------------------------------------------------
# sampleFrame -
test_that("sampleFrame returns subsetted flowFrame", {
  
  expect_equal(nrow(exprs(sampleFrame(fs[[1]],25000))), 25000)
  
})