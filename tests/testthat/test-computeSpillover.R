context("computeSpillover")

## .getCompleteTransList ----------------------------------------------------------

test_that(".getCompleteTransList", {
  
  # transList of incorrect class
  expect_error(.getCompleteTransList(fs[[4]], transList = "Test"), "Supplied transList should be of class transformList or transformerList.")
  
  # flowFrame/flowSet transformed but no transList
  expect_error(.getCompleteTransList(getData(gs, "T Cells")[[1]]), "Looks like the data is already transformed. Please supply the transList used.")
  expect_error(.getCompleteTransList(getData(gs, "T Cells")), "Looks like the data is already transformed. Please supply the transList used.")
  
  fst <- transform(fs, checkTransList(trans, inverse = FALSE))
  gst <- GatingSet(fst)
  
  expect_error(.getCompleteTransList(gst), "Looks like the data is already transformed. No transformations found in GatingSet.")
  
  # NULL transList -----------------------------------------------------------------
  
  # flowFrame no transformations - no transList
  tr <- .getCompleteTransList(fs[[4]])
  
  expect_s4_class(tr, "transformList")
  expect_equal(names(tr@transforms), getChannels(fs[[4]]))

  # flowSet no transformations -  no transList
  tr <- .getCompleteTransList(fs)
  
  expect_s4_class(tr, "transformList")
  expect_equal(names(tr@transforms), getChannels(fs[[4]]))
  
  # GatingSet no transformations
  gst <- GatingSet(fs)
  tr <- .getCompleteTransList(gst)
  
  expect_is(tr, "transformerList")
  expect_setequal(names(tr), getChannels(gst))
  
  # GatingSet transformed
  tr <- .getCompleteTransList(gs)
  
  expect_is(tr, "transformerList")
  expect_setequal(names(tr), getChannels(gs))
  
  # GatingSet with some transformations
  trns <- estimateLogicle(gst[[4]], c("PE-A","Alexa Fluor 700-A"))
  gst <- transform(gst, trns)
  
  tr <- .getCompleteTransList(gst)
  
  expect_is(tr, "transformerList")
  expect_setequal(names(tr), getChannels(gst))
  
  # Complete transList ------------------------------------------------------------
  
  # flowFrame complete transList
  tr <- .getCompleteTransList(fs[[4]], trans)
  
  expect_s4_class(tr, "transformList")
  expect_equal(names(tr@transforms), getChannels(fs[[4]]))
  
  # flowSet complete transList
  tr <- .getCompleteTransList(fs[[4]], trans)
  
  expect_s4_class(tr, "transformList")
  expect_equal(names(tr@transforms), getChannels(fs[[4]]))
  
  # GatingSet complete transList
  gst <- GatingSet(fs)
  tr <- .getCompleteTransList(gst, trans)
  
  expect_is(tr, "transformerList")
  expect_setequal(names(tr), getChannels(gst))
  
  # GatingSet complete transformList
  tr <- .getCompleteTransList(gst, checkTransList(trns, inverse = FALSE))
  
  expect_is(tr, "transformerList")
  expect_setequal(names(tr), getChannels(gst))
  
  # Transformed GatingSet - complete transList
  tr <- .getCompleteTransList(gs, trans)
  
  expect_is(tr, "transformerList")
  expect_setequal(names(tr), getChannels(gs))
  
  # Incomplete transList -----------------------------------------------------------
  
  # flowFrame with some transformations
  trns <- estimateLogicle(fs[[4]], c("PE-A","Alexa Fluor 488-A"))
  fst <- transform(fs, trns)
  
  tr <- .getCompleteTransList(fst[[4]], transList = trns)
  
  expect_s4_class(tr, "transformList")
  expect_setequal(names(tr@transforms), getChannels(fs[[4]]))
  
  # flowSet with some transformations
  trns <- estimateLogicle(fs[[4]], c("PE-A","Alexa Fluor 488-A"))
  fst <- transform(fs, trns)
  
  tr <- .getCompleteTransList(fst, transList = trns)
  
  expect_s4_class(tr, "transformList")
  expect_setequal(names(tr@transforms), getChannels(fs[[4]]))
  
  # GatingSet no transforms - incomplete transList
  gst <- GatingSet(fs)
  trns <- estimateLogicle(gst[[4]], c("PE-A","Alexa Fluor 488-A"))
  
  tr <- .getCompleteTransList(gst, trans)
  
  expect_is(tr, "transformerList")
  expect_setequal(names(tr), getChannels(gst))
  
  # GatingSet some transforms - incomplete transList
  gst <- GatingSet(fs)
  trns <- estimateLogicle(gst[[4]], c("PE-A","Alexa Fluor 488-A"))
  gst <- transform(gst, trns)
  
  tr <- .getCompleteTransList(gst, trns)
  
  expect_is(tr, "transformerList")
  expect_setequal(names(tr), getChannels(gst))
  
  # GatingSet some transforms - incomplete transList
  gst <- GatingSet(fs)
  trns <- estimateLogicle(gst[[4]], c("PE-A","Alexa Fluor 488-A"))
  gst <- transform(gst, trns)
  
  trns <- estimateLogicle(gst[[4]], c("PE-A","Alexa Fluor 488-A", "Alexa Fluor 700-A"))
  
  tr <- .getCompleteTransList(gst, trns)
  
  expect_is(tr, "transformerList")
  expect_setequal(names(tr), getChannels(gst))
  
  # GatingSet some transforms - complete transList
  tr <- .getCompleteTransList(gst, trans)
  
  expect_is(tr, "transformerList")
  expect_setequal(names(tr), getChannels(gst))
  
  
})

## .getTransformedData ------------------------------------------------------------

test_that(".getTransformedData", {
  
  expect_error(.getTransformedData(gs[[1]]), "x must be either a flowFrame, flowSet or GatingSet. Subsetted GatingSet should be used instead of GatingHierarchy.")
  
  # flowFrame raw
  fr <- .getTransformedData(fs[[1]])
  fst <- transform(fs, checkTransList(trans, inverse = FALSE))
  
  expect_equal(pData(parameters(fr))[,"maxRange"], pData(parameters(fst[[1]]))[,"maxRange"])
  
  # flowSet raw
  fst <- .getTransformedData(fs)
  fst2 <- transform(fs, checkTransList(trans, inverse = FALSE))
  
  expect_equal(pData(parameters(fst[[1]]))[,"maxRange"], pData(parameters(fst2[[1]]))[,"maxRange"])
  
  # GatingSet raw
  gst <- .getTransformedData(GatingSet(fs, trans))
  
  expect_equal(pData(parameters(getData(gst,"root")[[1]]))[,"maxRange"], pData(parameters(getData(gs,"root")[[1]]))[,"maxRange"])
  
  # flowSet transformed
  fst <- transform(fs, checkTransList(trans, inverse = FALSE))
  fst2 <- .getTransformedData(fst, trans)
  
  expect_equal(pData(parameters(fst2[[1]]))[,"maxRange"], pData(parameters(fst[[1]]))[,"maxRange"])
  
  # flowSet some transformations
  trns <- estimateLogicle(fs[[4]], c("PE-A","Alexa Fluor 700-A"))
  fst <- transform(fs, trns)
  fst2 <- .getTransformedData(fst, trns)
  fst3 <- transform(fs, estimateLogicle(fs[[4]], getChannels(fs)))
  
  expect_equal(pData(parameters(fst2[[1]]))[,"maxRange"], pData(parameters(fst3[[1]]))[,"maxRange"])
  
  # GatingSet some transformations
  gst <- GatingSet(fs)
  trns <- estimateLogicle(gst[[4]], c("PE-A","Alexa Fluor 700-A"))
  gst <- transform(gst, trns)
  gst2 <- .getTransformedData(gst, trns)
  gst3 <- transform(gst, estimateLogicle(gst[[4]], getChannels(fs)))
  
  expect_equal(pData(parameters(getData(gst2,"root")[[1]]))[,"maxRange"], pData(parameters(getData(gst3,"root")[[1]]))[,"maxRange"])
  
})

## .getRawData --------------------------------------------------------------------

test_that(".getRawData", {
  
  # Transformed data without transList
  expect_error(.getRawData(getData(gs, "T Cells")), "Supply a transList object to inverse transformations.")
  
  # Object of incorrect class
  expect_error(.getRawData(gs[[1]]), "x must be either a flowFrame, flowSet or GatingSet. Subsetted GatingSet should be used instead of GatingHierarchy.")
  
  # Untransformed ------------------------------------------------------------------
  
  # flowFrame
  expect_equal(.getRawData(fs[[1]]), fs[[1]])
  
  # flowSet
  expect_equal(.getRawData(fs), fs)
  
  # GatingSet
  gst <- GatingSet(fs)
  expect_equal(pData(parameters(.getRawData(gst)[[1]])), pData(parameters(fs[[1]])))
  
  # Transformed --------------------------------------------------------------------
  
  # flowFrame
  fr <- getData(gs, "T Cells")[[1]]
  inv <- checkTransList(trans, inverse = TRUE)
  fr <- transform(fr, inv)
  
  expect_equal(.getRawData(getData(gs, "T Cells")[[1]], transList = trans), fr)
  expect_equal(.getRawData(getData(gs, "T Cells")[[1]], transList = checkTransList(trans)), fr)
  
  # flowSet
  fst <- getData(gs, "T Cells")
  inv <- checkTransList(trans, inverse = TRUE)
  fst <- transform(fst, inv)
  
  expect_equal(.getRawData(getData(gs, "T Cells"), transList = trans), fst)
  expect_equal(.getRawData(getData(gs, "T Cells"), transList = checkTransList(trans)), fst)
  
  # GatingSet
  fst <- getData(gs, "T Cells")
  inv <- checkTransList(trans, inverse = TRUE)
  fst <- transform(fst, inv)
  
  expect_equal(pData(parameters(.getRawData(gs)[[1]])), pData(parameters(fst[[1]])))
  
  # Some transformations - complete transList --------------------------------------
  fr <- fs[[1]]
  trns <- estimateLogicle(fs[[1]], c("PE-A","Alexa Fluor 700-A"))
  fr <- transform(fr, trns)
  
  trns <- estimateLogicle(fs[[1]], getChannels(fs))
  
  expect_equal(pData(parameters(.getRawData(fr, trns))), pData(parameters(fs[[1]])))
  
})

## computeSpillover ---------------------------------------------------------------

test_that("computeSpillover", {
  
  # GatingSet method --------------------------------------------------------------
  computeSpillover(gs4, parent = "Single Cells")
  
  expect_true(checkFile("Spillover Matrix.csv"))
  
  sp <- read.csv("Spillover Matrix.csv", header = TRUE, row.names = 1)
  colnames(sp) <- rownames(sp)
  
  expect_equal(sp, spill)
  
  # GatingSet - no parent - cmfile
  computeSpillover(gs4, cmfile = "Compensation Channels.csv")
  
  expect_true(checkFile("Spillover Matrix.csv"))
  
  sp <- read.csv("Spillover Matrix.csv", header = TRUE, row.names = 1)
  colnames(sp) <- rownames(sp)
  
  expect_equal(sp, spill)
  
})

unlink("Spillover Matrix.csv")