context("Helper Functions")

## ------------------------------------------------------------------
# sampleFrame -

expect_equal(nrow(exprs(sampleFrame(fs[[1]],25000))), 25000)

## ------------------------------------------------------------------
# selectFrames -

pData(fs)$Treatment <- c("Control","Activated")

expect_equal(selectFrames(fs, c("Treatment","Control"))[[1]], fs[[1]])
expect_equal(selectFrames(fs, c("Treatment","Activated"))[[1]], fs[[2]])