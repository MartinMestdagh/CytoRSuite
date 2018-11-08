context("Gating Functions")

test_that(".onLoad set cytoRSuite_interact to TRUE", {
  
  cytoRSuite:::.onLoad()
  expect_true(getOption("cytoRSuite_interact") == interactive())
  
  # Set cytoRSuite_interact to FALSE to avoid user input when running tests
  options("cytoRSuite_interact" = FALSE)
  
})

## -----------------------------------------------------------------------------------
# drawPolygon -

test_that("drawPolygon returns the appropriate gates", {
  
  coords <- matrix(c(50000,100000,100000,75000,50000, 10000,10000,60000,85000,60000), ncol = 2, nrow = 5)
  colnames(coords) <- c("FSC-A","SSC-A")
  pg <- polygonGate(filterId = "Cells", .gate = coords)
  dp <- drawPolygon(fs[[1]], alias = "Cells", channels = c("FSC-A","SSC-A"))
  
  expect_s4_class(dp, "filters")
  expect_s4_class(dp[[1]], "polygonGate")
  expect_equal(dp[[1]], pg)
  expect_equal(parameters(dp[[1]]), parameters(pg))
  
})

## -----------------------------------------------------------------------------------
## drawRectangle -

test_that("drawRectangle returns the appropriate gates", {
  
  coords <- matrix(c(25000,150000, 5000,150000), ncol = 2, nrow = 2)
  colnames(coords) <- c("FSC-A","SSC-A")
  rownames(coords) <- c("min","max")
  rg <- rectangleGate(filterId = "Cells", .gate = coords)
  dr <- drawRectangle(fs[[1]], alias = "Cells", channels = c("FSC-A","SSC-A"))
  
  expect_s4_class(dr, "filters")
  expect_s4_class(dr[[1]], "rectangleGate")
  expect_equal(dr[[1]], rg)
  expect_equal(parameters(dr[[1]]), parameters(rg))
  
})

## -----------------------------------------------------------------------------------
# drawInterval -

test_that("drawInterval returns the appropriate gates", {
  
  coords <- matrix(c(25000,150000,-Inf,Inf), ncol = 2, nrow = 2)
  colnames(coords) <- c("FSC-A","SSC-A")
  rownames(coords) <- c("min","max")
  ig <- rectangleGate(filterId = "Cells", .gate = coords)
  di <- drawInterval(fs[[1]], alias = "Cells", channels = c("FSC-A","SSC-A"))
  
  expect_s4_class(di, "filters")
  expect_s4_class(di[[1]], "rectangleGate")
  expect_equal(di[[1]], ig)
  expect_equal(parameters(di[[1]]), parameters(ig))
  
})

## -----------------------------------------------------------------------------------
# drawThreshold -

test_that("drawThreshold returns the appropriate gates", {
  
  coords <- matrix(c(25000,Inf,5000,Inf), ncol = 2, nrow = 2)
  colnames(coords) <- c("FSC-A","SSC-A")
  rownames(coords) <- c("min","max")
  tg <- rectangleGate(filterId = "Cells", .gate = coords)
  dt <- drawThreshold(fs[[1]], alias = "Cells", channels = c("FSC-A","SSC-A"))
  
  expect_s4_class(dt, "filters")
  expect_s4_class(dt[[1]], "rectangleGate")
  expect_equal(dt[[1]], tg)
  expect_equal(parameters(dt[[1]]), parameters(tg))
  
})

## -----------------------------------------------------------------------------------
# drawBoundary -

test_that("drawBoundary returns the appropriate gates", {

  coords <- matrix(c(-Inf, 200000, -Inf, 200000), ncol = 2, nrow = 2)
  colnames(coords) <- c("FSC-A","SSC-A")
  rownames(coords) <- c("min","max")
  tb <- rectangleGate(filterId = "Cells", .gate = coords)
  db <- drawBoundary(fs[[1]], alias = "Cells", channels = c("FSC-A","SSC-A"))
  
  expect_s4_class(db, "filters")
  expect_s4_class(db[[1]], "rectangleGate")
  expect_equal(db[[1]], tb)
  expect_equal(parameters(db[[1]]), parameters(tb))
  
})

## -----------------------------------------------------------------------------------
# drawEllipse -

test_that("drawEllipse returns the appropriate gates",{
  
  de <- drawEllipse(fs[[1]], alias = "Cells", channels = c("FSC-A","SSC-A"))
  
  expect_s4_class(de, "filters")
  expect_s4_class(de[[1]], "ellipsoidGate")
  
})

## -----------------------------------------------------------------------------------
# drawQuadrants -

test_that("drawQuadrants returns the appropriate gates",{
  
  dq <- drawQuadrants(fs[[1]], alias = c("A","B","C","D"), channels = c("FSC-A","SSC-A"))
  
  expect_s4_class(dq, "filters")
  expect_length(dq, 4)
  expect_s4_class(dq[[1]], "rectangleGate")
  
})

## -----------------------------------------------------------------------------------
# drawWeb -

test_that("drawWeb returns the appropriate gates",{
  
  dw <- drawWeb(fs[[1]], alias = c("A","B","C"), channels = c("FSC-A","SSC-A"))
  
  expect_s4_class(dw, "filters")
  expect_length(dw, 3)
  expect_s4_class(dw[[1]], "polygonGate")
  
})