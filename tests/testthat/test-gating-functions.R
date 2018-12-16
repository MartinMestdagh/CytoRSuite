context("Gating Functions")

## drawPolygon -----------------------------------------------------------------------

test_that("drawPolygon", {
  
  expect_error(drawPolygon(fs[[1]], channels = "FSC-A"), "Please supply a name for a the gated population as the alias argument.")
  
  dp <- drawPolygon(fs[[1]], alias = "Cells", channels = c("FSC-A","SSC-A"), subSample = 100)
  
  expect_s4_class(dp, "filters")
  expect_s4_class(dp[[1]], "polygonGate")
  expect_equal(dp[[1]], pg)
  expect_equal(parameters(dp[[1]]), parameters(pg))
  
})

## drawRectangle ---------------------------------------------------------------------

test_that("drawRectangle", {
  
  expect_error(drawRectangle(fs[[1]], channels = "FSC-A"), "Please supply a name for a the gated population as the alias argument.")
  
  dr <- drawRectangle(fs[[1]], alias = "Cells", channels = c("FSC-A","SSC-A"), subSample = 100)
  
  expect_s4_class(dr, "filters")
  expect_s4_class(dr[[1]], "rectangleGate")
  expect_equal(dr[[1]], rg)
  expect_equal(parameters(dr[[1]]), parameters(rg))
  
})

## drawinterval ----------------------------------------------------------------------

test_that("drawInterval", {
  
  # 1-D ---------------------------------------------------------------
  di <- drawInterval(fs[[1]], alias = "Cells", channels = "FSC-A")
  
  expect_s4_class(di, "filters")
  expect_s4_class(di[[1]], "rectangleGate")
  expect_equal(di[[1]], igx)
  expect_equal(parameters(di[[1]]), parameters(igx))
  
  expect_error(drawInterval(fs[[1]], alias = "Cells", channels = "FSC-A", axis = "y"), "Cannot gate y axis if a single channel is supplied.")
  expect_error(drawInterval(fs[[1]], channels = "FSC-A"), "Please supply a name for a the gated population as the alias argument.")
  
  # 2-D x axis --------------------------------------------------------
  di <- drawInterval(fs[[1]], alias = "Cells", channels = c("FSC-A","SSC-A"), subSample = 100)
  
  expect_s4_class(di, "filters")
  expect_s4_class(di[[1]], "rectangleGate")
  expect_equal(di[[1]], ig)
  expect_equal(parameters(di[[1]]), parameters(ig))
  
  # 2-D y axis --------------------------------------------------------
  di <- drawInterval(fs[[1]], alias = "Cells", channels = c("FSC-A","SSC-A"), subSample = 100, axis = "y")
  
  expect_s4_class(di, "filters")
  expect_s4_class(di[[1]], "rectangleGate")
  expect_equal(di[[1]], igy)
  expect_equal(parameters(di[[1]]), parameters(igy))
  
})

## drawThreshold ------------------------------------------------------

test_that("drawThreshold", {
  
  expect_error(drawThreshold(fs[[1]], channels = "FSC-A"), "Please supply a name for a the gated population as the alias argument.")
  expect_error(drawThreshold(fs[[1]], alias = c("A","B"), channels = "FSC-A"), "Multiple threshold gates are not supported.")
  
  # 1-D ---------------------------------------------------------------
  dt <- drawThreshold(fs[[1]], alias = "Cells", channels = "FSC-A")
  
  expect_s4_class(dt, "filters")
  expect_s4_class(dt[[1]], "rectangleGate")
  expect_equal(dt[[1]], tg1)
  expect_equal(parameters(dt[[1]]), parameters(tg1))
  
  # 2-D ---------------------------------------------------------------
  dt <- drawThreshold(fs[[1]], alias = "Cells", channels = c("FSC-A","SSC-A"), subSample = 100)
  
  expect_s4_class(dt, "filters")
  expect_s4_class(dt[[1]], "rectangleGate")
  expect_equal(dt[[1]], tg)
  expect_equal(parameters(dt[[1]]), parameters(tg))
  
})

## drawBoundary -------------------------------------------------------

test_that("drawBoundary", {
  
  expect_error(drawBoundary(fs[[1]], channels = "FSC-A"), "Please supply a name for a the gated population as the alias argument.")
  expect_error(drawBoundary(fs[[1]], alias = c("A","B"), channels = "FSC-A"), "Multiple boundary gates are not supported.")

  # 1-D ---------------------------------------------------------------
  db <- drawBoundary(fs[[1]], alias = "Cells", channels = "FSC-A")
  
  expect_s4_class(db, "filters")
  expect_s4_class(db[[1]], "rectangleGate")
  expect_equal(db[[1]], bg1)
  expect_equal(parameters(db[[1]]), parameters(bg1))
  
  # 2-D ---------------------------------------------------------------
  db <- drawBoundary(fs[[1]], alias = "Cells", channels = c("FSC-A","SSC-A"), subSample = 100)
  
  expect_s4_class(db, "filters")
  expect_s4_class(db[[1]], "rectangleGate")
  expect_equal(db[[1]], bg)
  expect_equal(parameters(db[[1]]), parameters(bg))

})

## drawEllipse --------------------------------------------------------

test_that("drawEllipse",{
  
  expect_error(drawEllipse(fs[[1]], channels = "FSC-A"), "Please supply a name for a the gated population as the alias argument.")
  
  de <- drawEllipse(fs[[1]], alias = "Cells", channels = c("FSC-A","SSC-A"), subSample = 100)
  
  expect_s4_class(de, "filters")
  expect_s4_class(de[[1]], "ellipsoidGate")
  expect_equal(de[[1]]@mean, eg@mean)
  expect_equal(round(de[[1]]@cov, 8), round(eg@cov, 8))
  
})

## drawQuadrants -------------------------------------------------------

test_that("drawQuadrants",{
  
  expect_error(drawQuadrants(fs[[1]], channels = "FSC-A"), "Please supply a name for a the gated population as the alias argument.")
  expect_error(drawQuadrants(fs[[1]], alias = "A", channels = "FSC-A"), "Supply 4 population names as the alias argument to construct a set of quadrant gates.")
  
  dq <- drawQuadrants(fs[[1]], alias = c("A","B","C","D"), channels = c("FSC-A","SSC-A"), subSample = 100)
  
  expect_s4_class(dq, "filters")
  expect_length(dq, 4)
  expect_equal(as.vector(sapply(dq,class)), rep("rectangleGate", 4))
  expect_equal(qg, dq)
  
})

## drawweb -------------------------------------------------------------

test_that("drawWeb",{
  
  expect_error(drawWeb(fs[[1]], channels = "FSC-A"), "Please supply a name for a the gated population as the alias argument.")
  
  dw <- drawWeb(fs[[1]], alias = c("A","B","C","D","E","F","G","H"), channels = c("FSC-A","SSC-A"), subSample = 100)
  
  expect_s4_class(dw, "filters")
  expect_length(dw, 8)
  expect_equal(as.vector(sapply(dw,class)), rep("polygonGate", 8))
  expect_equal(wg, dw, tolerance = 0.01)
  
})