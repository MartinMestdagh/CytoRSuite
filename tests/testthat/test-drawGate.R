context("drawGate")

test_that(".onLoad set CytoRSuite_interact to TRUE", {
  
  CytoRSuite:::.onLoad()
  expect_true(getOption("CytoRSuite_interact") == interactive())
  
  # Set cytoRSuite_interact to FALSE to avoid user input when running tests
  options("CytoRSuite_interact" = FALSE)
  
})

## --------------------------------------------------------------------------------------
# flowFrame method -

test_that("drawGate flowFrame method returns appropriate gates", {
  
  gts <- drawGate(fs[[1]], alias = c("Cells","Cells","Cells","Cells","Cells","Cells"), channels = c("FSC-A","SSC-A"), type = c("r","p","e","i","t","b"), subSample = 100)
  
  expect_s4_class(gts, "filters")
  expect_equal(lapply(gts,function(x) {class(x)[1]}), list(rectangle = "rectangleGate", polygon = "polygonGate", ellipse = "ellipsoidGate", interval = "rectangleGate", threshold = "rectangleGate", boundary = "rectangleGate"))
  expect_length(gts, 6)
  expect_equal(unname(gts), filters(list(rg,pg,eg,ig,tg,bg)), tolerance = 0.01)
  
  gts <- drawGate(fs[[1]], alias = c("A","B","C","D"), channels = c("FSC-A","SSC-A"), type = "q", subSample = 100)
  
  expect_s4_class(gts, "filters")
  expect_equal(lapply(gts,function(x) {class(x)[1]}), list("rectangleGate","rectangleGate","rectangleGate","rectangleGate"))
  expect_length(gts, 4)
  expect_equal(gts, qg)
  
  gts <- drawGate(fs[[1]], alias = c("A","B","C","D","E","F","G","H"), channels = c("FSC-A","SSC-A"), type = "w", subSample = 100)
  
  expect_s4_class(gts, "filters")
  expect_equal(as.vector(sapply(gts,function(x) {class(x)[1]})), rep("polygonGate",8))
  expect_length(gts, 8)
  expect_equal(gts, wg, tolerance = 0.01)
  
  gts <- drawGate(fs[[1]], alias = "Cells", channels = "FSC-A")
  
  expect_equal(unname(gts), filters(list(igx)))
  
})

## --------------------------------------------------------------------------------------
# flowSet method -

test_that("drawGate flowSet method returns appropriate gates", {
    
    gts <- drawGate(fs, select = 1, alias = c("Cells","Cells","Cells","Cells","Cells","Cells"), channels = c("FSC-A","SSC-A"), type = c("r","p","e","i","t","b"), subSample = 100)
    
    expect_s4_class(gts, "filters")
    expect_equal(lapply(gts,function(x) {class(x)[1]}), list(rectangle = "rectangleGate", polygon = "polygonGate", ellipse = "ellipsoidGate", interval = "rectangleGate", threshold = "rectangleGate", boundary = "rectangleGate"))
    expect_length(gts, 6)
    expect_equal(unname(gts), filters(list(rg,pg,eg,ig,tg,bg)), tolerance = 0.01)
    
    expect_error(drawGate(fs, select = "A", alias = "Cells", channels = c("FSC-A","SSC-A")), "Vector supplied to select argument should contain the numeric indicies of the samples to select.")
    
    gts <- drawGate(fs, alias = c("A","B","C","D"), channels = c("FSC-A","SSC-A"), type = "q", subSample = 100)
    
    expect_s4_class(gts, "filters")
    expect_equal(lapply(gts,function(x) {class(x)[1]}), list("rectangleGate","rectangleGate","rectangleGate","rectangleGate"))
    expect_length(gts, 4)
    expect_equal(gts, qg)
    
    gts <- drawGate(fs, alias = c("A","B","C","D","E","F","G","H"), channels = c("FSC-A","SSC-A"), type = "w", subSample = 100)
    
    expect_s4_class(gts, "filters")
    expect_equal(as.vector(sapply(gts,function(x) {class(x)[1]})), rep("polygonGate",8))
    expect_length(gts, 8)
    expect_equal(gts, wg, tolerance = 0.01)
    
    gts <- drawGate(fs, alias = "Cells", channels = "FSC-A")
    
    expect_equal(unname(gts), filters(list(igx)))
    
  })

## --------------------------------------------------------------------------------------
# GatingSet method -

test_that("drawGate GatingSet methods returns appropriate gates and applies them to the GatingSet", {
  
  gs1 <- GatingSet(fs)
    
  drawGate(gs1, parent = "root", alias = c("x","y","z"), channels = c("FSC-A","SSC-A"), type = c("e","p","i"), subSample = 100, gtfile = "gatingTemplate.csv")
  
  expect_equal(basename(getNodes(gs1)), c("root","x","y","z"))
  
  drawGate(gs1, parent = "root", alias = c("K","L","M"), channels = c("FSC-A","SSC-A"), type = "w", subSample = 100)
  
  expect_equal(basename(getNodes(gs1)), c("root","x","y","z","K","L","M"))
  
  gby <- drawGate(gs1, groupBy = "OVAConc", parent = "root", alias = c("X"), channels = c("FSC-A","SSC-A"), type = "p", subSample = 100)

  expect_equal(basename(getNodes(gs1)), c("root","x","y","z","K","L","M","X"))
  
})
unlink("gatingTemplate.csv")