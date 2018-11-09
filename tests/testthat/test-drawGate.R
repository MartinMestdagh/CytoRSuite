context("drawGate")

test_that(".onLoad set cytoRSuite_interact to TRUE", {
  
  cytoRSuite:::.onLoad()
  expect_true(getOption("cytoRSuite_interact") == interactive())
  
  # Set cytoRSuite_interact to FALSE to avoid user input when running tests
  options("cytoRSuite_interact" = FALSE)
  
})

## --------------------------------------------------------------------------------------
# flowFrame method -

test_that("drawGate flowFrame method returns appropriate gates", {
  
  gts <- drawGate(fs[[1]], alias = c("Cells","Cells","Cells","Cells","Cells","Cells"), channels = c("FSC-A","SSC-A"), type = c("r","p","e","i","t","b"))
  
  expect_s4_class(gts, "filters")
  expect_equal(lapply(gts,function(x) {class(x)[1]}), list(rectangle = "rectangleGate", polygon = "polygonGate", ellipse = "ellipsoidGate", interval = "rectangleGate", threshold = "rectangleGate", boundary = "rectangleGate"))
  expect_length(gts, 6)
  
  gts <- drawGate(fs[[1]], alias = c("A","B","C","D"), channels = c("FSC-A","SSC-A"), type = "q")
  
  expect_s4_class(gts, "filters")
  expect_equal(lapply(gts,function(x) {class(x)[1]}), list("rectangleGate","rectangleGate","rectangleGate","rectangleGate"))
  expect_length(gts, 4)
  
  gts <- drawGate(fs[[1]], alias = c("A","B","C"), channels = c("FSC-A","SSC-A"), type = "w")
  
  expect_s4_class(gts, "filters")
  expect_equal(lapply(gts,function(x) {class(x)[1]}), list("polygonGate","polygonGate","polygonGate"))
  expect_length(gts, 3)
  
})

## --------------------------------------------------------------------------------------
# flowSet method -

test_that("drawGate flowSet method returns appropriate gates", {
  
  test_that("drawGate flowFrame method returns appropriate gates", {
    
    gts <- drawGate(fs, alias = c("Cells","Cells","Cells","Cells","Cells","Cells"), channels = c("FSC-A","SSC-A"), type = c("r","p","e","i","t","b"))
    
    expect_s4_class(gts, "filters")
    expect_equal(lapply(gts,function(x) {class(x)[1]}), list(rectangle = "rectangleGate", polygon = "polygonGate", ellipse = "ellipsoidGate", interval = "rectangleGate", threshold = "rectangleGate", boundary = "rectangleGate"))
    expect_length(gts, 6)
    
    gts <- drawGate(fs, alias = c("A","B","C","D"), channels = c("FSC-A","SSC-A"), type = "q")
    
    expect_s4_class(gts, "filters")
    expect_equal(lapply(gts,function(x) {class(x)[1]}), list("rectangleGate","rectangleGate","rectangleGate","rectangleGate"))
    expect_length(gts, 4)
    
    gts <- drawGate(fs, alias = c("A","B","C"), channels = c("FSC-A","SSC-A"), type = "w")
    
    expect_s4_class(gts, "filters")
    expect_equal(lapply(gts,function(x) {class(x)[1]}), list("polygonGate","polygonGate","polygonGate"))
    expect_length(gts, 3)
    
  })
  
})

## --------------------------------------------------------------------------------------
# GatingSet method -

test_that("drawGate GatingSet methods returns appropriate gates and applies them to the GatingSet", {
  
  mapply(function(x){
    
  gs1 <- GatingSet(fs)
  
  drawGate(gs1, parent = "root", alias = c("Cells"), channels = c("FSC-A","SSC-A"), type = x)
  
  expect_s4_equal(basename(getNodes(gs1)), c("root","Cells"))

  }, c("r","p","e","i","t","b"), c("A","B","C","D","E","F"))
  
  drawGate(gs1, parent = "root", alias = c("G","H","I","J"), channels = c("FSC-A","SSC-A"), type = "q")
  
  expect_equal(basename(getNodes(gs1)), c("root","A","B","C","D","E","F","G","H",'I',"J"))
  
  drawGate(gs1, parent = "root", alias = c("K","L","M"), channels = c("FSC-A","SSC-A"), type = "w")
  
  expect_equal(basename(getNodes(gs1)), c("root","A","B","C","D","E","F","G","H",'I',"J","K","L","M"))
  
})