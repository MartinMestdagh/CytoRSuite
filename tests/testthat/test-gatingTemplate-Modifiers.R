context("gatingTemplate Modifiers")

gs3 <- clone(gs)

## -------------------------------------------------------------------------
# removeGate -

test_that("removeGate", {
  
  expect_error(removeGate(gs3), "Please supply the name of the population to be removed.")
  expect_error(removeGate(gs3, alias = "Test"), "Supplied alias does not exist in the GatingSet.")
  
  expect_error(removeGate(gs3, alias = "T Cells"), "Please supply the name of the gatingTemplate csv file to remove the gate.")
  expect_error(removeGate(gs3, alias = "T Cells", gtfile = "Test.csv"), "Supplied gatingTemplate file does not exist in the current working directory.")
  
  removeGate(gs3, alias = "CD4 T Cells", gtfile = "Activation-gatingTemplate.csv")
  expect_equal(basename(getNodes(gs3)), c("root","Cells","Single Cells","Live Cells","Dendritic Cells","T Cells","CD8 T Cells","CD69+ CD8 T Cells"))
  expect_equal(basename(gatingTemplate("Activation-gatingTemplate.csv")@nodes), c("root","Cells","Single Cells","Live Cells","T Cells","Dendritic Cells","CD8 T Cells","CD69+ CD8 T Cells"))

  removeGate(gs3, alias = c("Dendritic Cells", "T Cells"), gtfile = "Activation-gatingTemplate.csv")
  expect_equal(basename(getNodes(gs3)), c("root","Cells","Single Cells","Live Cells"))
  expect_equal(basename(gatingTemplate("Activation-gatingTemplate.csv")@nodes), c("root","Cells","Single Cells","Live Cells"))
  
  write.csv(gtf, "Activation-gatingTemplate.csv", row.names = FALSE)
  
})

## -------------------------------------------------------------------------
# extractGate -

test_that("extractGate", {
  
  expect_error(extractGate(alias = "T Cells"), "Please supply the name of the parent population.")
  expect_error(extractGate(parent = "T Cells"), "Please supply the name(s) of the alias to extract.", fixed = TRUE)
  expect_error(extractGate(parent = "T Cells", alias = "CD4 T Cells"), "Please supply the name of the gtfile to extract gates from.")
  expect_error(extractGate(parent = "T Cells", alias = "CD4 T Cells", gtfile = "Test.csv"), "Supplied gtfile does not exist in the current working directory.")
  
  gate <- extractGate(parent = "T Cells", alias = c("CD4 T Cells","CD8 T Cells"), gtfile = "Activation-gatingTemplate.csv")
  
  coords <- matrix(c(1.7257,3.5148, -0.1326, 1.9919), ncol = 2, nrow = 2)
  colnames(coords) <- c("Alexa Fluor 700-A","Alexa Fluor 488-A")
  rownames(coords) <- c("min","max")
  test1 <- rectangleGate(filterId = "CD4 T Cells", .gate = coords)
  
  coords <- matrix(c(-0.3480,1.8883, 2.1335, 4.0759), ncol = 2, nrow = 2)
  colnames(coords) <- c("Alexa Fluor 700-A","Alexa Fluor 488-A")
  rownames(coords) <- c("min","max")
  test2 <- rectangleGate(filterId = "CD8 T Cells", .gate = coords)
  
  expect_equal(gate, list(list(filters(list(test1))), list(filters(list(test2)))), tolerance = 0.01)
  
})

## -------------------------------------------------------------------------
# editGate -

test_that("editGate", {
  
  gs3 <- clone(gs)
  
  expect_error(editGate(gs), "Please supply the name of the parent population.")
  expect_error(editGate(gs, parent = "Test"), "Supplied parent does not exist in the GatingSet.")
  expect_error(editGate(gs, parent = "T Cells"), "Please supply the name(s) of the gates to edit to the alias argument.", fixed =  TRUE)
  expect_error(editGate(gs, parent = "T Cells", alias = "Test"), "Supplied alias does not exist in the GatingSet.")
  expect_error(editGate(gs, parent = "T Cells", alias = "CD4 T Cells"), "Please supply the name of gatingTemplate to the gtfile argument.")
  expect_error(editGate(gs, parent = "T Cells", alias = "CD4 T Cells", gtfile = "Test.csv"), "Supplied gatingTemplate does not exist in the current working directory.")
  
  editGate(gs3, parent = "root", alias = "Cells", gtfile = "Activation-gatingTemplate.csv")
  
  expect_equal(getGate(gs3, "Cells")[[1]], pg)
  expect_equal(extractGate(parent = "root", alias = "Cells", "Activation-gatingTemplate.csv")[[1]][[1]][[1]], pg)
  
  editGate(gs3, parent = "root", alias = "Cells", overlay = "CD4 T Cells", gtfile = "Activation-gatingTemplate.csv", type = "r")
  
  expect_equal(getGate(gs3, "Cells")[[1]], rg)
  expect_equal(extractGate(parent = "root", alias = "Cells", "Activation-gatingTemplate.csv")[[1]][[1]][[1]], rg)
  
  write.csv(gtf, "Activation-gatingTemplate.csv", row.names = FALSE)
  
})

## -------------------------------------------------------------------------
# getGateType -

test_that("getGateType", {
  
  expect_equal(getGateType(filters(list(rg))), "rectangle")
  expect_equal(getGateType(filters(list(pg))), "polygon")
  expect_equal(getGateType(filters(list(igx))), "interval")
  expect_equal(getGateType(filters(list(ig))), "interval")
  expect_equal(getGateType(filters(list(igy))), "interval")
  expect_equal(getGateType(filters(list(tg))), "threshold")
  expect_equal(getGateType(filters(list(tg1))), "threshold")
  expect_equal(getGateType(filters(list(bg))), "boundary")
  expect_equal(getGateType(filters(list(bg1))), "boundary")
  expect_equal(getGateType(filters(list(eg))), "ellipse")
  expect_equal(getGateType(qg), "quadrant")
  expect_equal(getGateType(wg), "web")
  expect_equal(getGateType(filters(list(ig,tg,bg))), c("interval","threshold","boundary"))
  expect_equal(getGateType(filters(list(eg, eg, eg, eg))), c("ellipse","ellipse","ellipse","ellipse"))
  expect_equal(getGateType(filters(list(igx, igx, igx, igx))), c("interval","interval","interval","interval"))
  expect_equal(getGateType(filters(list(pg, pg, pg, pg))), c("polygon","polygon","polygon","polygon"))
  expect_equal(getGateType(filters(list(rg,pg,ig,tg,bg,eg))), c("rectangle","polygon","interval","threshold","boundary","ellipse"))
  expect_equal(getGateType(filters(list(rg,ig, tg,bg))), c("rectangle","interval","threshold","boundary"))
  
})

## -------------------------------------------------------------------------
# convertGatingTemplate -

test_that("convertGatingTemplate", {
  

  
})

write.csv(gtf, "Activation-gatingTemplate.csv", row.names = FALSE)
