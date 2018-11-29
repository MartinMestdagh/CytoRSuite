context("gatingTemplate Modifiers")

## -------------------------------------------------------------
# removeGate single alias -

gtf <- read.csv("Example-gatingTemplate.csv", header = TRUE)
gt <- gatingTemplate("Example-gatingTemplate.csv")

gating(gt,gs)

test_that("removeGate properly removes alias and descendants from GatingSet and gatingTemplate", {
  
  removeGate(gs, alias = "T Cells", gtfile = "Example-gatingTemplate.csv")
  expect_equal(basename(getNodes(gs)),c("root","Cells","Single Cells", "Live Cells", "Dendritic Cells"))
  
  gtn <- read.csv("Example-gatingTemplate.csv", header = TRUE)
  expect_equal(length(c("root",gtn$alias)),length(getNodes(gs)))
  expect_equal(c("root", as.character(gtn$alias)),basename(getNodes(gs)))
  
})

write.csv(gtf, "Example-gatingTemplate.csv", row.names = FALSE)

## ---------------------------------------------------------------
# removeGate multiple alias -

gt <- gatingTemplate("Example-gatingTemplate.csv")

gating(gt,gs)

test_that("removeGate properly removes muyltiple aliases and descendants from GatingSet and gatingTemplate", {
  
  removeGate(gs, alias = c("Live Cells","T Cells","Dendritic Cells"), gtfile = "Example-gatingTemplate.csv")
  expect_equal(basename(getNodes(gs)),c("root","Cells","Single Cells"))
  
  gtn <- read.csv("Example-gatingTemplate.csv", header = TRUE)
  expect_equal(length(c("root",gtn$alias)),length(getNodes(gs)))
  expect_equal(c("root", as.character(gtn$alias)),basename(getNodes(gs)))
  
})

write.csv(gtf, "Example-gatingTemplate.csv", row.names = FALSE)

## ----------------------------------------------------------------
# extractGate -

gt <- gatingTemplate("Example-gatingTemplate.csv")

gating(gt,gs)

test_that("extractGate successfully extracts multiple alias from gatingTemplate", {
  
  gates <- extractGate(parent = "Live Cells", alias = c("T Cells","Dendritic Cells"), gtfile = "Example-gatingTemplate.csv")
  expect_s4_class(gates, "filters")
  expect_s4_class(gates[[1]],"ellipsoidGate")
  expect_s4_class(gates[[2]],"rectangleGate")
  
  expect_error(extractGate(alias = c("T Cells","Dendritic Cells"), gtfile = "Example-gatingTemplate.csv"), "Please supply the name of the parent population.", fixed = TRUE)
  expect_error(extractGate(parent = "Live Cells", gtfile = "Example-gatingTemplate.csv"), "Please supply the name(s) of the alias to extract.", fixed = TRUE)
  expect_error(extractGate(parent = "Live Cells", alias = "T Cells"), "Please supply the name of the gtfile to extract gates from.", fixed = TRUE)
  expect_error(extractGate(parent = "Live Cells", alias = "T Cells", gtfile = "Example.csv"), "Supplied gtfile does not exist in the current working directory.", fixed = TRUE)
  
})

## -----------------------------------------------------------------
# getGateType -

test_that("getGateType returns correct gate types for drawGate", {
  
  r1 <- rectangleGate("FSC-A" = c(50,250), "SSC-A" = c(50,250))
  expect_equal(getGateType(filters(list(r1))), "rectangle")
  r2 <- rectangleGate("FSC-A" = c(50,250), "SSC-A" = c(-Inf,Inf))
  expect_equal(getGateType(filters(list(r2))), "interval")
  r3 <- rectangleGate("FSC-A" = c(50,Inf), "SSC-A" = c(100,Inf))
  expect_equal(getGateType(filters(list(r3))), "threshold")
  r4 <- rectangleGate("FSC-A" = c(-Inf,50), "SSC-A" = c(-Inf,100))
  expect_equal(getGateType(filters(list(r4))), "boundary")
  r5 <- rectangleGate("FSC-A" = c(50,250))
  expect_equal(getGateType(filters(list(r5))), "interval")
  r6 <- rectangleGate("FSC-A" = c(50,Inf))
  expect_equal(getGateType(filters(list(r6))), "threshold")
  r7 <- rectangleGate("FSC-A" = c(-Inf,50))
  expect_equal(getGateType(filters(list(r7))), "boundary")
  rs <- filters(list(r1,r2,r3,r4))
  expect_equal(getGateType(rs), c("rectangle","interval","threshold","boundary"))
  rs <- filters(list(r5,r6,r7))
  expect_equal(getGateType(rs), c("interval","threshold","boundary"))
  
  sqrcut <- matrix(c(300,300,600,600,50,300,300,50),ncol=2,nrow=4)
  colnames(sqrcut) <- c("FSC-A","SSC-A")
  p1 <- polygonGate(.gate = sqrcut)
  expect_equal(getGateType(filters(list(p1))), "polygon")
  
  
  cov <- matrix(c(6879, 3612, 3612, 5215), ncol=2,
                dimnames=list(c("FSC-A", "SSC-A"), c("FSC-A", "SSC-A")))
  mean <- c("FSC-A"=430, "SSC-A"=175)
  e1 <- ellipsoidGate(.gate=cov, mean=mean)
  expect_equal(getGateType(filters(list(e1))), "ellipse")
  
  gts <- filters(list(e1,p1))
  expect_equal(getGateType(gts), c("ellipse","polygon"))
  
})