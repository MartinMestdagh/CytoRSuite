## ----------------------------------------------------------------------
# Load in vdiffr for image comparison -
library(vdiffr)

## ----------------------------------------------------------------------
# Assign Activation dataset to fs -
fs <- Activation

# pData information -
pData(fs)$Samples <- c("Control","Activated")
chnls <- c("Alexa Fluor 405-A","Alexa Fluor 430-A","APC-Cy7-A", "PE-A", "Alexa Fluor 488-A", "Alexa Fluor 700-A", "Alexa Fluor 647-A", "7-AAD-A") 
markers <- c("Hoechst-405", "Hoechst-430", "CD11c", "Va2", "CD8", "CD4", "CD44", "CD69")
names(markers) <- chnls
markernames(fs) <- markers

# GatingSet -
gs <- GatingSet(fs)

# Compensation -
gs <- compensate(gs, fs[[1]]@description$SPILL)

# Transformation -
trans <- estimateLogicle(gs[[2]], getChannels(gs))
gs <- transform(gs, trans)

# gatingTemplate -
gt <- gatingTemplate("Example-gatingTemplate.csv")

# gatingTemplate file -
gtf <- read.csv("Example-gatingTemplate.csv")

# Gating -
gating(gt, gs)

## --------------------------------------------------------------------
# Construct gate objects -

# rectangleGate -
coords <- matrix(c(25000,150000, 5000,150000), ncol = 2, nrow = 2)
colnames(coords) <- c("FSC-A","SSC-A")
rownames(coords) <- c("min","max")
rg <- rectangleGate(filterId = "Cells", .gate = coords)

# polygonGate -
coords <- matrix(c(50000,100000,100000,75000,50000, 10000,10000,60000,85000,60000), ncol = 2, nrow = 5)
colnames(coords) <- c("FSC-A","SSC-A")
pg <- polygonGate(filterId = "Cells", .gate = coords)

# intervalGate -
coords <- matrix(c(25000,150000,-Inf,Inf), ncol = 2, nrow = 2)
colnames(coords) <- c("FSC-A","SSC-A")
rownames(coords) <- c("min","max")
ig <- rectangleGate(filterId = "Cells", .gate = coords)

# thresholdGate -
coords <- matrix(c(25000,Inf,5000,Inf), ncol = 2, nrow = 2)
colnames(coords) <- c("FSC-A","SSC-A")
rownames(coords) <- c("min","max")
tg <- rectangleGate(filterId = "Cells", .gate = coords)

# boundaryGate -
coords <- matrix(c(-Inf, 200000, -Inf, 200000), ncol = 2, nrow = 2)
colnames(coords) <- c("FSC-A","SSC-A")
rownames(coords) <- c("min","max")
bg <- rectangleGate(filterId = "Cells", .gate = coords)

# ellipsoidGate -
cov <- matrix(c(900000000, 0.00000008304362, 0.00000008304362, 2256250000), ncol=2,
              dimnames=list(c("FSC-A", "SSC-A"), c("FSC-A", "SSC-A")))
mean <- c("FSC-A"=65000, "SSC-A"=51250)
eg <- ellipsoidGate(filterId= "Cells", .gate=cov, mean=mean)

# drawQuadrants -
coords <- matrix(c(-Inf, 150000, -Inf, 150000), ncol=2, dimnames=list(c("min", "max"), c("FSC-A", "SSC-A")))
rg1 <- rectangleGate(filterId = "A", .gate = coords)
coords <- matrix(c(150000, Inf, -Inf, 150000), ncol=2, dimnames=list(c("min", "max"), c("FSC-A", "SSC-A")))
rg2 <- rectangleGate(filterId = "B", .gate = coords)
coords <- matrix(c(150000, Inf, 150000, Inf), ncol=2, dimnames=list(c("min", "max"), c("FSC-A", "SSC-A")))
rg3 <- rectangleGate(filterId = "C", .gate = coords)
coords <- matrix(c(-Inf, 150000, 150000, Inf), ncol=2, dimnames=list(c("min", "max"), c("FSC-A", "SSC-A")))
rg4 <- rectangleGate(filterId = "D", .gate = coords)
qg <- filters(list(rg1, rg2, rg3, rg4))

# drawWeb -
coords <- matrix(c(150000,50486.54,262143,262143,150000,729.81,729.81,37857), ncol = 2, nrow = 4)
colnames(coords) <- c("FSC-A","SSC-A")
pg1 <- polygonGate(filterId = "A", .gate = coords)
coords <- matrix(c(150000,262143,262143,150000,150000,37857,262413,262143), ncol = 2, nrow = 4)
colnames(coords) <- c("FSC-A","SSC-A")
pg2 <- polygonGate(filterId = "B", .gate = coords)
coords <- matrix(c(150000,150000,4610.2,4610.2,50486.54,150000,262413,262143,729.81,729.81), ncol = 2, nrow = 5)
colnames(coords) <- c("FSC-A","SSC-A")
pg3 <- polygonGate(filterId = "C", .gate = coords)
wg <- filters(list(pg1,pg2,pg3))
