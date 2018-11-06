library(vdiffr)
library(grDevices)

fs <- Activation

pData(fs)$Samples <- c("Control","Activated")
chnls <- c("Alexa Fluor 405-A","Alexa Fluor 430-A","APC-Cy7-A", "PE-A", "Alexa Fluor 488-A", "Alexa Fluor 700-A", "Alexa Fluor 647-A", "7-AAD-A") 
markers <- c("Hoechst-405", "Hoechst-430", "CD11c", "Va2", "CD8", "CD4", "CD44", "CD69")
names(markers) <- chnls
markernames(fs) <- markers

gs <- GatingSet(fs)
gs <- compensate(gs, fs[[1]]@description$SPILL)

trans <- estimateLogicle(gs[[2]], getChannels(gs))
gs <- transform(gs, trans)