#' Automatically register drawGate and manualGate with openCyto upon loading...
#' 
#' @noRd
.onLoad <- function(libname,pkgname){
  
  options("cytoRSuite_interact" = interactive())
  openCyto::registerPlugins(fun = gate_draw, methodName = "drawGate")
  openCyto::registerPlugins(fun = gate_manual, methodName = "manualGate")
  
}
