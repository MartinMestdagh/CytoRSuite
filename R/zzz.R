#' Automatically register ppdrawGate, drawGate and manualGate with openCyto upon
#' loading package...
#'
#' @noRd
.onLoad <- function(libname,pkgname){

  options("CytoRSuite_interact" = interactive())
  openCyto::registerPlugins(fun = gate_draw, methodName = "drawGate")
  openCyto::registerPlugins(fun = gate_manual, methodName = "manualGate")
  openCyto::registerPlugins(fun = ppmanualGate, methodName = 'ppmanualGate', dep=NA, "preprocessing")
  
}
