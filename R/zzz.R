#' Automatically register ppdrawGate, drawGate and manualGate with openCyto upon
#' loading package...
#'
#' @noRd
.onLoad <- function(libname,pkgname){

  openCyto::registerPlugins(fun = gate_draw, methodName = "drawGate")
  openCyto::registerPlugins(fun = gate_manual, methodName = "manualGate")
  openCyto::registerPlugins(fun = ppdrawGate, methodName = 'ppdrawGate', dep=NA, "preprocessing")
  
}
