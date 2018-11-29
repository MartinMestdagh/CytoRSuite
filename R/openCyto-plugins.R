#' drawGate Plugin for openCyto
#'
#' \code{drawGate} allows the user to draw polygon gates directly onto plots of
#' flow cytometry data. Simply left click to gate and right click to close the
#' gate.
#'
#' @param fr a \code{\link[flowCore:flowFrame-class]{flowFrame}} object
#'   containing the flow cytometry data for gating.
#' @param pp_res output of preprocessing function.
#' @param channels name(s) of the channel(s) to be used for plotting.
#' @param alias name of the population to be gated. This is not inherited from
#'   the gatingTemplate and must be supplied manually to the gating_args.
#' @param ... additional arguments passsed to \code{\link{plotCyto}}.
#'
#' @return a \code{polygonGate} constructed from coordinates supplied by
#'   \code{DrawGate}.
#'
#' @keywords manual, gating, polygon, polygonGate
#'
#' @importFrom openCyto registerPlugins
#'
#' @seealso \code{\link{plotCyto1d,flowFrame-method}}
#' @seealso \code{\link{plotCyto1d,flowSet-method}}
#'
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#'
#' @examples
#' \dontrun{
#' fs <- Activation # load in .fcs files
#'
#' gs <- GatingSet(fs) # add flowSet to GatingSet
#'
#' template <- add_pop(
#' gs, alias = "Lymphocytes", pop = "+", parent = "root",
#' dims = "FSC-A,SSC-A", gating_method = "drawGate",
#' gating_args = "subSample=1000,alias='Lymphocytes',type='ellipse'",
#' collapseDataForGating = TRUE, groupBy = 2
#' )
#'
#' # gating window will open to construct gate left click vertices on plot
#' # and close gate by right click and selecting "stop".
#' plotCyto(gs[[1]],
#'          parent = "root",
#'          alias = "Lymphocytes",
#'          channels = c("FSC-A","SSC-A"))
#' }
#'
#' @export
gate_draw <- function(fr, pp_res, channels, alias, ...){
  
  # Determine vertices of polygon using DrawGate
  gates <- drawGate(x = fr, channels = channels, alias = alias, ...)
  
  return(gates)
  
}
registerPlugins(gate_draw, "drawGate")

#' Apply Stored Gates to GatingSet
#'
#' \code{drawGate} allows the user to draw gates directly onto plots of flow
#' cytometry data. This plugin provides a way of passing manually drawn gates to
#' \code{openCyto}.
#'
#' @param fr a \code{\link[flowCore:flowFrame-class]{flowFrame}} object
#'   containing the flow cytometry data for gating.
#' @param pp_res output of preprocessing function.
#' @param channels fluorescent channel(s) to use for gating.
#' @param gate stored \code{drawGate} gate in csv file for passing to openCyto.
#'
#' @return pass saved gate to openCyto to apply to all samples.
#'
#' @keywords manual, gating, polygon, polygonGate
#'
#' @importFrom openCyto registerPlugins
#'
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#'
#' @export
gate_manual <- function(fr, pp_res, channels, gate){
  
  # Index of gate to use
  if(is.numeric(pp_res)){
    
    gt <- pp_res
    
  }else if(is.character(pp_res)){
    
    # names of gates must contain merged groupBy info
    gt <- match(pp_res, names(gate))
    
  }
  
  return(gate[[gt]])
  
}
registerPlugins(gate_manual, "manualGate")

#' drawGate Preprocessing Function
#'
#' This preprocessing function passes on the index of the gate or merged groupBy
#' info to apply to the samples.
#'
#' @param fs a \code{\link[flowCore:flowSet-class]{flowSet}} object containing
#'   samples in group for gating.
#' @param gs a \code{\link[flowWorkspace:GatingSet-class]{GatingSet}} containing
#'   all the samples to be gated.
#' @param gm gating method to use for gating, not used.
#' @param channels, names of the channels used to construct the gate, not used.
#' @param groupBy grouping variable to partition data, can either a single
#'   numeric or vector of pData column names.
#' @param isCollapse logical indicating when the data should be collapsed prior
#'   to gating, not used.
#' @param ... additional arguments (not used).
#'
#' @return index of save drawGate to apply to sample or merged groupBy information.
#'
#' @importFrom openCyto registerPlugins
#' @importFrom flowWorkspace pData
#'
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#'
#' @export
ppmanualGate <- function(fs, gs, gm, channels=NA, groupBy=NA, isCollapse=NA, ...) {
  
  # Samples
  smp <- length(gs)
  
  # Extract pData information
  pd <- pData(gs)
  
  # Add groupBy info to pData gs
  if(!is.na(groupBy) & grepl("^[A-Za-z]+$", groupBy) == TRUE){
    
    # groupBy is composed of characters
    pd$groupby <- do.call(paste, pd[, groupBy, drop = FALSE])
    
    grpby <- pd[,"groupby"][match(pData(fs[1])[, "name"], pd[, "name"])]
    
  }else if(!is.na(groupBy) & grepl("^[A-Za-z]+$", groupBy) == FALSE){
    
    if(groupBy == smp){
      
      grpby <- 1
      
    }else{
      
      message("Numeric groupBy is not supported, use pData variables instead. All samples will be grouped together.")
      grpby <- 1
      
    }
  }
  
  # No grouping select first gate - only 1 gate expected
  if(is.na(groupBy)){
    
    grpby <- 1
    
  }
  
  return(grpby)
  
}
registerPlugins(fun = ppmanualGate, methodName = 'ppmanualGate', dep=NA, "preprocessing")
