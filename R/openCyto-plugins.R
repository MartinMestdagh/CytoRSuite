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
#' @param subSample number of events to plot.
#' @param plot logical indicating whether a plot should be constructed.
#' @param transList object of class
#'   \code{\link[flowCore:transformList-class]{transformerList}} generated by
#'   \code{estimateLogicle} to transform fluorescent channels for gating.
#'   \code{transList} is required if logicle transformation has already been
#'   applied to \code{x} using estimateLogicle. \code{computeSpillover} will
#'   automatically call \code{\link[flowCore:logicleTransform]{estimateLogicle}}
#'   internally to transform channels prior to gating, if \code{transList} is
#'   supplied it will be used for the transformation instead.
#' @param gate_range range in which gate should be constructed (only needed for
#'   autogating functions).
#' @param type type of gate to be constructed, supported types include
#'   \code{c("polygon", "rectangle", "interval", "threshold", "ellipse",
#'   "quadrant")}.
#' @param min argument passed to \code{truncate_flowFrame} to restrict data to
#'   values > \code{min}.
#' @param max argument passed to \code{truncate_flowFrame} to restrict data to
#'   values < \code{max}.
#' @param alias used within gating_args to supply the names of the populations
#'   to drawGate (e.g. gating_args = alias='Lymphocytes').
#' @param axis indicates the axis to use for gating for \code{type="interval"}
#'   when 2 fluorescent channel are supplied.
#' @param adjust numeric smoothing factor used for 1D density plots.
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
gate_draw <- function(fr, pp_res, channels, alias, subSample, gate_range = NULL, min = NULL, max = NULL, type = "polygon", axis = "x", adjust = 1.5, plot = TRUE, transList = NULL, ...){
  
  # Two fluorescent channels must be supplied
  if(missing(channels) | !length(channels) %in% c(1,2)){
    
    stop("Two fluorescent channels must be specified to draw polygon gate.")
    
  }
  
  # Truncate flowFrame if min and max arguments are supplied
  if(!(is.null(min) && is.null(max))){
    
    fr <- .truncate_flowframe(fr, channels = channels, min = min, max = max)
    
  }
  
  # Determine vertices of polygon using DrawGate
  gates <- drawGate(x = fr, channels = channels, alias = alias, type = type, transList = transList, ...)
  
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
  
  return(gate)
  
}
registerPlugins(gate_manual, "manualGate")