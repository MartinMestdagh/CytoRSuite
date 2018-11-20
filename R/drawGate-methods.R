#' drawGate
#'
#' Manually draw gates around populations for analysis of flow cytometry data.
#'
#' \code{drawGate} is a convenient wrapper for the gating functions shipped with
#' \code{cytoRSuite} to facilitate analysis of flow cytometry by gate drawing.
#' Using \code{drawGate} users can specify the type of gate(s) to be constructed
#' through the \code{type} argument and \code{drawGate} will automatically
#' handle plotting the data and make calls to the relevant gating function(s) to
#' construct the gates around populations of interest. \code{drawGate} has
#' methods for \code{\link[flowCore:flowFrame-class]{flowFrame}},
#' \code{\link[flowCore:flowSet-class]{flowSet}} and
#' \code{\link[flowWorkspace:GatingSet-class]{GatingSet}} objects, refer to
#' their respective help pages for more information. The flowFrame and flowSet
#' methods simply return the constructed gates as a list of
#' \code{\link[flowCore:filters-class]{filters}}, whilst the GatingSet method
#' automatically applies the constructed gates to the GatingSet and saves the
#' constructed gates in an \code{openCyto}
#' \code{\link[openCyto:gatingTemplate-class]{gatingTemplate}}for future use.
#' See \code{\link{editGate}} and \code{\link{removeGate}} to manipulate
#' constructed gates and modify their entries in the gatingTemplate.
#'
#' @param x object of class \code{\link[flowCore:flowFrame-class]{flowFrame}}, \code{\link[flowCore:flowSet-class]{flowSet}} or
#'   \code{\link[flowWorkspace:GatingSet-class]{GatingSet}}.
#' @param ... additional method-specific arguments.
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @seealso \code{\link{drawGate,flowFrame-method}}
#' @seealso \code{\link{drawGate,flowSet-method}}
#' @seealso \code{\link{drawGate,GatingSet-method}}
#'
#' @export
setGeneric(
  name = "drawGate",
  def = function(x, ...) {
    standardGeneric("drawGate")
  }
)

#' drawGate flowFrame Method.
#'
#' Manually draw gates around populations for analysis of flow cytometry data.
#'
#' @param x object of class \code{\link[flowCore:flowFrame-class]{flowFrame}}.
#' @param channels vector of channel names to use for plotting, can be of length
#'   1 for 1-D density histogram or length 2 for 2-D scatter plot.
#' @param alias the name(s) of the populations to be gated. If multiple
#'   population names are supplied (e.g. \code{c("CD3,"CD4)}) multiple gates
#'   will be returned. \code{alias} is \code{NULL} by default which will halt
#'   the gating routine.
#' @param type vector of gate type names used to construct the gates. Multiple
#'   gate types are supported but should be accompanied with an \code{alias}
#'   argument of the same length (i.e. one \code{type} per \code{alias}).
#'   Supported gate types include \code{polygon, rectangle, ellipse, threshold,
#'   boundary, interval, quadrant and web} which can be abbreviated as upper or
#'   lower case first letters as well. Default \code{type} is \code{"interval"}
#'   for 1D gates and \code{"polygon"} for 2D gates.
#' @param subSample  numeric indicating the number of events to plot, set to all
#'   events by default. Reducing the sample size can significantly increase
#'   plotting speed on less powerful machines.
#' @param axis indicates whether the \code{"x"} or \code{"y"} axis should be
#'   gated for 2-D interval gates.
#' @param labels logical indicating whether to include \code{\link{plotLabels}}
#'   for the gated population(s), \code{TRUE} by default.
#' @param adjust smooothing factor passed to
#'   \code{\link[stats:density]{density}} for 1-D plots (defaults to 1.5).
#' @param plot logical indicating whether a plot should be drawn, set to
#'   \code{TRUE} by default.
#' @param ... additional arguments for \code{\link{plotCyto,flowFrame-method}}.
#'
#' @return a \code{\link[flowCore:filters-class]{filters}} list containing the
#'   drawn gate objects.
#'
#' @importFrom BiocGenerics colnames
#' @importFrom flowCore filters
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @seealso \code{\link{plotCyto1d,flowFrame-method}}
#' @seealso \code{\link{plotCyto2d,flowFrame-method}}
#' @seealso \code{\link{drawGate,flowSet-method}}
#' @seealso \code{\link{drawGate,GatingSet-method}}
#'
#' @export
setMethod(drawGate, signature = "flowFrame", definition = function(x, channels = NULL, alias = NULL, subSample = 250000, type = "polygon", axis = "x", adjust = 1.5, labels = TRUE, plot = TRUE, ...) {

  # Assign x to fr
  fr <- x

  # Check type argument is valid
  type <- checkGateType(type = type, alias = alias)

  # Set default type for 1D gates to interval
  if (length(channels) == 1 & all(type %in% "polygon")) {
    type <- rep("interval", length(type))
  }

  # Check alias is supplied correctly
  checkAlias(alias = alias, type = type)

  # Check supplied channel(s) are valid
  channels <- checkChannels(fr, channels = channels, plot = TRUE)

  # Make one call to drawPlot
  if (plot == TRUE) {
    if(getOption("cytoRSuite_interact") == FALSE) {
      plotCyto(fr, channels = channels, subSample = subSample, popup = FALSE, legend = FALSE, ...)
    }else{
      plotCyto(fr, channels = channels, subSample = subSample, popup = TRUE, legend = FALSE, ...)
    }
  }

  # Construct gates save as filters object
  if (length(type) == 1 & type[1] == "quadrant") {
    gates <- drawQuadrants(fr = fr, channels = channels, alias = alias, subSample = subSample, plot = FALSE, labels = labels, ...)
  } else if (length(type) == 1 & type[1] == "web") {
    gates <- drawWeb(fr = fr, channels = channels, alias = alias, subSample = subSample, plot = FALSE, labels = labels, ...)
  } else {
    gates <- mapply(function(type, alias) {
      if (type == "polygon") {
        drawPolygon(fr = fr, channels = channels, alias = alias, subSample = subSample, plot = FALSE, labels = labels, ...)
      } else if (type == "rectangle") {
        drawRectangle(fr = fr, channels = channels, alias = alias, subSample = subSample, plot = FALSE, labels = labels, ...)
      } else if (type == "interval") {
        drawInterval(fr = fr, channels = channels, alias = alias, subSample = subSample, plot = FALSE, axis = axis, labels = labels, ...)
      } else if (type == "threshold") {
        drawThreshold(fr = fr, channels = channels, alias = alias, subSample = subSample, plot = FALSE, labels = labels, ...)
      } else if (type == "boundary") {
        drawBoundary(fr = fr, channels = channels, alias = alias, subSample = subSample, plot = FALSE, labels = labels, ...)
      } else if (type == "ellipse") {
        drawEllipse(fr = fr, channels = channels, alias = alias, subSample = subSample, plot = FALSE, labels = labels, ...)
      }
    }, type, alias)
  }

  gates <- filters(gates)

  return(gates)
})

#' drawGate flowSet Method
#'
#' Manually draw gates around populations for analysis of flow cytometry data.
#'
#' @param x object of class \code{\link[flowCore:flowSet-class]{flowSet}}.
#' @param select vector containing the indicies of samples within gs to use for
#'   plotting.
#' @param channels vector of channel names to use for plotting, can be of length
#'   1 for 1-D density histogram or length 2 for 2-D scatter plot.
#' @param alias the name(s) of the populations to be gated. If multiple
#'   population names are supplied (e.g. \code{c("CD3,"CD4)}) multiple gates
#'   will be returned. \code{alias} is \code{NULL} by default which will halt
#'   the gating routine.
#' @param type vector of gate type names used to construct the gates. Multiple
#'   gate types are supported but should be accompanied with an \code{alias}
#'   argument of the same length (i.e. one \code{type} per \code{alias}).
#'   Supported gate types are \code{polygon, rectangle, ellipse, threshold,
#'   boundary, interval, quadrant and web} which can be abbreviated as upper or
#'   lower case first letters as well. Default \code{type} is \code{"interval"}
#'   for 1D gates and \code{"polygon"} for 2D gates.
#' @param subSample  numeric indicating the number of events to plot, set to all
#'   events by default. Reducing the sample size can significantly increase
#'   plotting speed on less powerful machines.
#' @param axis indicates whether the \code{"x"} or \code{"y"} axis should be
#'   gated for 2-D interval gates.
#' @param labels logical indicating whether to include \code{\link{plotLabels}}
#'   for the gated population(s), \code{TRUE} by default.
#' @param adjust smooothing factor passed to
#'   \code{\link[stats:density]{density}} for 1-D plots (defaults to 1.5).
#' @param plot logical indicating whether a plot should be drawn, set to
#'   \code{TRUE} by default.
#' @param ... additional arguments for \code{\link{plotCyto,flowSet-method}}.
#'
#' @return a \code{\link[flowCore:filters-class]{filters}} list containing the
#'   drawn gate objects.
#'
#' @importFrom BiocGenerics colnames
#' @importFrom flowCore filters
#' @importFrom methods as
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @seealso \code{\link{plotCyto1d,flowSet-method}}
#' @seealso \code{\link{plotCyto2d,flowSet-method}}
#' @seealso \code{\link{drawGate,flowFrame-method}}
#' @seealso \code{\link{drawGate,GatingSet-method}}
#'
#' @export
setMethod(drawGate, signature = "flowSet", definition = function(x, select = NULL, channels = NULL, alias = NULL, subSample = 250000, type = "polygon", axis = "x", adjust = 1.5, labels = TRUE, plot = TRUE, ...) {

  # Assign x to fs
  fs <- x

  # Restrict to samples matching pData requirements
  if (!is.null(select)) {
    if (class(select) != "numeric") {
      stop("Vector supplied to select argument should contain the numeric indicies of the samples to select.")
    }

    # Extract samples using selectFrames
    fs <- fs[select]
  }
  fr <- as(fs, "flowFrame")

  # Check type argument is valid
  type <- checkGateType(type = type, alias = alias)

  # Set default type for 1D gates to interval
  if (length(channels) == 1 & all(type %in% "polygon")) {
    type <- rep("interval", length(type))
  }

  # Check alias is supplied correctly
  checkAlias(alias = alias, type = type)

  # Check supplied channel(s) are valid
  channels <- checkChannels(fr, channels = channels, plot = TRUE)

  # Make one call to drawPlot
  if (plot == TRUE) {
    if(getOption("cytoRSuite_interact") == FALSE){
      plotCyto(fs, channels = channels, subSample = subSample, popup = FALSE, legend = FALSE, merge = TRUE, ...)
    }else {
      plotCyto(fs, channels = channels, subSample = subSample, popup = TRUE, legend = FALSE, merge = TRUE, ...)
    }
  }

  # Construct gates save as filters object
  if (length(type) == 1 & type[1] == "quadrant") {
    gates <- drawQuadrants(fr = fr, channels = channels, alias = alias, subSample = subSample, plot = FALSE, labels = labels, ...)
  } else if (length(type) == 1 & type[1] == "web") {
    gates <- drawWeb(fr = fr, channels = channels, alias = alias, subSample = subSample, plot = FALSE, labels = labels, ...)
  } else {
    gates <- mapply(function(type, alias) {
      if (type == "polygon") {
        drawPolygon(fr = fr, channels = channels, alias = alias, subSample = subSample, plot = FALSE, labels = labels, ...)
      } else if (type == "rectangle") {
        drawRectangle(fr = fr, channels = channels, alias = alias, subSample = subSample, plot = FALSE, labels = labels, ...)
      } else if (type == "interval") {
        drawInterval(fr = fr, channels = channels, alias = alias, subSample = subSample, plot = FALSE, axis = axis, labels = labels, ...)
      } else if (type == "threshold") {
        drawThreshold(fr = fr, channels = channels, alias = alias, subSample = subSample, plot = FALSE, labels = labels, ...)
      } else if (type == "boundary") {
        drawBoundary(fr = fr, channels = channels, alias = alias, subSample = subSample, plot = FALSE, labels = labels, ...)
      } else if (type == "ellipse") {
        drawEllipse(fr = fr, channels = channels, alias = alias, subSample = subSample, plot = FALSE, labels = labels, ...)
      }
    }, type, alias)
  }

  gates <- filters(gates)
  return(gates)
})

#' drawGate GatingSet Method
#'
#' Manually draw gates around populations for analysis of flow cytometry data.
#'
#' @param x object of class
#'   \code{\link[flowWorkspace:GatingSet-class]{GatingSet}}.
#' @param groupBy vector of pData column names (e.g.
#'   c("Treatment","Concentration") indicating how the samples should be grouped
#'   prior to gating, set to the length of x by default to construct a single
#'   gate for all samples. If groupBy is suppied a different gate will be
#'   constructed for each group.
#' @param select vector containing the indicies of samples within each group to
#'   use for plotting.
#' @param channels vector of channel names to use for plotting, can be of length
#'   1 for 1-D density histogram or length 2 for 2-D scatter plot.
#' @param parent name of the \code{parent} population to extract for gating.
#' @param alias the name(s) of the populations to be gated. If multiple
#'   population names are supplied (e.g. \code{c("CD3,"CD4)}) multiple gates
#'   will be returned. \code{alias} is \code{NULL} by default which will halt
#'   the gating routine.
#' @param type vector of gate type names used to construct the gates. Multiple
#'   gate types are supported but should be accompanied with an \code{alias}
#'   argument of the same length (i.e. one \code{type} per \code{alias}).
#'   Supported gate types are \code{polygon, rectangle, ellipse, threshold,
#'   boundary, interval, quadrant and web} which can be abbreviated as upper or
#'   lower case first letters as well. Default \code{type} is \code{"interval"}
#'   for 1D gates and \code{"polygon"} for 2D gates.
#' @param gtfile name of \code{gatingTemplate} csv file to be saved.
#' @param subSample  numeric indicating the number of events to plot, set to all
#'   events by default. Reducing the sample size can significantly increase
#'   plotting speed on less powerful machines.
#' @param axis indicates whether the \code{"x"} or \code{"y"} axis should be
#'   gated for 2-D interval gates.
#' @param labels logical indicating whether to include \code{\link{plotLabels}}
#'   for the gated population(s), \code{TRUE} by default.
#' @param adjust smooothing factor passed to
#'   \code{\link[stats:density]{density}} for 1-D plots (defaults to 1.5).
#' @param plot logical indicating whether a plot should be drawn, set to
#'   \code{TRUE} by default.
#' @param ... additional arguments for \code{\link{plotCyto,GatingSet-method}}.
#'
#' @return drawn gates are applied to the
#'   \code{\link[flowWorkspace:GatingSet-class]{GatingSet}} and saved to a
#'   \code{\link[openCyto:gatingTemplate-class]{gatingTemplate}}.
#'
#' @importFrom BiocGenerics colnames
#' @importFrom flowWorkspace getData
#' @importFrom openCyto add_pop
#' @importFrom methods as
#' @importFrom utils read.csv write.csv
#' @importFrom flowCore filters
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @seealso \code{\link{plotCyto,GatingSet-method}}
#' @seealso \code{\link{plotCyto1d,flowFrame-method}}
#' @seealso \code{\link{plotCyto2d,flowFrame-method}}
#' @seealso \code{\link{drawGate,flowFrame-method}}
#' @seealso \code{\link{drawGate,flowSet-method}}
#'
#' @export
setMethod(drawGate, signature = "GatingSet", definition = function(x, groupBy = length(x), select = NULL, parent = "root", alias = NULL, channels = NULL, type = "polygon", subSample = 250000, gtfile = NULL, axis = "x", adjust = 1.5, labels = TRUE, plot = TRUE, ...) {

  # Assign x to gs
  gs <- x
  smp <- length(gs)

  # Extract pData information
  pd <- pData(gs)
  
  # Check whether a gatingTemplate ready exists for this population
  if (!is.null(gtfile)) {

    # Check whether gate already exists in gtfile
    checkTemplate(parent, alias, gtfile)
  }

  fs <- flowWorkspace::getData(x, parent)

  # grouping required
  if(is.numeric(groupBy)){
    
    if(length(groupBy) > 1){
      
      stop("Only a single numeric can be supplied to groupBy.")
      
    }else if(groupBy > smp){
      
      groupBy <- smp
      
    }
    
    if(groupBy == smp){
      
      grps <- list(fs)
      
    }else{
      
      stop("Numeric groupBy is not currently supported - use pData variables instead.")
      
    }
    
  }else if(is.character(groupBy)){
    
    if(!all(groupBy %in% colnames(pd))){
      
      stop("Names supplied to groupBy do not exist in pData(x).")
      
    }
    
    pd$groupby <- do.call(paste, pd[, groupBy, drop = FALSE])
    grps <- lapply(unique(pd$groupby), function(x){
      
      fs[which(pd$groupby %in% x)]
      
    })
    
  }
  
  # Gate each group - list of filters
  fltrsLst <- lapply(grps, function(fs){
  
    # Restrict to samples matching pData requirements
    if (!is.null(select)) {
      if (class(select) != "numeric") {
        stop("Vector supplied to select argument should contain the numeric indicies of the samples to select.")
      }

      # Extract samples using selectFrames
      fs <- fs[select]
    }
    fr <- as(fs, "flowFrame")
    
    # Remove "Original" column introduced by coercion
    if(is.na(match("Original", BiocGenerics::colnames(fr))) == FALSE){
      
      fr <- suppressWarnings(fr[, -match("Original", BiocGenerics::colnames(fr))])
      
    }

    # Check type argument is valid
    type <- checkGateType(type = type, alias = alias)

    # Set default type for 1D gates to interval
    if (length(channels) == 1 & all(type %in% "polygon")) {
      type <- rep("interval", length(type))
    }

    # Check alias is supplied correctly
    checkAlias(alias = alias, type = type)

    # Check supplied channel(s) are valid
    channels <- checkChannels(fr, channels = channels, plot = TRUE)

    # Main
    if(is.numeric(groupBy)){
      
      if(parent == "root"){
        
        pnt <- "All Events"
        
      }else{
        
        pnt <- parent
        
      }
      
      if(groupBy == length(gs)){
        
        main <- paste("Combined Events","\n", pnt)
        
      }
      
    }else if(is.character(groupBy)){
      
      if(parent == "root"){
        
        pnt <- "All Events"
        
      }else{
        
        pnt <- parent
        
      }
      
      main <- paste(pd[pd$name %in% sampleNames(fs), "groupby"][1], "\n", pnt)
      
    }
  
    # Make one call to drawPlot
    if (plot == TRUE) {
      if(getOption("cytoRSuite_interact") == FALSE){
        plotCyto(x = gs[sampleNames(fs)], parent = parent, channels = channels, subSample = subSample, popup = FALSE, legend = FALSE, merge = TRUE, main = main, ...)
      }else{
        plotCyto(x = gs[sampleNames(fs)], parent = parent, channels = channels, subSample = subSample, popup = TRUE, legend = FALSE, merge = TRUE, main = main, ...)
      }
    }

    # Construct gates save as filters object
    if (length(type) == 1 & type[1] == "quadrant") {
      gates <- drawQuadrants(fr = fr, channels = channels, alias = alias, subSample = subSample, plot = FALSE, labels = labels, ...)
    } else if (length(type) == 1 & type[1] == "web") {
      gates <- drawWeb(fr = fr, channels = channels, alias = alias, subSample = subSample, plot = FALSE, labels = labels, ...)
    } else {
      gates <- mapply(function(type, alias) {
        if (type == "polygon") {
          drawPolygon(fr = fr, channels = channels, alias = alias, subSample = subSample, plot = FALSE, labels = labels, ...)
        } else if (type == "rectangle") {
          drawRectangle(fr = fr, channels = channels, alias = alias, subSample = subSample, plot = FALSE, labels = labels, ...)
        } else if (type == "interval") {
          drawInterval(fr = fr, channels = channels, alias = alias, subSample = subSample, plot = FALSE, axis = axis, labels = labels, ...)
        } else if (type == "threshold") {
          drawThreshold(fr = fr, channels = channels, alias = alias, subSample = subSample, plot = FALSE, labels = labels, ...)
        } else if (type == "boundary") {
          drawBoundary(fr = fr, channels = channels, alias = alias, subSample = subSample, plot = FALSE, labels = labels, ...)
        } else if (type == "ellipse") {
          drawEllipse(fr = fr, channels = channels, alias = alias, subSample = subSample, plot = FALSE, labels = labels, ...)
        }
      }, type, alias)
    }

    gates <- filters(gates)

  })
  
  # Name gates with groupBy info
  if(is.numeric(groupBy)){
  
    # group number
    names(fltrsLst) <- unique(pd$groupby)
    
  }else if(is.character(groupBy)){
    
    # merge columns
    names(fltrsLst) <- unique(do.call(paste, pd[, groupBy, drop = FALSE]))
    
  }
  gates <- fltrsLst
  
  # format gates to be a list of alias lists each of length group appropriately named
  gates <- lapply(1:length(alias), function(y){
    gates <- lapply(gates, function(x){
      
        gts <- filters(list(x[[y]]))
      
    })
    names(gates) <- unique(pd$groupby)
    return(gates)
  })
  
  # Prepare gatingTemplate entries
  pop <- "+"
  
  # Prepare groupBy
  if(is.character(groupBy)){
    
    groupBy <- paste(groupBy, collapse = ",")
    
  }
  
  # Use add_pop to apply gates to GatingSet and construct gatingTemplate
  if (is.null(gtfile)) {
    message("No gatingTemplate file name supplied - creating gatingTemplate.csv to store gates.")

    # need to extract alias from gates list into new named list
    pops <- list()
    for (i in 1:length(alias)) {
      pops[[i]] <- add_pop(
        gs = x, alias = alias[i], parent = parent, pop = pop, dims = paste(channels, collapse = ","), gating_method = "manualGate",
        gating_args = list(gate = gates[[i]]), groupBy = groupBy, collapseDataForGating = TRUE,
        preprocessing_method = "ppmanualGate"
      )
    }
    pops <- do.call("rbind", pops)

    write.csv(pops, "gatingTemplate.csv", row.names = FALSE)
  } else if (checkFile(gtfile) == FALSE) {
    message(paste("Supplied gtfile does not exist in working directory - writing", paste(gtfile), "."))

    pops <- list()
    for (i in 1:length(alias)) {
      pops[[i]] <- add_pop(
        gs = x, alias = alias[i], parent = parent, pop = pop, dims = paste(channels, collapse = ","), gating_method = "manualGate",
        gating_args = list(gate = gates[[i]]), groupBy = groupBy, collapseDataForGating = TRUE, 
        preprocessing_method = "ppmanualGate"
      )
    }
    pops <- do.call("rbind", pops)

    write.csv(pops, gtfile, row.names = FALSE)
  } else if (checkFile(gtfile) == TRUE) {
    gt <- read.csv(gtfile, header = TRUE)

    pops <- list()
    for (i in 1:length(alias)) {
      pops[[i]] <- add_pop(
        gs = x, alias = alias[i], parent = parent, pop = pop, dims = paste(channels, collapse = ","), gating_method = "manualGate",
        gating_args = list(gate = gates[[i]]), groupBy = groupBy, collapseDataForGating = TRUE,
        preprocessing_method = "ppmanualGate"
      )
    }
    pops <- do.call("rbind", pops)
    gt <- rbind(gt, pops)

    write.csv(gt, gtfile, row.names = FALSE)
  }

  invisible(pops)
})
