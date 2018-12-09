#' Check Supplied Channels
#'
#' \code{checkChannels} will check whether the supplied channels are valid for
#' the \code{\link[flowCore:flowFrame-class]{flowFrame}},
#' \code{\link[flowCore:flowSet-class]{flowset}} or
#' \code{\link[flowWorkspace:GatingSet-class]{GatingSet}} . \code{checkChannels}
#' will also return the channels if the marker names are supplied.
#'
#' @param x an object of class
#'   \code{\link[flowCore:flowFrame-class]{flowFrame}}.
#' @param channels vector of channel names (e.g. c("PE-A","APC-A")) or marker
#'   names.
#' @param plot logical indicating whether the channels will be used to construct
#'   a plot, set to TRUE by default to accept 1 or 2 channels only.
#'
#' @return vector of valid channel names.
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @export
setGeneric(name = "checkChannels",
def = function(x, channels, plot){standardGeneric("checkChannels")}
)

#' Check Supplied Channels - flowFrame Method
#'
#' \code{checkChannels} will check whether the supplied channels are valid for
#' the supplied \code{\link[flowCore:flowFrame-class]{flowFrame}}.
#'
#' @param x an object of class
#'   \code{\link[flowCore:flowFrame-class]{flowFrame}}.
#' @param channels vector of channel names (e.g. c("PE-A","APC-A")) or marker
#'   names.
#' @param plot logical indicating whether the channels will be used to construct
#'   a plot, set to TRUE by default to accept 1 or 2 channels.
#'
#' @return vector of valid channel names.
#'
#' @importFrom flowWorkspace pData
#' @importFrom flowCore parameters
#' 
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @examples 
#' \dontrun{
#' fs <- Activation
#' chnls <- c( "PE-A", "Alexa Fluor 488-A", "Alexa Fluor 700-A", "Alexa Fluor 647-A", "7-AAD-A") 
#' markers <- c("Va2", "CD8", "CD4", "CD44", "CD69")
#' names(markers) <- chnls
#' markernames(fs) <- markers
#' 
#' checkChannels(fs[[1]], channels = c("CD4","CD8"), plot = TRUE)
#' checkChannels(fs[[1]], channels = c("CD4","CD8","CD44","CD69"), plot = TRUE)
#' }
#'
#' @export
setMethod(checkChannels, signature = "flowFrame", definition = function(x, channels, plot = TRUE){
 
  # Incorrect channels length
  if(plot == TRUE){
    
    if(!length(channels) %in% c(1,2)){
    
      stop("Invalid number of supplied channels.")
    
    }
    
  }
  
  chans <- BiocGenerics::colnames(x)
  fr.data <- pData(parameters(x))
  
  # Channel Indices supplied
  if(is.numeric(channels)){
    
    channels <- chans[channels]
    
  }
  
  # Check if channels match colnames of flowFrame
  if(all(channels %in% chans)){
    
    # Supplied channels are valid
    
  }else if(!all(channels %in% chans)){
    
    lapply(channels, function(channel){
      
      if(channel %in% chans){
        
        
      }else if(channel %in% fr.data$desc){
        
        channels[channels %in% channel] <<- as.character(fr.data$name[match(channel, fr.data$desc)])
     
      }else if(!channel %in% chans & !channel %in% fr.data$desc){
        
        stop(paste(channel,"is not a valid channel for this flowFrame."))
        
      }
      
    })
    
  }
  
  return(channels)
  
}) 
  
#' Check Supplied Channels - flowSet Method
#'
#' \code{checkChannels} will check whether the supplied channels are valid for
#' the supplied \code{\link[flowCore:flowSet-class]{flowSet}}.
#'
#' @param x an object of class \code{\link[flowCore:flowSet-class]{flowSet}}.
#' @param channels vector of channel names (e.g. c("PE-A","APC-A")) or marker
#'   names.
#' @param plot logical indicating whether the channels will be used to construct
#'   a plot, set to TRUE by default to accept 1 or 2 channels.
#'
#' @return vector of valid channel names.
#'
#' @importFrom flowWorkspace pData
#' @importFrom flowCore parameters
#' 
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @examples 
#' \dontrun{
#' fs <- Activation
#' chnls <- c( "PE-A", "Alexa Fluor 488-A", "Alexa Fluor 700-A", "Alexa Fluor 647-A", "7-AAD-A") 
#' markers <- c( "Va2", "CD8", "CD4", "CD44", "CD69")
#' names(markers) <- chnls
#' markernames(fs) <- markers
#' 
#' checkChannels(fs, channels = c("CD4","CD8"), plot = TRUE)
#' checkChannels(fs, channels = c("CD4","CD8","CD44","CD69"), plot = TRUE)
#' }
#'
#' @export
setMethod(checkChannels, signature = "flowSet", definition = function(x, channels, plot = TRUE){
  
  # Incorrect channels length
  if(plot == TRUE){
    
    if(!length(channels) %in% c(1,2)){
      
      stop("Invalid number of supplied channels.")
      
    }
    
  }
  
  # Assign x to fs
  fs <- x
  
  chans <- BiocGenerics::colnames(fs[[1]])
  fr.data <- pData(parameters(fs[[1]]))
  
  # Channel Indices supplied
  if(is.numeric(channels)){
    
    channels <- chans[channels]
    
  }
  
  # Check if channels match colnames of flowFrame
  if(all(channels %in% chans)){
    
    # Supplied channels are valid
    
  }else if(!all(channels %in% chans)){
    
    lapply(channels, function(channel){
      
      if(channel %in% chans){
        
        
      }else if(channel %in% fr.data$desc){
        
        channels[channels %in% channel] <<- as.character(fr.data$name[match(channel, fr.data$desc)])
        
      }else if(!channel %in% chans & !channel %in% fr.data$desc){
        
        stop(paste(channel,"is not a valid channel for this flowFrame."))
        
      }
      
    })
    
  }
  
  return(channels)
  
}) 

#' Check Supplied Channels - GatingSet Method
#'
#' \code{checkChannels} will check whether the supplied channels are valid for
#' the supplied \code{\link[flowWorkspace:GatingSet-class]{GatingSet}}.
#'
#' @param x an object of class
#'   \code{\link[flowWorkspace:GatingSet-class]{GatingSet}}.
#' @param channels vector of channel names (e.g. c("PE-A","APC-A")) or marker
#'   names.
#' @param plot logical indicating whether the channels will be used to construct
#'   a plot, set to TRUE by default to accept 1 or 2 channels.
#'
#' @return vector of valid channel names.
#'
#' @importFrom flowWorkspace pData getData
#' @importFrom flowCore parameters
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @examples 
#' \dontrun{
#' fs <- Activation
#' chnls <- c( "PE-A", "Alexa Fluor 488-A", "Alexa Fluor 700-A", "Alexa Fluor 647-A", "7-AAD-A") 
#' markers <- c( "Va2", "CD8", "CD4", "CD44", "CD69")
#' names(markers) <- chnls
#' markernames(fs) <- markers
#' gs <- GatingSet(fs)
#' 
#' checkChannels(fs, channels = c("CD4","CD8"), plot = TRUE)
#' checkChannels(fs, channels = c("CD4","CD8","CD44","CD69"), plot = TRUE)
#' }
#'
#' @export
setMethod(checkChannels, signature = "GatingSet", definition = function(x, channels, plot = TRUE){
  
  # Incorrect channels length
  if(plot == TRUE){
    
    if(!length(channels) %in% c(1,2)){
      
      stop("Invalid number of supplied channels.")
      
    }
    
  }
  
  # Assign x to gs
  gs <- x
  fs <- getData(gs, "root")
  
  chans <- BiocGenerics::colnames(fs[[1]])
  fr.data <- pData(parameters(fs[[1]]))
  
  # Channel Indices supplied
  if(is.numeric(channels)){
    
    channels <- chans[channels]
    
  }
  
  # Check if channels match colnames of flowFrame
  if(all(channels %in% chans)){
    
    # Supplied channels are valid
    
  }else if(!all(channels %in% chans)){
    
    lapply(channels, function(channel){
      
      if(channel %in% chans){
        
        
      }else if(channel %in% fr.data$desc){
        
        channels[channels %in% channel] <<- as.character(fr.data$name[match(channel, fr.data$desc)])
        
      }else if(!channel %in% chans & !channel %in% fr.data$desc){
        
        stop(paste(channel,"is not a valid channel for this flowFrame."))
        
      }
      
    })
    
  }
  
  return(channels)
  
})

#' Check Gate Type(s) Supplied to drawGate.
#'
#' @param type vector indicating the types of gates to construct using
#'   \code{drawGate}.
#' @param alias names of the populations to be gated.
#'
#' @return Stop gating process if type is incorrect or returns \code{type} as
#'   full lower case name(s). If a single type is supplied for multiple
#'   populations, the same type will be used for all populations.
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#' 
#' @seealso \code{\link{drawGate,flowFrame-method}}
#' @seealso \code{\link{drawGate,flowSet-method}}
#' @seealso \code{\link{drawGate,GatingSet-method}}
#'
#' @examples
#' \dontrun{
#' checkGateType(type = "r", alias = c("A","B","C"))
#' }
#'
#' @noRd
checkGateType <- function(type, alias){
  
  if(all(type %in% c("q","Q","quadrant","Quadrant")) & length(alias) != 4){
    
    stop("Supply the names of 4 poulations to alias for quadrant gates.")
    
  }
  
  gts <- c("polygon", "Polygon", "p", "P","rectangle", "Rectangle", "r", "R","interval", "Interval", "i", "I","threshold", "Threshold", "t", "T", "boundary", "Boundary", "b", "B","ellipse", "Ellipse", "e", "E","quadrant", "Quadrant", "q", "Q", "web", "Web", "w","W")
  
  if(!all(type %in% gts)){
    
    if(length(type[type %in% gts == FALSE]) >= 2){
      
      stop(paste(paste(type[type %in% gts == FALSE], collapse = " & "), "are not valid gate types for drawGate!"))
      
    }else{
      
      stop(paste(type[type %in% gts == FALSE],"is not a valid gate type for drawGate!"))
      
    }
    
  }
  
  type[type %in% c("polygon", "Polygon", "p", "P")] <- "polygon"
  
  type[type %in% c("rectangle", "Rectangle", "r", "R")] <- "rectangle"
  
  type[type %in% c("interval", "Interval", "i", "I")] <- "interval"
  
  type[type %in% c("threshold", "Threshold", "t", "T")] <- "threshold"
  
  type[type %in% c("boundary", "Boundary", "b", "B")] <- "boundary"
  
  type[type %in% c("ellipse", "Ellipse", "e", "E")] <- "ellipse"
  
  type[type %in% c("quadrant", "Quadrant", "q", "Q")] <- "quadrant"
  
  type[type %in% c("web", "Web", "w", "W")] <- "web"
  
  # Repeat type to equal length of alias
  if(length(type) != length(alias) & type[1] != "quadrant" & type[1] != "web"){
  
      type <- rep(type, length(alias))
      
  }
  
  return(type)
}

#' Check Alias Supplied to drawGate
#'
#' @param alias vector indicating the names of the populations to be gated.
#' @param type vector indicating the type(s) of gate(s) to be constructed.
#'
#' @return Stops the gating process if alias is missing or of the incorrect
#'   length given the gate type.
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#' 
#' @seealso \code{\link{checkGateType}}
#'
#' @examples
#' \dontrun{
#' checkAlias(alias = c("A","B","C","D"), type = "q")
#' }
#'
#' @noRd
checkAlias <- function(alias = NULL, type){
  
  if(is.null(alias)){
    
    stop("The name(s) of the population(s) to be gated must be supplied as the alias argument.")
  
  }

  if(type[1] == "quadrant" & length(alias) != 4){
    
    stop("Supply 4 population names to alias argument to construct quadrant gates.")
    
  }
  
  if(length(type) > 1){
    
    if(length(alias) != length(type)){
      
      stop("Length of alias must be the same length as type for multi-gates.")
      
    }
    
  }
  
}

#' Check Operating System & Open New Graphics Device
#'
#' \code{checkOSGD} is used internally by plotCyto to open an OS-specific
#' interactive garphics device to facilitate gate drawing. Mac users will need
#' to install \href{https://www.xquartz.org/}{XQuartz} for this functionality.
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @import grDevices 
#'
#' @export
checkOSGD <- function(){
  
  if(.Platform$OS.type == "windows"){
    
    grDevices::windows()
    
  }else if(.Platform$OS.type == "unix"){
    
    if(Sys.info()["sysname"] == "Linux"){
      
      X11()
      
    }else if(Sys.info()["sysname"] == "Darwin"){
      
      quartz()
      
    }
  
  }
  
}

#' Check File Exists in Working Directory
#' 
#' @param name filename including file extension to be checked.
#' 
#' @return TRUE/FALSE if file exists in the current working directory.
#' 
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @examples 
#' \dontrun{
#' checkFile("gatingTemplate.csv")
#' }
#'
#' @noRd
checkFile <- function(name){
  
  if(length(which(list.files() == name)) != 0){
    
    # File exists in working directory
    return(TRUE)
  
  }else if(length(which(list.files() == name)) == 0){
    
    # File does not exist in working directory
    return(FALSE)
  }
}

#' Check gatingTemplate for Existing Entry
#'
#' @param parent name of the parent population.
#' @param alias name of the population of interest.
#' @param gtfile csv file name of the gatingTemplate.
#'
#' @return stops the gating process if an entry already exists in the gtfile for
#'   the supplied alias.
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @importFrom utils read.csv
#'
#' @examples
#' \dontrun{
#' gt <- system.file("extdata", "Example-gatingTemplate.csv", package = "cytoRSuite")
#' checkTemplate(parent = "T Cells", alias = "CD4 T Cells", gtfile = gt)
#' }
#' 
#' @export
checkTemplate <- function(parent, alias, gtfile){
  
  if(checkFile(gtfile)){
    
    gt <- read.csv(gtfile, header = TRUE)
  
    # Parent and alias entries match file
    if(any(gt$parent %in% parent & gt$alias %in% alias)){
    
       message(paste(paste(gt$alias, collapse = " & "),"already exists in",gtfile,"."))
       stop("Please supply a different gtfile name or edit the existing gate(s) using editGate.")
    
    }
  
  }
  
}

#' Check Transformation List Object
#'
#' @param transList object of class
#'   \code{\link[flowCore:transformList-class]{transformList}} or
#'   \code{\link[flowWorkspace]{transformerList}} generated by
#'   \code{\link[flowCore:logicleTransform]{estimateLogicle}} which was used to
#'   transform the fluorescent channels of the samples.
#' @param inverse logical indicating whther the returned transformList should
#'   contain the inverse transformations.
#'
#' @return NULL or object of class
#'   \code{\link[flowCore:transformList-class]{transformList}}
#'
#' @importFrom flowCore inverseLogicleTransform transformList
#' @importFrom methods is
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @noRd
checkTransList <- function(transList = NULL, inverse = FALSE){
  
  if(is.null(transList)){
    
    return(NULL)
    
  }else{
    
    if(!class(transList)[1] %in% c("transformList","transformerList")){
      
      stop("Supplied transList should be of class transformList or transformerList.")
      
    }else{
    
      if(is(transList, "transformList")){
    
        if(inverse){
      
          transList <- inverseLogicleTransform(transList)
          return(transList)
      
        }else{
      
        return(transList)
    
        }
      
      }else if(is(transList, "transformerList")){
    
        if(inverse){
      
          transList <- transformList(names(transList), lapply(transList, `[[`, "inverse"))
          return(transList)
      
        }else{
      
          transList <- transformList(names(transList), lapply(transList, `[[`, "transform"))
          return(transList)
      
        }
    
      }
      
    }
  
  }
  
}

#' Check Overlays Supplied to plotCyto
#'
#' \code{checkOverlay} will check whether the supplied overlay is supported and
#' convert it into an appropriate format for use in \code{\link{plotCyto}}.
#'
#' @param x object of class \code{flowFrame} or \code{flowSet}.
#' @param overlay object to overlay.
#' @param subSample  numeric indicating the number of events to plot, set to all
#'   events by default. Reducing the sample size can significantly increase
#'   plotting speed on less powerful machines.
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @noRd
setGeneric(name = "checkOverlay",
           def = function(x, ...){standardGeneric("checkOverlay")}
)

#' Check Overlays Supplied to plotCyto
#'
#' \code{checkOverlay} will check whether the supplied overlay is supported and
#' convert it into an appropriate format for use in \code{\link{plotCyto}}. This
#' flowFrame method will return a list of flowFrames to overlay.
#'
#' @param x object of class \code{flowFrame}.
#' @param overlay object to overlay.
#' @param subSample  numeric indicating the number of events to plot, set to all
#'   events by default. Reducing the sample size can significantly increase
#'   plotting speed on less powerful machines.
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#' 
#' @importFrom flowCore sampleFilter
#'
#' @noRd
setMethod(checkOverlay, signature = "flowFrame", definition = function(x, overlay, subSample = NULL){
  
  # Assign x to fr
  fr <- x
  
  # Check overlay class
  if(class(overlay) == "flowFrame"){
    
    if(!is.null(subSample)){
      
      overlay <- Subset(overlay, sampleFilter(size = subSample))
      
    }
    overlay <- list(overlay)
    
  }else if(class(overlay) == "flowSet"){
    
    if(!is.null(subSample)){
      
      overlay <- Subset(overlay, sampleFilter(size = subSample))
      
    }
    
    overlay <- lapply(seq(1,length(overlay),1), function(x) overlay[[x]])
    
  }else if(all(as.vector(sapply(overlay,class)) == "flowFrame")){
    
    if(!is.null(subSample)){
      
      overlay <- lapply(overlay, function(x) {Subset(x, sampleFilter(size = subSample))})
      
    }
    
    overlay <- overlay
    
  }else if(all(as.vector(sapply(overlay,class)) == "flowSet") & length(overlay) == 1){
    
    if(!is.null(subSample)){
      
      overlay <- lapply(overlay, function(x) {Subset(x, sampleFilter(size = subSample))})
      
    }
    
    overlay <- lapply(overlay, function(x){lapply(seq(1,length(x),1), function(y) x[[y]])})[[1]]
    
  }else{
    
    stop("Overlay should be either a flowFrame, flowSet, list of flowFrames or a list containing a flowSet.")
    
  }
  
  # return is a list of flowFrames to overlay
  return(overlay)
  
})
  
#' Check Overlays Supplied to plotCyto
#'
#' \code{checkOverlay} will check whether the supplied overlay is supported and
#' convert it into an appropriate format for use in \code{\link{plotCyto}}. This
#' flowSet method will return a list of flowFrame lists to overlay.
#'
#' @param x object of class \code{flowSet}.
#' @param overlay object to overlay.
#' @param subSample  numeric indicating the number of events to plot, set to all
#'   events by default. Reducing the sample size can significantly increase
#'   plotting speed on less powerful machines.
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#' 
#' @importFrom flowCore sampleFilter
#'
#' @noRd
setMethod(checkOverlay, signature = "flowSet", definition = function(x, overlay, subSample = NULL){
  
  # Assign x to fs
  fs <- x
  
  # Check overlay class
  if(class(overlay) == "flowFrame"){
    
    if(!is.null(subSample)){
      
      overlay <- Subset(overlay, sampleFilter(size = subSample))
      
    }
    
    overlay <- lapply(rep(list(overlay),length(fs)),"list")

  }else if(class(overlay) == "flowSet"){
    
    if(!is.null(subSample)){
      
      overlay <- Subset(overlay, sampleFilter(size = subSample))
      
    }
    
    overlay <- lapply(lapply(seq(1,length(overlay),1), function(x) overlay[[x]]),"list")
    
  }else if(all(as.vector(sapply(overlay,class)) == "flowFrame")){
    
    if(!is.null(subSample)){
      
      overlay <- lapply(overlay, function(x) {Subset(x, sampleFilter(size = subSample))})
      
    }
    
    if(length(overlay) == 1){
      
      overlay <- lapply(rep(list(overlay[[1]]),length(fs)),"list")
      
    }else{
    
      if(length(overlay) != length(fs)){
        
        stop("Supplied list of flowFrames should be of the same length as the flowSet.")
        
      }
      overlay <- lapply(overlay,"list")
    
    }
    
  }else if(all(as.vector(sapply(overlay,class)) == "flowSet")){
    
    if(!all(as.vector(sapply(overlay,length)) == length(fs))){
      
      stop("Each flowSet in supplied list should be of the same length as the supplied flowSet.")
      
    }
    
    if(!is.null(subSample)){
      
      overlay <- lapply(overlay, function(x) {Subset(x, sampleFilter(size = subSample))})
      
    }
    
    # list of flowFrame lists
    overlay <- lapply(overlay, function(x){lapply(seq(1,length(x),1), function(y) x[[y]])})
    overlay <- lapply(seq_along(fs), function(x){lapply(overlay, `[[`, x)})
    
  }else if(all(do.call("rbind", lapply(overlay, function(x){sapply(x,class)})) == "flowFrame")){
    
    if(all(as.vector(lapply(overlay, function(x){sapply(x,length)})) != length(fs))){
    
      stop("Each list of flowFrames should be the same length as the flowSet.")
      
    }
      
    if(!is.null(subSample)){
      
      overlay <- lapply(overlay, function(x) {lapply(x, function(y){Subset(y, sampleFilter(size = subSample))})})
      
    }
    
    # list of flowFrame lists
    overlay <- lapply(seq_along(fs), function(x){lapply(overlay, `[[`, x)})
    
  }else{
    
    stop("Overlay should be either a flowFrame, flowSet, list of flowFrames or a list of flowSets.")
    
  }
  
  # return is a list of flowFrame lists to overlay -  1 flowFrame list per flowFrame in fs
  return(overlay)
  
})

#' Check Overlays Supplied to plotCyto
#'
#' \code{checkOverlay} will check whether the supplied overlay is supported and
#' convert it into an appropriate format for use in \code{\link{plotCyto}}. This
#' flowSet method will return a list of flowFrame lists to overlay.
#'
#' @param x object of class \code{GatingHierarchy}.
#' @param overlay object to overlay.
#' @param subSample  numeric indicating the number of events to plot, set to all
#'   events by default. Reducing the sample size can significantly increase
#'   plotting speed on less powerful machines.
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#' 
#' @importFrom flowWorkspace getData
#'
#' @noRd
setMethod(checkOverlay, signature = "GatingHierarchy", definition = function(x, overlay, subSample = NULL){
  
  # Assign x to gh
  gh <- x
  
  # Extract flowFrame
  fr <- getData(gh, "root")
  
  # Overlay should be character vector of population names
  if(inherits(overlay, "flowFrame") | inherits(overlay, "flowSet") | inherits(overlay, "list")){
    
  }else if(inherits(overlay, "character")){
    
    if(!all(overlay %in% basename(getNodes(gh)))){
      
      stop("Supplied population(s) for overlay does not exist in the GatingHierarchy.")
      
    }else{
      
      nms <- overlay
      overlay <- lapply(overlay, function(x){
        
        getData(gh, x)
        
      })
      names(overlay) <- nms
    
    }
    
  }
  
  # checkOverlay to convert overlay to correct format
  checkOverlay(fr, overlay = overlay, subSample = subSample)
  
})

#' Check Overlays Supplied to plotCyto
#'
#' \code{checkOverlay} will check whether the supplied overlay is supported and
#' convert it into an appropriate format for use in \code{\link{plotCyto}}. This
#' flowSet method will return a list of flowFrame lists to overlay.
#'
#' @param x object of class \code{GatingSet}.
#' @param overlay object to overlay.
#' @param subSample  numeric indicating the number of events to plot, set to all
#'   events by default. Reducing the sample size can significantly increase
#'   plotting speed on less powerful machines.
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#' 
#' @importFrom flowWorkspace getData
#'
#' @noRd
setMethod(checkOverlay, signature = "GatingSet", definition = function(x, overlay, subSample = NULL){
  
  # Assign x to gh
  gs <- x
  
  # Extract flowFrame
  fs <- getData(gs, "root")
  
  # Overlay should be character vector of population names
  if(inherits(overlay, "flowFrame") | inherits(overlay, "flowSet") | inherits(overlay, "list")){
    
  }else if(inherits(overlay, "character")){
    
    if(!all(overlay %in% basename(getNodes(gs)))){
      
      stop("Supplied population(s) for overlay does not exist in the GatingHierarchy.")
      
    }else{
      
      nms <- overlay
      overlay <- lapply(overlay, function(x){
        
        getData(gs, x)
        
      })
      names(overlay) <- nms
      
    }
    
  }
  
  # checkOverlay to convert overlay to correct format
  checkOverlay(fs, overlay = overlay, subSample = subSample)
  
})

#' Check Statistic for ComputeStats
#' 
#' @param stat computeStats statistic.
#' 
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @noRd
checkStat <- function(stat){
  
  if(!stat %in% c("mean","Mean","median","Median","mode","Mode","count","Count","freq","Freq","geo mean","Geo mean","Geo Mean","CV","cv","CVI","cvi")){
  
    stop("Supplied statistic not supported.")
    
  }

  if(stat %in% c("mean","Mean")){
    stat <- "mean"
  }else if(stat %in% c("median", "Median")){
    stat <- "median"
  }else if(stat %in% c("mode","Mode")){
    stat <- "mode"
  }else if(stat %in% c("count","Count")){
    stat <- "count"
  }else if(stat %in% c("freq","Freq")){
    stat <- "freq"
  }else if(stat %in% c("geo mean","Geo mean","Geo Mean")){
    stat <- "geo mean"
  }else if(stat %in% c("cv","CV")){
    stat <- "CV"
  }else if(stat %in% c("cvi","CVI")){
    stat <- "CVI"
  }

  return(stat)
  
}