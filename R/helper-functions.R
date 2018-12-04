#' Extract Fluorescent Channels
#'
#' @param x object of class \code{\link[flowCore:flowFrame-class]{flowFrame}},
#'   \code{\link[flowCore:flowSet-class]{flowSet}} or
#'   \code{\link[flowWorkspace:GatingSet-class]{GatingSet}}.
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#' 
#' @seealso \code{\link{getChannels,flowFrame-method}}
#' @seealso \code{\link{getChannels,flowSet-method}}
#' @seealso \code{\link{getChannels,GatingSet-method}}
#'
#' @export
setGeneric(name="getChannels",
           def=function(x){standardGeneric("getChannels")}
)

#' Extract Fluorescent Channels - flowFrame Method
#' 
#' @param x object \code{\link[flowCore:flowFrame-class]{flowFrame}}.
#' 
#' @return vector of fluorescent channels.
#' 
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#' 
#' @seealso \code{\link{getChannels,flowSet-method}}
#' @seealso \code{\link{getChannels,GatingSet-method}}
#' 
#' @examples 
#' \dontrun{
#' fs <- Activation
#' getChannels(fs[[1]])
#' }
#' 
#' @export
setMethod(getChannels, signature = "flowFrame", definition = function(x){
  
  colnames(x@description$SPILL)
  
})

#' Extract Fluorescent Channels - flowSet Method
#' 
#' @param x object \code{\link[flowCore:flowSet-class]{flowSet}}.
#' 
#' @return vector of fluorescent channels.
#' 
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#' 
#' @seealso \code{\link{getChannels,flowFrame-method}}
#' @seealso \code{\link{getChannels,GatingSet-method}}
#' 
#' @examples 
#' \dontrun{
#' fs <- Activation
#' getChannels(fs)
#' }
#' 
#' @export
setMethod(getChannels, signature = "flowSet", definition = function(x){
  
  colnames(x[[1]]@description$SPILL)
  
})

#' Extract Fluorescent Channels - GatingSet Method
#' 
#' @param x object \code{\link[flowWorkspace:GatingSet-class]{GatingSet}}.
#' 
#' @return vector of fluorescent channels.
#' 
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#' 
#' @seealso \code{\link{getChannels,flowFrame-method}}
#' @seealso \code{\link{getChannels,flowSet-method}}
#' 
#' @examples 
#' \dontrun{
#' fs <- Activation
#' gs <- GatingSet(fs)
#' getChannels(gs)
#' }
#' 
#' @export
setMethod(getChannels, signature = "GatingSet", definition = function(x){
  
  colnames(x@data[[1]]@description$SPILL)
  
})

#' Select Fluorescent Channel for Compensation Controls
#'
#' @param x object of class \code{\link[flowCore:flowFrame-class]{flowFrame}},
#'   \code{\link[flowCore:flowSet-class]{flowSet}} or
#'   \code{\link[flowWorkspace:GatingSet-class]{GatingSet}} containing
#'   compensation controls.
#'
#' @return vector of channels in order of compensation Control samples.
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @export
setGeneric(name="selectChannels",
           def=function(x){standardGeneric("selectChannels")}
)

#' Select Fluorescent Channel for Compensation Controls - flowFrame Method
#'
#' @param x object of class \code{\link[flowCore:flowFrame-class]{flowFrame}}.
#'
#' @return selected channel associated with the supplied flowFrame.
#'
#' @importFrom utils menu
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @examples 
#' \dontrun{
#' fs <- Activation
#' selectChannels(fs[[1]])
#' }
#'
#' @export
setMethod(selectChannels, signature = "flowFrame", definition = function(x){
  
  # Assign x to fr
  fr <- x
  
  opts <- getChannels(fr)
  
  # Print sample name and select channel
  message(paste("Select a fluorescent channel for the following compensation control:", fr@description$GUID))
  
  if(getOption("CytoRSuite_interact") == TRUE){
    
    channel <- opts[menu(choices = opts, graphics = TRUE)]
    
  }else{
    
    channel <- opts[1]
    
  }
  
  return(channel)
  
}) 
 
#' Select Fluorescent Channel for Compensation Controls - flowSet Method
#'
#' @param x object of class
#'   \code{\link[flowCore:flowSet-class]{flowSet}} containing compensation
#'   controls.
#'
#' @return vector of channels in order of flowSet.
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @importFrom utils menu
#'
#' @examples 
#' \dontrun{
#' fs <- Activation
#' selectChannels(fs)
#' }
#'
#' @export
setMethod(selectChannels, signature = "flowSet", definition = function(x){
  
  # Assign x to fs
  fs <- x
  
  opts <- c(getChannels(fs), "Unstained")
  
  # Print sample name and select channel
  channels <- opts[sapply(pData(fs)$name, function(x){
    
    message("Select a fluorescent channel for the following compensation control:")
    
    print(x)
    
    if(getOption("CytoRSuite_interact") == TRUE){
        
      menu(choices = opts, graphics = TRUE)
        
    }else{
        
      1
        
    }
    
  })]
  
  return(channels)
  
}) 

#' Select Fluorescent Channel for Compensation Controls - GatingSet Method
#'
#' @param x object of class
#'   \code{\link[flowWorkspace:GatingSet-class]{GatingSet}} containing
#'   compensation controls.
#'
#' @return vector of channels in order of GatingSet.
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @importFrom utils menu
#'
#' @examples 
#' \dontrun{
#' fs <- Activation
#' gs <- GatingSet(fs)
#' selectChannels(gs)
#' }
#'
#' @export
setMethod(selectChannels, signature = "GatingSet", definition = function(x){
  
  # Assign x to gs
  gs <- x
  
  opts <- c(getChannels(gs), "Unstained")
  
  # Print sample name and select channel
  channels <- opts[sapply(pData(gs)$name, function(x){
    
    message("Select a fluorescent channel for the following compensation control:")
    
    print(x)
    
    if(getOption("CytoRSuite_interact") == TRUE){
      
      menu(choices = opts, graphics = TRUE)
      
    }else{
      
      1
      
    }
    
  })]
  
  return(channels)
  
})

#' Sample a flowFrame
#'
#' @param fr object of class \code{\link[flowCore:flowFrame-class]{flowFrame}}.
#' @param size numeric indicating the number of events to keep. If \code{size >
#'   nrow(fr)}, size is set to \code{nrow(fr)}.
#'
#' @return \code{\link[flowCore:flowFrame-class]{flowFrame}} restricted to
#'   \code{size} events.
#'
#' @importFrom BiocGenerics nrow
#' @importFrom flowCore sampleFilter
#' @importFrom flowCore Subset
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @examples 
#' \dontrun{
#' fs <- Activation
#' sampleFrame(fs[[1]], 50000)
#' }
#'
#' @export
sampleFrame <- function(fr, size = 250000){
  
  # Number of events
  events <- nrow(fr)
  
  if(events < size){
    
    size <- events
  
  }else{
    
  }

  smp <- sampleFilter(size = size)
  fr <- Subset(fr, smp)
  
  return(fr)
}
