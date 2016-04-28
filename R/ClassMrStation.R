MrStation <-setClass("MrStation", 
  slots = c(Metadata="list"),
  contains = "MrData"
)
setMethod(f="initialize", signature="MrStation", 
  definition=function(.Object, ...){
  .Object <- callNextMethod()
  args <- list(...)
  .Object@Metadata <- args$Metadata
  return(.Object)
})


setGeneric(name="getMetadata", def=function(.Object) standardGeneric("getMetadata"))
setMethod(f="getMetadata",
  signature="MrStation",
  definition=function(.Object){
    return(.Object@Metadata)
})
