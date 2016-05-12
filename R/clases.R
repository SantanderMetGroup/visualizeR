
MrData <-setClass("MrData", 
  slots = c(Data="array", xyCoords="list", Variable="list", Dates="list", Transformation="character"),
  prototype=list(Data=array(), xyCoords=list(), Variable=list(), Dates=list(), Transformation=character())
)
setMethod(f="initialize", signature="MrData", 
  definition=function(.Object, ...){
  args <- list(...)
  .Object@Data <- args$Data
  .Object@xyCoords <- args$xyCoords
  .Object@Variable <- args$Variable
  .Object@Dates <- args$Dates
  .Object@Transformation <- args$Transformation
  return(.Object)
})

MrGrid <-setClass("MrGrid", 
  slots = c(InitializationDates="list", Members="character"),
  prototype=list(InitializationDates=list(), Members=character()),
  contains = "MrData"
)
setMethod(f="initialize", signature="MrGrid", 
  definition=function(.Object, ...){
  .Object <- callNextMethod()
  args <- list(...)
  if(!is.null(args$InitializationDates)){
    .Object@InitializationDates <- args$InitializationDates
  } 
  if(!is.null(args$Members)){
    .Object@Members <- args$Members
  }
  mr.data.valid(.Object)
  return(.Object)
})

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

MrEnsemble <-setClass("MrEnsemble",
  contains = "MrGrid"
)
setMethod(f="initialize", signature="MrEnsemble", 
  definition=function(.Object, ...){
  .Object <- callNextMethod()
  mr.data.valid(.Object)
  return(.Object)
})

setGeneric(name="mr.data.valid", def=function(object) standardGeneric("mr.data.valid"))
setMethod(f="mr.data.valid", signature="MrGrid",
	definition=function(object){
		if(length(object@Members)>1) {
			stop("The input dataset is a multi-member ensemble.")
		}
	}
)
setMethod(f="mr.data.valid", signature="MrEnsemble",
	definition=function(object){
		if(length(object@Members)<=1) {
			stop("The input dataset is not a multi-member ensemble.")
		}
	}
)

setGeneric(name="getData", def=function(.Object) standardGeneric("getData"))
setMethod(f="getData",
  signature="MrData",
  definition=function(.Object){
    return(.Object@Data)
})

setGeneric(name="getxyCoords", def=function(.Object) standardGeneric("getxyCoords"))
setMethod(f="getxyCoords",
  signature="MrData",
  definition=function(.Object){
    return(.Object@xyCoords)
})

setGeneric(name="getVariable", def=function(.Object) standardGeneric("getVariable"))
setMethod(f="getVariable",
  signature="MrData",
  definition=function(.Object){
    return(.Object@Variable)
})

setGeneric(name="getVarName", def=function(.Object) standardGeneric("getVarName"))
setMethod(f="getVarName",
  signature="MrData",
  definition=function(.Object){
    return(.Object@Variable$varName)
})

setGeneric(name="getDates", def=function(.Object) standardGeneric("getDates"))
setMethod(f="getDates",
  signature="MrData",
  definition=function(.Object){
    return(.Object@Dates)
})

setGeneric(name="getTransformation", def=function(.Object) standardGeneric("getTransformation"))
setMethod(f="getTransformation",
  signature="MrData",
  definition=function(.Object){
    return(.Object@Transformation)
})

setGeneric(name="getInitializationDates", def=function(.Object) standardGeneric("getInitializationDates"))
setMethod(f="getInitializationDates",
  signature="MrGrid",
  definition=function(.Object){
    return(.Object@InitializationDates)
})

setGeneric(name="getMembers", def=function(.Object) standardGeneric("getMembers"))
setMethod(f="getMembers",
  signature="MrGrid",
  definition=function(.Object){
    return(.Object@Members)
})

setGeneric(name="getMetadata", def=function(.Object) standardGeneric("getMetadata"))
setMethod(f="getMetadata",
  signature="MrStation",
  definition=function(.Object){
    return(.Object@Metadata)
})
