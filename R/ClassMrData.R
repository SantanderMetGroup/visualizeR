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
