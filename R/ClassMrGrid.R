MrGrid <-setClass("MrGrid", 
  slots = c(InitializationDates="character", Members="character"),
  prototype=list(InitializationDates=character(), Members=character()),
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


setGeneric(name="mr.data.valid", def=function(.Object) standardGeneric("mr.data.valid"))
setMethod(f="mr.data.valid", signature="MrGrid",
	definition=function(.Object){
		if(length(.Object@Members)>1) {
			stop("The input dataset is a multi-member ensemble.")
		}
	}
)


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

