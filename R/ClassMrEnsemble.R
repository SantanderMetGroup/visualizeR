MrEnsemble <-setClass("MrEnsemble",
  contains = "MrGrid"
)
setMethod(f="initialize", signature="MrEnsemble", 
  definition=function(.Object, ...){
  .Object <- callNextMethod()
  mr.data.valid(.Object)
  return(.Object)
})


setMethod(f="mr.data.valid", signature="MrEnsemble",
	definition=function(.Object){
		if(length(.Object@Members)<=1) {
			stop("The input dataset is not a multi-member ensemble.")
		}
	}
)

