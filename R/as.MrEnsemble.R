
#' @import methods
#' @export

as.MrEnsemble <- function(obj.s3){
  lons <- obj.s3$xyCoords$x
  lats <- obj.s3$xyCoords$y
  xyCoords <- list(x=seq(1,length(lons)),y=seq(1,length(lats)))
  attr(xyCoords, "projection") <- "lat-lon"
  attr(xyCoords, "longitude") <- lons
  attr(xyCoords, "latitude") <- lats
  dimNames <- attr(obj.s3$Data, "dimensions")
  newdim <- c(length(obj.s3$Variable$varName),length(obj.s3$Members),dim(obj.s3$Data)[grep("time", dimNames)],dim(obj.s3$Data)[grep("lat", dimNames)],dim(obj.s3$Data)[grep("lon", dimNames)])
  newdata <- obj.s3$Data
  attr(newdata, "dimensions") <- c("var","member","time","y","x")
  dim(newdata) <- newdim
  if(is.null(obj.s3$Transformation)){
    obj.s3$Transformation <- character(0)
  }
  return(
    MrEnsemble(
      Variable = obj.s3$Variable,
      Dates = obj.s3$Dates,
      Data = newdata,  
      Transformation = obj.s3$Transformation,
      xyCoords = xyCoords,
      InitializationDates = obj.s3$InitializationDates,
      Members = obj.s3$Members
    )   
  )
}
