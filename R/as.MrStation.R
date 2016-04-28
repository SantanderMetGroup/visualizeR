as.MrStation <- function(obj.s3){
  lons <- obj.s3$xyCoords[,1]
  lats <- obj.s3$xyCoords[,2]
  xyCoords <- list(x=seq(1,length(lons)),y=c(1))
  attr(xyCoords, "projection") <- "stations"
  attr(xyCoords, "longitude") <- lons
  attr(xyCoords, "latitude") <- lats
  newdim <- c(length(obj.s3$Variable$varName),1,dim(obj.s3$Data)[1],1,dim(obj.s3$Data)[2])
  newdata <- obj.s3$Data
  attr(newdata, "dimensions") <- c("var","member","time","y","x")
  dim(newdata) <- newdim
  if(is.null(obj.s3$Transformation)){
    obj.s3$Transformation <- character(0)
  }
  return(
    MrStation(
      Variable = obj.s3$Variable,
      Dates = obj.s3$Dates,
      Data = newdata,
      Transformation = obj.s3$Transformation,
      xyCoords = xyCoords,
      Metadata = obj.s3$Metadata
    )   
  )
}