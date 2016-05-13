#' @title Convert a gridded data set with just one ensemble member to a S4 class. 
#' 
#' @description Convert a gridded data set with just one ensemble member to the MrGrid class. 
#'  MrGrid is a S4 class derived from the MrData. The data set must have the elements Variable, 
#'  Dates, Data, xyCoords, InitializationDates and Members. This function is prepared to convert 
#'  the data sets loaded from the ECOMS User Data Gateway (ECOMS-UDG). See the loadeR.ECOMS R 
#'  package for more details (http://github.com/SantanderMetGroup/loadeR.ECOMS). 
#' 
#' @param obj.s3 A list the with the elements Variable, Dates, Data, xyCoords, InitializationDates 
#'  and Members
#
#' @import methods
#' 
#' @details  
#'  The visualization functions are programmed to work with S4 class. This function converts gridded 
#'  data sets (with just one member) to S4 to use those functions. 
#'  
#' @note For gridded data sets with more than one ensemble member use as.MrEnsemble function.
#'  For station data use as.MrStation function.
#'  
#' @author M.D. Frias \email{mariadolores.frias@@unican.es}, J. Fernandez and Max Tuni
#' 
#' @family VisualizeR

as.MrGrid <- function(obj.s3){
  lons <- obj.s3$xyCoords$x
  lats <- obj.s3$xyCoords$y
  xyCoords <- list(x=seq(1,length(lons)),y=seq(1,length(lats)))
  attr(xyCoords, "projection") <- "lat-lon"
  attr(xyCoords, "longitude") <- lons
  attr(xyCoords, "latitude") <- lats
  dimNames <- attr(obj.s3$Data, "dimensions")
  if(is.null(obj.s3$Members)){
    obj.s3$Members <- "Member_1"
  }
  if(is.null(obj.s3$Transformation)){
    obj.s3$Transformation <- character(0)
  }
  newdim <- c(length(obj.s3$Variable$varName),length(obj.s3$Members),dim(obj.s3$Data)[grep("time", dimNames)],dim(obj.s3$Data)[grep("lat", dimNames)],dim(obj.s3$Data)[grep("lon", dimNames)])
  newdata <- obj.s3$Data
  attr(newdata, "dimensions") <- c("var","member","time","y","x")
  dim(newdata) <- newdim
  return(
    MrGrid(
      Variable = obj.s3$Variable,
      Dates = obj.s3$Dates,
      Data = newdata,  
      Transformation = obj.s3$Transformation, 
      xyCoords = xyCoords,
      InitializationDates = as.list(obj.s3$InitializationDates),
      Members = obj.s3$Members
    )   
  )
}
