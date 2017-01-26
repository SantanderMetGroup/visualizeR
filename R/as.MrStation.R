##     as.MrStation Station data conversion to S4 class.
##
##     Copyright (C) 2016 Santander Meteorology Group (http://www.meteo.unican.es)
##
##     This program is free software: you can redistribute it and/or modify
##     it under the terms of the GNU General Public License as published by
##     the Free Software Foundation, either version 3 of the License, or
##     (at your option) any later version.
## 
##     This program is distributed in the hope that it will be useful,
##     but WITHOUT ANY WARRANTY; without even the implied warranty of
##     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##     GNU General Public License for more details.
## 
##     You should have received a copy of the GNU General Public License
##     along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' @title Convert station data to a S4 class. 
#' 
#' @description Convert station data to the MrStation class. MrStation is a S4 class derived 
#'  from the MrData. The data set must have the elements Variable, Dates, Data, xyCoords and 
#'  MetaData. This function is prepared to convert the station data loaded from the ECOMS User 
#'  Data Gateway (ECOMS-UDG). See the loadeR.ECOMS R package for more details 
#'  (http://github.com/SantanderMetGroup/loadeR.ECOMS). 
#' 
#' @param obj.s3 A list the with the elements Variable, Dates, Data, xyCoords and MetaData. 
#
#' @import methods
#' 
#' @details  
#'  The visualization functions are programmed to work with S4 class. This function converts  
#'  station data to S4 to use those functions. 
#'  
#' @note For gridded data sets with just one ensemble member use as.MrGrid function.
#'  For gridded data sets with more than one ensemble member use as.MrEnsemble function.
#'  
#' @author M.D. Frias \email{mariadolores.frias@@unican.es}, J. Fernandez and Max Tuni
#' 
#' @family VisualizeR

as.MrStation <- function(obj.s3){
  lons <- obj.s3$xyCoords[,1]
  lats <- obj.s3$xyCoords[,2]
  xyCoords <- list(x=seq(1,length(lons)),y=c(1))
  attr(xyCoords, "projection") <- "stations"
  attr(xyCoords, "longitude") <- lons
  attr(xyCoords, "latitude") <- lats
  newdim <- c(length(obj.s3$Variable$varName),1,dim(obj.s3$Data)[1],1,length(lons))
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
