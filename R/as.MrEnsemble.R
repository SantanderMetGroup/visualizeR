##     as.MrEnsemble Multi member data conversion to S4 class.
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

#' @title Convert a gridded data set with more than one member to a S4 class. 
#' 
#' @description Convert a gridded data set with more than one ensemble member to the MrEnsemble
#'  class. MrEnsemble is a S4 class derived from the MrGrid. The data set must have the elements Variable, 
#'  Dates, Data, xyCoords, InitializationDates and Members. This function is prepared to convert 
#'  the data sets loaded from the ECOMS User Data Gateway (ECOMS-UDG). See the loadeR.ECOMS R 
#'  package for more details (http://meteo.unican.es/trac/wiki/udg/ecoms/RPackage). 
#' 
#' @param obj.s3 A list the with the elements Variable, Dates, Data, xyCoords, InitializationDates 
#'  and Members
#
#' @import methods
#' 
#' @details  
#'  The visualization functions are programmed to work with S4 class. This function converts gridded 
#'  data sets (with more than one member) to S4 to use those functions. 
#'  
#' @note For gridded data sets with just one ensemble member use as.MrGrid function.
#'  For station data use as.MrStation function.
#'  
#' @author M.D. Frias \email{mariadolores.frias@@unican.es}, J. Fernandez and Max Tuni
#' 


as.MrEnsemble <- function(obj.s3){
  lons <- obj.s3$xyCoords$x
  lats <- obj.s3$xyCoords$y
  xyCoords <- list(x=seq(1,length(lons)),y=seq(1,length(lats)))
  attr(xyCoords, "projection") <- "lat-lon"
  attr(xyCoords, "longitude") <- lons
  attr(xyCoords, "latitude") <- lats
  dimNames <- attr(obj.s3$Data, "dimensions")
  newdim <- c(length(obj.s3$Variable$varName),length(obj.s3$Members),dim(obj.s3$Data)[grep("time", dimNames)],length(lats),length(lons))
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
      InitializationDates = as.list(obj.s3$InitializationDates),
      Members = obj.s3$Members
    )   
  )
}
