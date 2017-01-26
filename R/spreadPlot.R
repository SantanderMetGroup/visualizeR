##     spreadPlot Box plot for visualization of forecast skill of ensemble predictions. 
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

#' @title Box plot for visualization of forecast skill of ensemble predictions.
#' 
#' @description Box plot for visualization of forecast skill of ensemble predictions. 
#'  This function is prepared to plot the data sets loaded from the ECOMS User Data Gateway (ECOMS-UDG). See 
#'  the loadeR.ECOMS R package for more details (http://meteo.unican.es/trac/wiki/udg/ecoms/RPackage).
#' 
#' @param mm.obj A multi-member list with predictions, either a field or a multi-member 
#'  station object as a result of downscaling of a forecast using station data. See details.
#' @param year.target Year selected to plot the probability of the tercile in bars.
#' @param detrend Logical indicating if the data should be detrended. Default is TRUE.
#' @param boxplot Logical flag indicating whether a boxplot should be added to the graph. Default is TRUE.
#' @param violin  Logical flag indicating whether a violin plot should be added to the graph instead of the boxplot. 
#'  Default is FALSE.
#' @param add.points Logical flag indicating whether crosses indicating the ensemble members should be added to the 
#'  graph. Default is FALSE.
#' @param pch Single character or an integer code for one of the set of R graphics symbols. Cross is the default symbol. 
#'  Only consider if add.points is TRUE. 
#' 
#' @importFrom transformeR array3Dto2Dmat mat2Dto3Darray 
#' @importFrom stats filter
#' @importFrom graphics lines
#' @import sm
#' @import vioplot 
#'   
#' @export
#' 
#' @details 
#'  For rectangular spatial domains (i.e., for fields), the spatial average is first computed (with a warning) to obtain a
#'  unique series for the whole domain. The climatology for the forecast period is computed. The shaded areas show 
#'  the central tercile (dark shade) and the maximum and minimum (light shade). To avoid overinterpretation of daily 
#'  peaks, the daily data has been smoothed by means of a (centered) moving average of 31 days. Therefore, at the 
#'  location of the boxplots, the background shows the monthly mean forecast (the terciles and extremes being computed 
#'  over members and years). Monthly means are calculated for each ensemble member. This information is included to the 
#'  graph in a boxplot or a violin plot (to unveil multimodalities in the data) or/and the values of the ensemble members. 
#' 
#'  In case of multi-member fields or stations, they are spatially averaged to obtain one single time series
#'  for each member prior to data analysis, with a warning.  
#'   
#' 
#' @note The computation of climatological terciles requires a representative period to obtain meaningful results.
#' 
#' @author M.D. Frias \email{mariadolores.frias@@unican.es} and J. Fernandez
#' 
#' @family VisualizeR

spreadPlot <- function(mm.obj, year.target, detrend = FALSE, boxplot=TRUE, violin = FALSE, add.points=FALSE, pch=NULL) {
     # Check data temporal scale. Daily or subdaily required.
     mm.dates <-as.POSIXlt(mm.obj$Dates$start)
     if (diff.Date(mm.dates$mday)[1]==0){
       # Annual data
       if (diff.Date(mm.dates$year)[1]==1){
         stop("Data are not at daily time scale")
       } # Monthly data
       if (diff.Date(mm.dates$mon)[1]==1){
         stop("Data are not at daily time scale")
       } 
     }
     # Check input datasets
     if (isS4(mm.obj)==FALSE){
       mm.obj <- convertIntoS4(mm.obj)
     }
     stopifnot(checkData(mm.obj))
     yy <- unique(getYearsAsINDEX.S4(mm.obj))      
     if (!year.target %in% yy) {
       stop("Target year outside temporal data range")
     }
     # Detrend
     if (detrend){
       mm.obj <- detrend.forecast(mm.obj)
     }
     # Spatial mean of forecast and Benchmark if necessary
     sp.mm.obj <- spatialMean(mm.obj)
     # Plot the climatology shadow 
     ma <- function(x, n=31){filter(x, rep(1/n, n), sides=2)} # Time filter (moving average)
     arr <- getData(sp.mm.obj)[1,,,1,1]
     mm.ma <- t(apply(arr, MARGIN=1, FUN=ma)) 
     days <- sprintf("%02d%02d", mm.dates$mon+1, mm.dates$mday)
     days <- factor(days, levels=unique(days))
     # Quantiles mixing for each day all the years and members
     ens.quant <- t(do.call("rbind",
                            lapply(unique(days), FUN=function(x){quantile(c(mm.ma[,days==x]), probs=c(0.0,1/3,0.5,2/3,1.0), na.rm=T)})
     ))
     season.days <- 1:length(unique(days))    
     par(bg="white", mar=c(4,4,1,1))
     plot(season.days, ens.quant[3,], ylim = range(mm.ma, na.rm=T), ty = 'n', ylab = paste(getVarName(mm.obj),"- Daily Mean"), xlab = sprintf("time\n(shade: %d to %d, symbols: %d)", mm.dates[1]$year+1900, last(mm.dates)$year+1900, year.target), xaxt="n")
     polygon(x = c(season.days, rev(season.days)), y = c(ens.quant[1, ], rev(ens.quant[5, ])), border = "transparent", col = rgb(0.2,0.2,0.2,.2))
     polygon(x = c(season.days, rev(season.days)), y = c(ens.quant[2, ], rev(ens.quant[4, ])), border = "transparent", col = rgb(0.3,0.3,0.3,.3))
     lines(season.days, ens.quant[3,], lwd=2)      
     # Boxplot, Violinplot and/or points for the selected year
     year.filter <- getYearsAsINDEX.S4(mm.obj)==year.target
     month.index <- factor(months(mm.dates[year.filter]), levels=unique(months(mm.dates)))
     nmembers <- length(getMembers(mm.obj))
     mm.monmeans <- do.call("rbind", lapply(c(1:nmembers), function(x) tapply(arr[x,year.filter], INDEX = month.index, FUN = mean, na.rm = TRUE)))
     pos.cen <- grep("..15", levels(days)) + 0.5
     pos.ini <- grep("..01", levels(days)) 
     if (violin) {
       boxplot=FALSE # Uncompatible options
       for(i in seq(1,length(pos.cen))){
         vioplot(mm.monmeans[,i], wex=6, border="black", at=pos.cen[i], col="grey", add=T)
       }
     }       
     if (boxplot){
       boxplot(mm.monmeans, boxwex=3, lwd=3, border="black", at=pos.cen, col=rgb(1,0,0,0), axes=F, add=T)
     }     
     if (add.points) {
       # Different color of the points depending on the tercile 
       color <- matrix(c(rep("#353535",nrow(mm.monmeans)*ncol(mm.monmeans))), ncol=ncol(mm.monmeans), nrow=nrow(mm.monmeans))
       for (i in seq(1,ncol(mm.monmeans))){
         color[mm.monmeans[,i]<ens.quant[2, grep("..15", levels(days))[i]],i] <- "blue"
         color[mm.monmeans[,i]>ens.quant[4, grep("..15", levels(days))[i]],i] <- "red"
       }       
       if (is.null(pch)) {
         pch="+"
       }  
       points(rep(pos.cen,each=nmembers), mm.monmeans, col=color, pch=pch)
     }
     axis(1, at=pos.cen, labels=levels(month.index), las="1")
     abline(v=pos.ini, lty="dotted")          
}
# End

