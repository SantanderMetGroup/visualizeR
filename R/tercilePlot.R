##     tercilePlot Tercile plot for visualization of forecast skill of seasonal climate predictions.
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
##     along with this program. If not, see <http://www.gnu.org/licenses/>.

#' @title Tercile plot for visualization of forecast skill of seasonal climate predictions.
#' 
#' @description Tercile plot for the visualization of forecast skill of seasonal climate predictions.
#'  This function is prepared to plot the data sets loaded from the ECOMS User Data Gateway (ECOMS-UDG). See 
#'  the loadeR.ECOMS R package for more details (http://meteo.unican.es/trac/wiki/udg/ecoms/RPackage).
#' 
#' @param hindcast A multi-member list with the hindcast for verification. See details.
#' @param obs List with the benchmarking observations for forecast verification.
#' @param forecast A multi-member list with the forecasts. Default is NULL. 
#' @param year.target Year within the hindcast period considered as forecast. Default is NULL.
#' @param detrend Logical indicating if the data should be linear detrended. Default is FALSE.
#' @param color.pal Color palette for the representation of the probabilities. Default to \code{"bw"} (black and white).
#'  \code{"reds"} for a white-red transition, \code{"ypb"} for a yellow-pink-blue transition  or 
#'  \code{"tcolor"} for a colorbar for each tercile, blue-grey-red for below, normal and above terciles, respectively.
#' @param subtitle String to include a subtitle bellow the title. Default is NULL.
#' 
#' @importFrom RColorBrewer brewer.pal
#' @importFrom transformeR array3Dto2Dmat mat2Dto3Darray getYearsAsINDEX subsetGrid 
#' @importFrom abind abind
#' @importFrom fields image.plot
#' @importFrom grDevices grey.colors
#' @importFrom graphics par image axis points mtext
#'   
#' @export
#' 
#' @details 
#'  
#' First daily data are averaged to obtain a single seasonal value. For
#' rectangular spatial domains (i.e., for fields), the spatial average is first computed (with a warning) to obtain a
#' unique series for the whole domain. The corresponding terciles for each ensemble member are then computed
#' for the hindcast period. Thus, each particular member and season, are categorized into three categories (above, 
#' between or below), according to their respective climatological terciles. Then, a probabilistic forecast is computed 
#' year by year by considering the number of members falling within each category. This probability is represented by the
#' colorbar. For instance, probabilities below 1/3 are very low, indicating that a minority of the members 
#' falls in the tercile. Conversely, probabilities above 2/3 indicate a high level of member agreement (more than 66\% of members
#' falling in the same tercile). The observed terciles (the events that actually occurred) are represented by the 
#' white circles. If the forecast object is not NULL, then the probabilities for this season are also ploted next to the 
#' hindcast results.  
#'  
#' Finally, the ROC Skill Score (ROCSS) is computed for the hindcasts. It is indicated in the secondary (right) Y axis. 
#' For each tercile, it provides a quantitative measure of the forecast skill, and it is commonly used to evaluate the 
#' performance of probabilistic systems (Joliffe and Stephenson 2003). The value of this score ranges from 1 
#' (perfect forecast system) to -1 (perfectly bad forecast system). A value zero indicates no skill compared with a random 
#' prediction. If year.target is not NULL, this year is not included in the computation of the score (operational point 
#' of view). Significance of the Area under the ROC curve is highlighted with an *. 
#'  
#' In case of multi-member fields or stations, they are spatially averaged to obtain one single time series
#' for each member prior to data analysis, with a warning.   
#' 
#' @note The computation of climatological terciles requires a representative period to obtain meaningful results.
#' 
#' @examples \dontrun{
#' data(tas.cfs)
#' data(tas.cfs.operative.2016)
#' data(tas.ncep)
#' require(transformeR)
#' # Select spatial domain
#' tas.ncep2 <- subsetGrid(tas.ncep, lonLim = c(-80, -35), latLim = c(-12, 12))
#' tas.cfs2 <- subsetGrid(tas.cfs, lonLim = c(-80, -35), latLim = c(-12, 12))
#' tas.cfs.operative2.2016 <- subsetGrid(tas.cfs.operative.2016, 
#'                            lonLim = c(-80, -35), latLim = c(-12, 12))
#' # Tercile plot
#' tercilePlot(hindcast = tas.cfs2, obs = tas.ncep2, forecast = tas.cfs.operative2.2016)
#' }
#' 
#' @author M.D. Frias\email{mariadolores.frias@@unican.es} and J. Fernandez based on the original diagram 
#' conceived by A. Cofino (See Diez et al, 2011 for more details).
#' 
#' @family visualization functions
#' 
#' @references
#' Diez, E., Orfila, B., Frias, M.D., Fernandez, J., Cofino, A.S., Gutierrez, J.M., 2011. 
#' Downscaling ECMWF seasonal precipitation forecasts in Europe using the RCA model.
#' Tellus A 63, 757-762. doi:10.1111/j.1600-0870.2011.00523.x
#'    
#' Jolliffe, I. T. and Stephenson, D. B. 2003. Forecast Verification: A Practitioner's Guide in 
#' Atmospheri Science, Wiley, NY
 

tercilePlot <- function(hindcast, obs, forecast=NULL, year.target = NULL, detrend = FALSE, color.pal = c("bw", "reds", "ypb", "tcolor"), subtitle = NULL){
      # Check data dimension from the original data sets
      checkDim(hindcast)
      checkDim(obs)
      if (!is.null(forecast)){
        checkDim(forecast)   
      }
      color.pal <- match.arg(color.pal, c("bw", "reds", "ypb", "tcolor"))
      yy <- unique(getYearsAsINDEX(hindcast))  
      if (!is.null(year.target)){
        if (!year.target %in% yy) {
          stop("Target year outside temporal data range")
        }
      }
      if (is.null(forecast) & !is.null(year.target)){
        yy.forecast <- year.target
        forecast <- subsetGrid(hindcast, years=yy.forecast, drop=F)
        hindcast <- subsetGrid(hindcast, years=yy[yy!=yy.forecast], drop=F)
        obs <- subsetGrid(obs, years=yy[yy!=yy.forecast], drop=F)
        yy <- yy[yy!=yy.forecast]
      }
      # Check input datasets
      if (isS4(hindcast)==FALSE){
        hindcast <- convertIntoS4(hindcast)
      }
      if (isS4(obs)==FALSE){
        obs <- convertIntoS4(obs)
      }
      stopifnot(checkData(hindcast, obs))
      if (!is.null(forecast)){
        yy.forecast <- unique(getYearsAsINDEX(forecast))
        year.target <- NULL
        if (isS4(forecast)==FALSE){
          forecast <- convertIntoS4(forecast)
        }
      }
      # Detrend
      if (detrend){
        hindcast <- detrend.data(hindcast)
        obs <- detrend.data(obs)
        if (!is.null(forecast)){
          forecast <- detrend.data(hindcast, forecast)
        }
      }
      # Spatial mean of forecast and Benchmark if necessary
      sp.hindcast <- spatialMean(hindcast)
      sp.obs <- spatialMean(obs)
      # Computation of seasonal mean
      sm.hindcast <- seasMean(sp.hindcast)
      sm.obs <- seasMean(sp.obs)
      # Computation of terciles and exceedance probabilities
      probs.hindcast <- QuantileProbs(sm.hindcast)
      probs.obs <- QuantileProbs(sm.obs)
      cofinogram.data <- t(getData(probs.hindcast)[,,,,])
      obs.terciles <- t(getData(probs.obs)[,,,,])
      obs.t <- getData(probs.obs)[3,,,,]-getData(probs.obs)[1,,,,]
      # Compute ROCSS
      rocss.t.u <- rocss.fun(obs.terciles[,3], cofinogram.data[,3])
      rocss.t.m <- rocss.fun(obs.terciles[,2], cofinogram.data[,2])
      rocss.t.l <- rocss.fun(obs.terciles[,1], cofinogram.data[,1])
      # Calculations for the forecast
      if (!is.null(forecast)){
        sp.forecast <- spatialMean(forecast)
        sm.forecast <- seasMean(sp.forecast)
        probs.forecast <- QuantileProbs(sm.forecast, sm.hindcast)
        cofinogram.data <- rbind(cofinogram.data,rep(NaN,3),t(getData(probs.forecast)[,,,,]))
      }
      # Color selection
      if (color.pal=="tcolor"){
          t.color <- tercileBrewerColorRamp(10)     
          brks <- c(seq(0,1,length=nrow(t.color)+1))
      } else{
          cbar <- switch(color.pal, 
              "reds" <- c("#fff5f0", "#fff5f0", brewer.pal(8,"Reds")),
              "bw" = rev(grey.colors(10)), 
              "ypb" = rev(c("#000066FF","#0000C1FF","#1600FFFF","#5D00FFFF","#A412EDFF","#EB3FC0FF","#FF6D92FF","#FF9A65FF","#FFC738FF","#FFF50AFF"))
         )
         brks <- c(seq(0,1,length=length(cbar)+1))
      }
      opar <- par(no.readonly=TRUE)
      par(mar = c(4, 5, 0, 3)) #change lines of the margins
      par(oma = c(0, 0, 0, 3)) #change size of the outer margins
      n.x <- 1:nrow(cofinogram.data)
      if (color.pal=="tcolor"){          
          image(n.x, c(-1.5,-0.5), matrix(cofinogram.data[,1]), breaks=brks, col=t.color$low, ylab="", xlab="", asp = 1, yaxt="n", bty = "n", axes = FALSE)      
          image(n.x, c(-0.5,0.5), matrix(cofinogram.data[,2]), breaks=brks, col=t.color$middle, ylab="", xlab="", asp = 1, yaxt="n", bty = "n", axes = FALSE, add=TRUE)      
          image(n.x, c(0.5,1.5), matrix(cofinogram.data[,3]), breaks=brks, col=t.color$high, ylab="", xlab="", asp = 1, yaxt="n", bty = "n", axes = FALSE, add=TRUE)      
      } else{         
         image(n.x, c(-1,0,1), cofinogram.data, breaks=brks, col=cbar, ylab="", xlab="", asp = 1, yaxt="n", bty = "n", axes = FALSE)      
      }         
      axis(1, at = 1:length(yy), labels=yy, pos=-1.5, cex.axis=0.75, las=2) 
      if (!is.null(forecast)){
        axis(1, at = (length(yy)+2):(length(yy)+1+length(yy.forecast)), labels=yy.forecast, pos=-1.5, cex.axis=0.75, las=2) 
      }
      points(1:length(obs.t), obs.t, pch = 21, bg = "white")
      axis(2, at=-1:1, labels=c("Below", "Normal", "Above"), cex.axis=0.75, las="2")
      # Add skill score values and AUC significance to the plot   
      add.significance <- function(obj){
        if (obj$sig==TRUE){
          lab <- paste(round(obj$score.val,2),"*", sep="")      
        } else {
          lab <- round(obj$score.val,2)       
        }
        return(lab)
      } 
      axis(4, at=c(-1:1,2.3), labels=c(add.significance(rocss.t.l), add.significance(rocss.t.m), add.significance(rocss.t.u), "ROCSS"), las="2", tick = FALSE, cex.axis=0.8)
      if (color.pal=="tcolor"){       
          #par(oma = c(7, 0, 2, 5.7))
          image.plot(add = TRUE, horizontal = T, smallplot = c(0.20,0.85,0.3,0.35), legend.only = TRUE, breaks = brks, lab.breaks=c(rep("", length(brks))), col = t.color[,3], zlim=c(0,1))
          #par(oma = c(7, 0, 2, 4.2))
          image.plot(add = TRUE, horizontal = T, smallplot = c(0.20,0.85,0.23,0.28), legend.only = TRUE, breaks = brks, lab.breaks=c(rep("", length(brks))), col = t.color[,2], zlim=c(0,1))
          #par(oma = c(7, 0, 2, 2.7))
          image.plot(add = TRUE, horizontal = T, smallplot = c(00.20,0.85,0.16,0.21), legend.only = TRUE, breaks = brks, col = t.color[,1], zlim=c(0,1), legend.lab="Probability of the tercile")          
      } else{          
          par(oma = c(3, 0, 1, 3.2))
          #image.plot(add = TRUE, legend.only = TRUE, breaks = brks, col = cbar, smallplot = c(0.96,0.99,0.2,0.8), zlim=c(0,1), legend.lab="Probability of the tercile")            
          image.plot(add = TRUE, horizontal = T, smallplot = c(0.20,0.85,0.25,0.28), legend.only = TRUE, breaks = brks, col = cbar, zlim=c(0,1), legend.lab="Probability of the tercile", legend.cex=0.8, axis.args=c(cex.axis=0.8))            
      }
      mons.start <- months(as.POSIXlt((getDates(obs)$start)[1]),abbreviate=T)
      mons.end <- months(last(as.POSIXlt(getDates(obs)$end))-1, abbreviate=T)
      title <- sprintf("%s, %s to %s", attr(getVariable(hindcast), "longname"), mons.start, mons.end)
      mtext(title, side=3, line=-3, adj=0, cex=0.9, font=2)
      if (!is.null(subtitle)){
        mtext(subtitle, side=3, line=-4, adj=0, cex=0.7)
      }
      par(opar)
}
# End
