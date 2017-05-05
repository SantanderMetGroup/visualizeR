##     tercileBarplot Tercile bar plot for visualization of forecast skill of seasonal climate predictions.
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

#' @title Tercile bar plot for visualization of forecast skill of seasonal climate predictions.
#' 
#' @description Tercile bar plot for visualization of the skill of seasonal climate predictions for a particular year.
#'  This function is prepared to plot the data sets loaded from the ECOMS User Data Gateway (ECOMS-UDG). See 
#'  the loadeR.ECOMS R package for more details (http://meteo.unican.es/trac/wiki/udg/ecoms/RPackage).
#'  
#' @param hindcast A multi-member list with the hindcast for verification. See details.
#' @param obs List with the benchmarking observations for forecast verification.
#' @param forecast A multi-member list with the forecasts. Default is NULL. 
#' @param year.target Year within the hindcast period considered as forecast. Default is NULL.
#' @param detrend Logical indicating if the data should be linear detrended. Default is FALSE.
#' @param score.threshold Threshold to remark high positive score values in the figure.
#' @param subtitle String to include a subtitle bellow the title. Default is NULL.
#' 
#' @importFrom fields image.plot
#' @importFrom transformeR array3Dto2Dmat mat2Dto3Darray subsetGrid
#' @importFrom graphics barplot
#'   
#' @export
#' 
#' @details 
#'  
#' First daily data are averaged to obtain a single seasonal value. For rectangular spatial domains (i.e., for fields), 
#' the spatial average is first computed (with a warning) to obtain a
#' unique series for the whole domain. The corresponding terciles for each ensemble member are then computed
#' for the hindcast period. Thus, each particular member and season, are categorized into three categories (above, 
#' between or below), according to their respective climatological terciles. Then, a probabilistic forecast is computed 
#' year by year by considering the number of members falling within each category. The probability for the the forecast 
#' or selected year is represented by the bars. The 1/3 probability is plotted by a grey line. For instance, probabilities below this 
#' line are very low, indicating that a minority of the members falls in the tercile. Conversely, probabilities above 2/3 
#' indicate a high level of member agreement (more than 66\% of members falling in the same tercile).  
#'  
#' Finally, the ROC Skill Score (ROCSS) is indicated at the bottom part of the bar plot for each tercile. It provides a 
#' quantitative measure of the forecast skill, and it is commonly used to evaluate the performance of probabilistic systems
#' (Joliffe and Stephenson 2003). The value of this score ranges from 1 (perfect forecast system) to -1 
#' (perfectly bad forecast system). Zero indicates no skill compared with a random prediction. The selected year 
#' is not included in the computation of the score (operational point of view). Negative values
#' are written in red while high positive values are in blue. The threshold to highlight high positive values can be
#' modified with the score.threshold argument.
#'  
#' In case of multi-member fields or stations, they are spatially averaged to obtain one single time series
#' for each member prior to data analysis, with a warning.    
#' 
#' @note The computation of climatological terciles requires a representative period to obtain meaningful results.

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
#' # Tercile bar plot
#' tercileBarplot(hindcast = tas.cfs2, obs = tas.ncep2, forecast = tas.cfs.operative2.2016)
#' }

#' @author M.D. Frias \email{mariadolores.frias@@unican.es} and J. Fernandez
#' 
#' @family VisualizeR
#' 
#' @references
#'  Jolliffe, I. T. and Stephenson, D. B. 2003. Forecast Verification: A Practitioner's Guide in 
#'  Atmospheri Science, Wiley, NY
#'

tercileBarplot <- function(hindcast, obs, forecast=NULL, year.target = NULL, detrend = FALSE, score.threshold=NULL, subtitle = NULL) {
  # Check data dimension from the original data sets
  checkDim(hindcast)
  checkDim(obs)
  if (!is.null(forecast)){
    checkDim(forecast)   
  }      
  yy <- unique(getYearsAsINDEX(hindcast))
  if (is.null(forecast)){
    if (is.null(year.target)){
      year.target <- last(yy)
    }
    if (!year.target %in% yy) {
      stop("Target year outside temporal data range")
    }
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
    if (length(yy.forecast)>1) {
      stop("Select just one year for forecast")
    }
    year.target <- NULL
    if (isS4(forecast)==FALSE){
      forecast <- convertIntoS4(forecast)
    }
  }
  # Detrend
  if (detrend){
    hindcast <- detrend.data(hindcast)
    obs <- detrend.data(obs)
    forecast <- detrend.data(hindcast, forecast)
  }
  # Spatial mean of forecast and Benchmark if necessary
  sp.hindcast <- spatialMean(hindcast)
  sp.obs <- spatialMean(obs)
  sp.forecast <- spatialMean(forecast)
  # Computation of seasonal mean
  sm.hindcast <- seasMean(sp.hindcast)
  sm.obs <- seasMean(sp.obs)
  sm.forecast <- seasMean(sp.forecast)
  # Computation of terciles and exceedance probabilities
  probs.hindcast <- QuantileProbs(sm.hindcast)
  probs.obs <- QuantileProbs(sm.obs)
  probs.forecast <- QuantileProbs(sm.forecast, sm.hindcast)
  cofinogram.data <- t(getData(probs.hindcast)[,,,,])
  obs.terciles <- t(getData(probs.obs)[,,,,])
  obs.t <- getData(probs.obs)[3,,,,]-getData(probs.obs)[1,,,,]
  # Compute ROCSS
  rocss.t.u <- rocss.fun(obs.terciles[,3], cofinogram.data[,3])
  rocss.t.m <- rocss.fun(obs.terciles[,2], cofinogram.data[,2])
  rocss.t.l <- rocss.fun(obs.terciles[,1], cofinogram.data[,1])
  # Threshold to write the score with different colors in the plot
  if (is.null(score.threshold)){
    threshold <- 0.5
  } else{
    threshold <-score.threshold
  }  
  color.score <- function(sval){
    if (sval<0){return("red")}
    else if (sval>=threshold){return("blue")}
    else {return("darkgrey")}
  }
  # Bars for the selected year  
  opar <- par(no.readonly=TRUE)
  par(oma = c(4, 0, 2, 0))
  barplot(getData(probs.forecast)[,,,,], names.arg=c("Below", "Normal", "Above"), col="lightgrey", ylab="Probability of the tercile", ylim=c(0,1))
  abline(h=0.33, col="darkgrey", lwd=4)
  # Add title
  mons.start <- months(as.POSIXlt((getDates(obs)$start)[1]),abbreviate=T)
  mons.end <- months(last(as.POSIXlt(getDates(obs)$end))-1, abbreviate=T)
  title <- sprintf("%s, %s to %s, %d", attr(getVariable(hindcast), "longname"), mons.start, mons.end, yy.forecast)
  mtext(title, side=3, line=2, adj=0, cex=1.2, font=2)
  if (!is.null(subtitle)){
    mtext(subtitle, side=3, line=1, adj=0, cex=0.7)
  }
  # Add skill score values to the plot
  mtext("ROCSS:", side=1, line=3, adj=0, font=2)
  mtext(round(rocss.t.l,2), side=1, line=3, at=0.7, font=2, col=color.score(round(rocss.t.l,2)))
  mtext(round(rocss.t.m,2), side=1, line=3, at=1.9, font=2, col=color.score(round(rocss.t.m,2)))
  mtext(round(rocss.t.u,2), side=1, line=3, at=3.1, font=2, col=color.score(round(rocss.t.u,2)))
  # Add legend
  par(oma = c(0, 0, 0, 10))
  brks <- c(-1,0,threshold,1)
  cbar <- c("red", "darkgrey", "blue")
  image.plot(add = TRUE, legend.only = TRUE, breaks = brks, lab.breaks=brks, col = cbar, zlim=c(-1,1), horizontal=T) 
  par(opar)
}
# End
