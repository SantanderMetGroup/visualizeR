#' @title Tercile bar plot for visualization of forecast skill of ensemble predictions.
#' 
#' @description Tercile bar plot for visualization of the skill of ensemble predictions for a particular year.
#'  This function is prepared to plot the data sets loaded from the ECOMS User Data Gateway (ECOMS-UDG). See 
#'  the loadeR.ECOMS R package for more details (http://meteo.unican.es/trac/wiki/udg/ecoms/RPackage).
#'  
#' @param mm.obj A multi-member object with predictions, either a field or a multi-member station object as a result of
#' downscaling of a forecast using station data. See details.
#' @param obs List with the benchmarking observations for forecast verification
#' @param year.target Year within the whole verification period to display the results for. This year is not
#'  included in the computation of the score (operational point of view).
#' @param score.threshold Threshold to remark high positive score values in the figure.
#' @param detrend Logical. Should data be detrended prior to verification? Default to \code{TRUE}.
#' 
#' @importFrom fields image.plot
#' @importFrom transformeR array3Dto2Dmat mat2Dto3Darray 
#' @importFrom graphics barplot
#'   
#' @export
#' 
#' @details 
#'  
#' For each member, the daily predictions are averaged to obtain a single seasonal forecast. For
#' rectangular spatial domains (i.e., for fields), the spatial average is first computed (with a warning) to obtain a
#' unique series for the whole domain. The corresponding terciles for each ensemble member are then computed
#' for the analysis period. Thus, each particular member and season, are categorized into three categories (above, 
#' between or below), according to their respective climatological terciles. Then, a probabilistic forecast is computed 
#' year by year by considering the number of members falling within each category. The probability for the selected year
#' is represented by the bars. The 1/3 probability is plotted by a grey line. For instance, probabilities below this 
#' line are very low, indicating that a minority of the members falls in the tercile. Conversely, probabilities above 2/3 
#' indicate a high level of member agreement (more than 66\% of members falling in the same tercile). 
#'  
#'  Finally, the ROC Skill Score (ROCSS) is indicated at the bottom part of the bar plot for each tercile. It provides a 
#'  quantitative measure of the forecast skill, and it is commonly used to evaluate the performance of probabilistic systems
#'  (Joliffe and Stephenson 2003). The value of this score ranges from 1 (perfect forecast system) to -1 
#'  (perfectly bad forecast system). Zero indicates no skill compared with a random prediction. The target year 
#'  is not included in the computation of the score (operational point of view). Negative values
#'  are written in red while high positive values are in blue. The threshold to highlight high positive values can be
#'  modified with the score.threshold argument.
#'  
#'  In case of multi-member fields or stations, they are spatially averaged to obtain one single time series
#'  for each member prior to data analysis, with a warning.    
#' 
#' @note The computation of climatological terciles requires a representative period to obtain meaningful results.
#' 
#' @author M.D. Frias \email{mariadolores.frias@@unican.es} and J. Fernandez
#' 
#' @family VisualizeR
#' 
#' @references
#'  Jolliffe, I. T. and Stephenson, D. B. 2003. Forecast Verification: A Practitioner's Guide in 
#'  Atmospheri Science, Wiley, NY
#'

tercileBarplot <- function(mm.obj, obs, year.target, detrend = TRUE, score.threshold=NULL) {
  # Check input datasets
  if (isS4(mm.obj)==FALSE){
    mm.obj <- convertIntoS4(mm.obj)
  }
  if (isS4(obs)==FALSE){
    obs <- convertIntoS4(obs)
  }
  stopifnot(checkData(mm.obj, obs))
  yy <- unique(getYearsAsINDEX.S4(mm.obj))
  if (!year.target %in% yy) {
    stop("Target year outside temporal data range")
  }
  iyear <- which(yy == year.target)
  # Detrend
  if (detrend){
    mm.obj <- detrend.forecast(mm.obj)
    obs <- detrend.forecast(obs)
  }
  # Spatial mean of forecast and Benchmark if necessary
  sp.mm.obj <- spatialMean(mm.obj)
  sp.obs <- spatialMean(obs)
  # Computation of seasonal mean
  sm.mm.obj <- seasMean(sp.mm.obj)
  sm.obs <- seasMean(sp.obs)
  # Computation of terciles and exceedance probabilities
  probs.mm.obj <- QuantileProbs(sm.mm.obj)
  probs.obs <- QuantileProbs(sm.obs)
  cofinogram.data <- t(getData(probs.mm.obj)[,,,,])
  obs.terciles <- t(getData(probs.obs)[,,,,])
  obs.t <- getData(probs.obs)[3,,,,]-getData(probs.obs)[1,,,,]
  # Compute ROCSS
  i.yy <- !yy == year.target # Remove year.target for the score calculation
  rocss.t.u <- rocss.fun(obs.terciles[i.yy,3], cofinogram.data[i.yy,3])
  rocss.t.m <- rocss.fun(obs.terciles[i.yy,2], cofinogram.data[i.yy,2])
  rocss.t.l <- rocss.fun(obs.terciles[i.yy,1], cofinogram.data[i.yy,1])
  
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
  mons.start <- unique(months(as.POSIXlt(getDates(obs)$start), abbreviate = T))
  mons.end <- unique(months(as.POSIXlt(getDates(obs)$end), abbreviate = T))
  title <- sprintf("%s, %s to %s, %d", attr(getVariable(mm.obj), "longname"), mons.start[1], last(mons.end), year.target)
  par(oma = c(5, 0, 0, 0))
  barplot(cofinogram.data[iyear,], names.arg=c("Below", "Normal", "Above"), col="lightgrey", ylab="Probability of the tercile", ylim=c(0,1), main=title)
  abline(h=0.33, col="darkgrey", lwd=4)
  # Add skill score values to the plot
  mtext("ROCSS:", side=1, line=3, adj=0, font=2)
  mtext(round(rocss.t.l,2), side=1, line=3, at=0.7, font=2, col=color.score(round(rocss.t.l,2)))
  mtext(round(rocss.t.m,2), side=1, line=3, at=1.9, font=2, col=color.score(round(rocss.t.m,2)))
  mtext(round(rocss.t.u,2), side=1, line=3, at=3.1, font=2, col=color.score(round(rocss.t.u,2)))
  # Add legend
  par(oma = c(1.5, 0, 0, 10))
  brks <- c(-1,0,threshold,1)
  cbar <- c("red", "darkgrey", "blue")
  image.plot(add = TRUE, legend.only = TRUE, breaks = brks, lab.breaks=brks, col = cbar, zlim=c(-1,1), horizontal=T) 
}
# End
