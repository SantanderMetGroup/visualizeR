#' @title Tercile plot for visualization of the skill of an ensemble forecast prediction
#' 
#' @description Tercile plot for the visualization of the skill of an ensemble forecast prediction.
#' 
#' @param mm.obj A multi-member S4 object (MrEnsemble class) with predictions, either a field or a multi-member 
#' station object as a result of downscaling of a forecast using station data. See details.
#' @param obs S4 object with the benchmarking observations for forecast verification
#' @param select.year Year within the whole verification period to display the results for.
#' @param detrend Logical indicating if the data should be detrended. Default is TRUE
#' @param color.pal Color palette for the representation of the probabilities. Default to \code{"bw"} (black and white).
#'  \code{"reds"} for a white-red transition or \code{"tcolor"} for a colorbar for each tercile, blue-grey-red
#'  for below, normal and above terciles, respectively.
#'  @param subtitle String to include a subtitle bellow the title. Default is NULL.
#' 
#' @importFrom RColorBrewer brewer.pal
#' @importFrom downscaleR array3Dto2Dmat mat2Dto3Darray 
#' @importFrom abind abind
#' @import fields
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
#' year by year by considering the number of members falling within each category. This probability is represented by the
#' colorbar. For instance, probabilities below 1/3 are very low, indicating that a minority of the members 
#'  falls in the tercile. Conversely, probabilities above 2/3 indicate a high level of member agreement (more than 66\% of members
#'  falling in the same tercile). The observed terciles (the events that actually occurred) are represented by the white circles.
#'  
#'  Finally, the ROC Skill Score (ROCSS) is indicated in the secondary (right) Y axis. For each tercile, it provides a 
#'  quantitative measure of the forecast skill, and it is commonly used to evaluate the performance of probabilistic systems
#'  (Joliffe and Stephenson 2003). The value of this score ranges from 1 (perfect forecast system) to -1 
#'  (perfectly bad forecast system). A value zero indicates no skill compared with a random prediction.
#'  
#'  In case of multi-member fields or stations, they are spatially averaged to obtain one single time series
#'  for each member prior to data analysis, with a warning.   
#' 
#' @note The computation of climatological terciles requires a representative period to obtain meaningful results.
#' 
#' @author M.D. Frias\email{mariadolores.frias@@unican.es} and J. Fernandez based on the original diagram 
#' conceived by A. Cofino (See Diez et al, 2011 for more details).
#' 
#' @family visualizeR
#' 
#' @references
#' Diez, E., Orfila, B., Frias, M.D., Fernandez, J., Cofino, A.S., Gutierrez, J.M., 2011. 
#' Downscaling ECMWF seasonal precipitation forecasts in Europe using the RCA model.
#' Tellus A 63, 757-762. doi:10.1111/j.1600-0870.2011.00523.x
#'    
#' Jolliffe, I. T. and Stephenson, D. B. 2003. Forecast Verification: A Practitioner's Guide in 
#' Atmospheri Science, Wiley, NY
#'  

tercilePlotS4 <- function(mm.obj, obs, select.year, detrend = TRUE, color.pal = c("bw", "reds", "tcolor"), subtitle = NULL){
      color.pal <- match.arg(color.pal, c("bw", "reds", "tcolor"))
      # Check input datasets
      stopifnot(checkEnsemblesObs(mm.obj, obs))
      yrs <- getYearsAsINDEX.S4(mm.obj)
      yy <- unique(yrs)
      if (!select.year %in% yy) {
        stop("Target year outside temporal data range")
      }
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
      i.yy <- !yy == select.year # Remove select.year for the score calculation
      rocss.t.u <- rocss.fun(obs.terciles[i.yy,3], cofinogram.data[i.yy,3])
      rocss.t.m <- rocss.fun(obs.terciles[i.yy,2], cofinogram.data[i.yy,2])
      rocss.t.l <- rocss.fun(obs.terciles[i.yy,1], cofinogram.data[i.yy,1])
      # Color selection
      if (color.pal=="tcolor"){
          t.color <- tercileBrewerColorRamp(10)     
          brks <- c(seq(0,1,length=nrow(t.color)+1))
      } else {
          cbar <- switch(color.pal, 
              "reds" <- c("#fff5f0", "#fff5f0", brewer.pal(8,"Reds")),
              "bw" = rev(grey.colors(10))
         )
         brks <- c(seq(0,1,length=length(cbar)+1))
      }
      opar <- par(no.readonly=TRUE)
      par(mar = c(4, 5, 0, 3)) #change lines of the margins
      par(oma = c(0, 0, 0, 3)) #change size of the outer margins
      if (color.pal=="tcolor"){          
          image(yy, c(-1.5,-0.5), matrix(cofinogram.data[,1]), breaks=brks, col=t.color$low, ylab="", xlab="", asp = 1, yaxt="n", bty = "n", axes = FALSE)      
          image(yy, c(-0.5,0.5), matrix(cofinogram.data[,2]), breaks=brks, col=t.color$middle, ylab="", xlab="", asp = 1, yaxt="n", bty = "n", axes = FALSE, add=TRUE)      
          image(yy, c(0.5,1.5), matrix(cofinogram.data[,3]), breaks=brks, col=t.color$high, ylab="", xlab="", asp = 1, yaxt="n", bty = "n", axes = FALSE, add=TRUE)      
      } else{         
         image(yy, c(-1,0,1), cofinogram.data, breaks=brks, col=cbar, ylab="", xlab="", asp = 1, yaxt="n", bty = "n", axes = FALSE)      
      }         
      axis(1, at = yy, pos=-1.5)      
      points(yy, obs.t, pch = 21, bg = "white")
      axis(2, at=-1:1, labels=c("Below", "Normal", "Above"), las="2")
      # Add skill score values to the plot    
      axis(4, at=c(-1:1,2.3), labels=c(round(rocss.t.l,2), round(rocss.t.m,2), round(rocss.t.u,2), "ROCSS"), las="2", tick = FALSE)
      if (color.pal=="tcolor"){       
          #par(oma = c(7, 0, 2, 5.7))
          image.plot(add = TRUE, horizontal = T, smallplot = c(0.15,0.8,0.3,0.35), legend.only = TRUE, breaks = brks, lab.breaks=c(rep("", length(brks))), col = t.color[,3], zlim=c(0,1))
          #par(oma = c(7, 0, 2, 4.2))
          image.plot(add = TRUE, horizontal = T, smallplot = c(0.15,0.8,0.23,0.28), legend.only = TRUE, breaks = brks, lab.breaks=c(rep("", length(brks))), col = t.color[,2], zlim=c(0,1))
          #par(oma = c(7, 0, 2, 2.7))
          image.plot(add = TRUE, horizontal = T, smallplot = c(0.15,0.8,0.16,0.21), legend.only = TRUE, breaks = brks, col = t.color[,1], zlim=c(0,1), legend.lab="Probability of the tercile")          
      } else{          
          #par(oma = c(5, 0, 2, 3.2))
          #image.plot(add = TRUE, legend.only = TRUE, breaks = brks, col = cbar, smallplot = c(0.96,0.99,0.2,0.8), zlim=c(0,1), legend.lab="Probability of the tercile")            
          image.plot(add = TRUE, horizontal = T, smallplot = c(0.15,0.8,0.25,0.33), legend.only = TRUE, breaks = brks, col = cbar, zlim=c(0,1), legend.lab="Probability of the tercile")            
      }
      mons <- unique(months(as.POSIXlt(as.POSIXlt(getDates(obs)$start)), abbreviate = T))
      title <- sprintf("%s, %s to %s, %d", attr(getVariable(mm.obj), "longname"), mons[1],last(mons), select.year)
      mtext(title, side=3, line=-2, adj=0, cex=1.2, font=2)
      mtext (title)
      if (!is.null(subtitle)){
        mtext(subtitle, side=3, line=-3, adj=0, cex=0.8)
      }
      par(opar)
}
# End
